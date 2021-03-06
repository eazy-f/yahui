module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Error
import Data.Functor ( (<$>) )
import System.IO
import System.Environment ( getArgs )
import Control.Exception
import Control.Concurrent
import GHC.IO ( unsafeInterleaveIO )
import GHC.IO.Exception
import Control.Concurrent.STM

import Network.Socket
import Network.BSD
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS

import Data.ByteString.Char8 ( pack, unpack )
import Data.Char ( isAlphaNum, toUpper )

import qualified ActiveSync as AS
import qualified Network.Connection as C

import qualified System.Log.Logger as L
import System.Log.Handler.Simple ( fileHandler )
import System.Log.Handler ( setFormatter )
import System.Log.Formatter

type ImapSrv = StateT ImapState (ReaderT ImapConfig IO)
data ImapState = ImapState { getCmds :: [ ImapCmd ],
                             getNextState :: ImapSrv (),
                             getTag :: String, 
                             imapInput :: [ ImapToken ],
                             connCtx :: C.ConnectionContext,
                             conn :: C.Connection,
                             getLogChan :: TChan String,
                             stateData :: ImapStateData }

data ImapConfig = ImapConfig { hostname :: String,
                               port :: Integer,
                               sslCertFile :: String,
                               sslKeyFile  :: String }

type ImapCmd = ( String, ImapSrvVoid )
type ImapSrvVoid = ErrorT String ImapSrv ()
data ImapStateName = NOTAUTHENTICATED | AUTHENTICATED | SELECTED | LOGOUT
data ImapTokenType = TypeNL |
                     TypeString |
                     TypeLiteral |
                     TypeUntagged |
                     TypeEOF |
                     TypeUnknown deriving Eq
data ImapToken = NL | Untagged | ImapString String | ImapLiteral String
                    | ImapAtom String | ImapEOF deriving Show

data ImapStateData = AuthenticatedData { asdConnection :: AS.ASConnection, 
                                         asdPassword :: String,
                                         asdUsername :: String } 
                   | EmptyData


main = do
  fileConf <- readFileConfig
  cmdConf <- readCmdConfig -- it is actually a function on config
  let conf = cmdConf fileConf
  runReaderT tcpServerStart conf

readFileConfig = do
  return ImapConfig { hostname = "localhost",
                      port = 8993,
                      sslCertFile = "ca-cert.pem",
                      sslKeyFile = "ca-key.pem" }

readCmdConfig = fmap parseArgs getArgs

parseArgs ("--cert":filename:rest) =
  (parseArgs rest) . \s -> s {sslCertFile = filename}
parseArgs ("--key":filename:rest) =
  (parseArgs rest) . \s -> s {sslKeyFile = filename}
parseArgs ("-p":port:rest) =
  (parseArgs rest) . \s -> s {port = read port}
parseArgs _ = id

tcpServerStart = do
  host <- hostname <$> ask
  port <- port <$> ask
  log <- liftIO $ newTChanIO
  -- FIXME: error handling
  sock <- liftIO $ startServerSocket host port
  liftIO $ forkIO $ logWriter log
  listenLoop sock log

startServerSocket host port = do
  ( info : _ ) <- getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- socket AF_INET Stream 0
  bind sock $ addrAddress info
  let queueLen = 5
  listen sock queueLen
  return sock
  
logWriter logChan = do
  let name = "yahui"
  fileLog <- fileHandler "yahui.log" L.DEBUG
  let formatter = simpleLogFormatter "$time $loggername: <$prio> $msg"
  let fileLogFormatted = setFormatter fileLog formatter
  L.updateGlobalLogger name $ L.setLevel L.DEBUG
  L.updateGlobalLogger name $ L.setHandlers [ fileLogFormatted ]
  logWriterLoop name logChan
  
logWriterLoop logger logChan = do
  msg <- atomically $ readTChan logChan
  L.debugM logger msg
  logWriterLoop logger logChan
  
listenLoop sock log = do
  conf <- ask
  liftIO $ acceptClient sock log conf
  listenLoop sock log

acceptClient sock log conf = do
  ( client, _ ) <- accept sock
  handle <- socketToHandle client ReadWriteMode
  forkIO $ imapHandleClient handle conf log `finally` hClose handle

  
imapHandleClient connHandle conf logChan = do
  connCtx <- C.initConnectionContext
  let fakeConnParams = C.ConnectionParams {
        C.connectionHostname = "localhost",
        C.connectionPort = 666, -- thunderbird looks here
        C.connectionUseSecure = Nothing,
        C.connectionUseSocks = Nothing
  }
  connection <- C.connectFromHandle connCtx connHandle fakeConnParams
  content <- imapGetContent connection
  let initState = ImapState{ getCmds = [],
                             getNextState = imapServerLoop,
                             getTag = "*", 
                             imapInput = content, 
                             stateData = EmptyData, 
                             conn = connection,
                             connCtx = connCtx,
                             getLogChan = logChan }
  runReaderT (evalStateT imapServerStart initState) conf
  
imapServerStart = do
  logInfo "start"
  putUntagged "OK YAHUI IMAP server is happy to accept your connection"
  loadCommands NOTAUTHENTICATED
  imapServerLoop
  
logInfo msg = logMsg msg

logMsg msg = do
  state <- get
  let logChan = getLogChan state
  liftIO $ atomically $ writeTChan logChan msg

imapServerLoop = do
  tryRunNextCmd
  setTag "*"
  currentState <- get
  getNextState currentState
  
imapServerLogout = return ()

tryRunNextCmd = do
  result <- runErrorT runNextCmd
  case result of
    Right res  -> return res
    Left error -> reportCmdError error
    
reportCmdError error = do
  answer $ "BAD " ++ error
  swallow


-- FIXME: ugly
runNextCmd :: ErrorT String ImapSrv ()
runNextCmd = do
  cmd <- readCmd
  runCmd cmd
  readNewLine
  return ()

  
readCmd = do
  [tag, cmd] <- replicateM 2 readAtom
  setTag tag
  return $ map toUpper cmd
  
--readWord :: ImapSrv String
readAtom  = do
  ImapAtom atom <- tryReadToken TypeString
  return atom

readWord = do
  ImapString word <- tryReadToken TypeString
  return word
  
readNewLine = tryReadToken TypeNL
  
--tryReadToken :: ImapTokenType -> ImapSrv ImapToken
tryReadToken typeNeeded = do
  state <- get
  let ( token : rest ) = imapInput state
  logInfo $ show token
  doLogoutOnEof token
  case getTokenType token of
    actualType | actualType == typeNeeded  -> do
      put $ state { imapInput = rest }
      return token
    _ ->
      throwError "Parse error: wrong token type"

getTokenType NL = TypeNL
getTokenType Untagged = TypeUntagged
getTokenType ( ImapAtom _ ) = TypeString
getTokenType ( ImapString _ ) = TypeString
getTokenType ( ImapLiteral _ ) = TypeLiteral
getTokenType ImapEOF = TypeEOF
      
readToken :: ImapSrv ImapToken
readToken = do
  state <- get
  let ( token:rest ) = imapInput state
  doLogoutOnEof token
  put $ state { imapInput = rest }
  return token
  
doLogoutOnEof ImapEOF = do
  state <- get
  put $ state { getNextState = imapServerLogout }

doLogoutOnEof _ = return ()


  
swallow = do
  token <- readToken
  case token of
    NL      -> return ()
    ImapEOF -> return ()
    _  -> swallow
    
setTag tag = state $ \ initState -> ( (), initState { getTag = tag } )
  
runCmd :: String -> ImapSrvVoid
runCmd cmdName = do
  state <- get
  let cmd = lookup cmdName $ getCmds state
  case cmd of
       Just implementation -> implementation
       Nothing -> throwError $ "Command " ++ cmdName ++ " is unknown"

answerOk = answer "OK"
answerOkMsg msg = answer $ "OK" ++ " " ++ msg
  
answer msg = do
  state <- get
  let tag = getTag state
  imapPutTokens $ ( concatMap imapToTokens [ tag, msg ] ) ++ [ NL ]

putUntagged msg = imapPutTokens $ ( Untagged : imapToTokens msg ) ++ [ NL ]

imapToTokens msg = map ImapAtom $ words msg

imapPutTokens ( tokens@[_, NL] ) = do
  mapM_ imapPutToken tokens
imapPutTokens ( [ nl@NL ] ) = do
  imapPutToken nl
imapPutTokens ( token:tokens ) = do
  imapPutToken token
  imapPutStr " "
  imapPutTokens tokens
imapPutTokens [] = return ()

imapPutToken Untagged = imapPutStr "*"
imapPutToken NL = imapPutStr "\r\n"
imapPutToken ( ImapString str ) = imapPutStr $ "\"" ++ str ++ "\""
imapPutToken ( ImapAtom str ) = imapPutStr str

imapPutStr str = do
  state <- get
  let output = conn state
  liftIO $ C.connectionPut output $ pack str

loadCommands AUTHENTICATED =
  putCmds [ cmdLogout, cmdCapability, cmdList, cmdNoop ]

loadCommands _ =
  putCmds [ cmdLogin, cmdLogout, cmdCapability, cmdStartTls, cmdNoop ]

putCmds cmds = do
  state <- get
  put $ state { getCmds = cmds }
  
cmdLogin = ( "LOGIN", cmdLoginDo )

cmdLoginDo = do
  ( username, password ) <- liftM2 (,) readWord readWord
  loginRes <- liftCatchedIO $ AS.login username password
  case loginRes of
    Right connection -> do
      answerOk
      -- dirty workaround
      lift $ switchAuthenticated connection username password
    Left  e    -> throwError e
    
cmdStartTls = ( "STARTTLS", cmdStartTlsDo )

cmdStartTlsDo = do
  ImapState { connCtx = ctx, conn = conn } <- get
  tlsParams <- serverTlsParams
  answerOkMsg "begin TLS negotiation now"
  liftIO $ C.connectionSetSecure ctx conn tlsParams
  
serverTlsParams = do
  certFile <- sslCertFile <$> ask
  keyFile  <- sslKeyFile <$> ask
  (cert, key) <- liftIO $ readSslKey certFile keyFile
  let params = TLS.defaultParamsServer {
          TLS.pCiphers = TLS.ciphersuite_all,
          TLS.pCertificates = [ ( cert , Just key ) ]
        }
  return $ C.TLSSettings params

readSslKey certFile keyFile = do
  cert <- TLS.fileReadCertificate certFile
  key  <- TLS.fileReadPrivateKey  keyFile
  return (cert, key)
    
switchAuthenticated conn username password = do
  state <- get
  let authData = AuthenticatedData { asdConnection = conn,
                                     asdUsername = username,
                                     asdPassword = password }
  put $ state { stateData = authData }
  loadCommands AUTHENTICATED

liftCatchedIO op = do
  res <- liftIO $ ( try op :: IO ( Either SomeException ( Either String AS.ASConnection ) ) )
  case res of
    Left e -> throwError $ show e
    Right success -> return success

cmdLogout = ( "LOGOUT", cmdLogoutDo )
cmdLogoutDo = do
  putUntagged "BYE"
  answerOk
  putNextState imapServerLogout

putNextState nextState = do
  state <- get
  put $ state { getNextState = nextState }
  
cmdCapability = ( "CAPABILITY", cmdCapabilityDo )
cmdCapabilityDo = do
  let authenticationMethods = concatMap ( (++) " AUTH=" ) [ "PLAIN" ]
  putUntagged $ "CAPABILITY IMAP4 IMAP4rev1 STARTTLS IDLE LITERAL+ " ++ authenticationMethods
  answerOkMsg "CAPABILITY completed"
  
-- FIXME: add sound implementation
cmdLsub = ( "LSUB", cmdLsubDo )
cmdLsubDo = do
  [ _refName, _mailbox ] <- replicateM 2 readWord
  answerOk

cmdList = ( "LIST", do
               [ refName, mailbox ] <- replicateM 2 readWord
               answerOk
          )
               

cmdNoop = ( "NOOP", cmdNoopDo )
cmdNoopDo = do
  answerOk
  
imapGetContent conn = do
  chars <- connectionGetContents conn
  return $ imapParseTokens (concat $ map unpack chars)

-- FIXME: find a better way than this recursion
connectionGetContents conn = do
  char <- unsafeInterleaveIO $ C.connectionGet conn 1
  rest <- unsafeInterleaveIO $ connectionGetContents conn
  return ( char : rest )
   


-- FIXME: tail recursion?
imapParseTokens (' ':input) = imapParseTokens input
imapParseTokens [] = [ ImapEOF ]
imapParseTokens input = token : ( imapParseTokens rest )
                        where                          
                          ( token, rest ) = imapParseToken input
                          
-- FIXME: better error handling
imapParseToken ( '"' : input ) =
  let ( string, ( _dquote : rest ) ) = span ( (/=) '"' ) input in
  ( ImapString string, rest )
imapParseToken ('\r':'\n':rest) = 
  ( NL, rest )
-- hardcode for single CR from mutt
imapParseToken ('\r':rest) = 
  ( NL, rest )
imapParseToken input@( c : _ ) | isAtomChar c =
  let ( atom, rest ) = span isAtomChar input in
  ( ImapAtom atom, rest )

isAtomChar c = ( isAlphaNum c ) || ( not $ c `elem` "(){ %*\"\\]\r\n" )
