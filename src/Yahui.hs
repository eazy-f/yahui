module Main where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error
import System.IO
import Control.Exception
import Control.Concurrent
import GHC.IO.Exception
import Control.Concurrent.STM

import Network.Socket
import Network.BSD

import qualified ActiveSync as AS

type ImapSrv = StateT ImapState IO
data ImapState = ImapState { getCmds :: [ ImapCmd ],
                             getNextState :: ImapSrv (), 
                             getTag :: String, 
                             imapInput :: [ ImapToken ], 
                             conn :: Handle,
                             getLogChan :: TChan String,
                             stateData :: ImapStateData }
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
                    | ImapEOF
data ParserMode = WordMode

data ImapStateData = AuthenticatedData { asdUrl :: String, 
                                         asdPassword :: String,
                                         asdUsername :: String } 
                   | EmptyData


main = tcpServerStart "localhost" "8993"

tcpServerStart host port = do
  log <- newTChanIO
  -- FIXME: error handling
  ( info : _ ) <- getAddrInfo Nothing (Just host) (Just port)
  sock <- socket AF_INET Stream 0
  bind sock $ addrAddress info
  let queueLen = 5
  listen sock queueLen
  forkIO $ logWriter log
  listenLoop sock log
  
logWriter logChan = do
  msg <- atomically $ readTChan logChan
  putStr $ msg ++ "\n"
  logWriter logChan
  
listenLoop sock log = do
  ( client, _ ) <- accept sock
  handle <- socketToHandle client ReadWriteMode
  forkIO $ imapHandleClient handle log `finally` hClose handle
  listenLoop sock log
  
  
imapHandleClient connHandle logChan = do
  content <- imapGetContent connHandle
  let initState = ImapState{ getCmds = [],
                             getNextState = imapServerLoop,
                             getTag = "*", 
                             imapInput = content, 
                             stateData = EmptyData, 
                             conn = connHandle,
                             getLogChan = logChan }
  evalStateT imapServerStart initState
imapServerStart = do
  logInfo "start"
  putUntagged "YAHUI IMAP server is happy to accept your connection"
  loadCommands NOTAUTHENTICATED
  imapServerLoop
  
logInfo msg = logMsg $ "[INFO] " ++ msg

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
  [tag, cmd] <- replicateM 2 readWord
  setTag tag
  return cmd
  
--readWord :: ImapSrv String
readWord = do
  ImapString word <- tryReadToken TypeString
  return word
  
readNewLine = tryReadToken TypeNL
  
--tryReadToken :: ImapTokenType -> ImapSrv ImapToken
tryReadToken typeNeeded = do
  state <- get
  let ( token : rest ) = imapInput state
  doLogoutOnEof token
  case getTokenType token of
    actualType | actualType == typeNeeded  -> do
      put $ state { imapInput = rest }
      return token
    _ ->
      throwError "Parse error: wrong token type"

getTokenType NL = TypeNL
getTokenType Untagged = TypeUntagged
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
  
answer msg = do
  state <- get
  let tag = getTag state
  imapPutTokens $ ( concatMap imapToTokens [ tag, msg ] ) ++ [ NL ]

putUntagged msg = imapPutTokens $ ( Untagged : imapToTokens msg ) ++ [ NL ]

imapToTokens msg = map ImapString $ words msg

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
imapPutToken ( ImapString str ) = imapPutStr str

imapPutStr str = do
  state <- get
  let output = conn state
  liftIO $ hPutStr output str

loadCommands AUTHENTICATED =
  putCmds [ cmdLogout, cmdCapability, cmdNoop ]

loadCommands _ =
  putCmds [ cmdLogin, cmdLogout, cmdCapability, cmdNoop ]

putCmds cmds = do
  state <- get
  put $ state { getCmds = cmds }
  
cmdLogin = ( "LOGIN", cmdLoginDo )

cmdLoginDo = do
  ( username, password ) <- liftM2 (,) readWord readWord
  loginRes <- liftCatchedIO $ AS.login username password
  case loginRes of
    Right url -> do
      answerOk
      -- dirty workaround
      lift $ switchAuthenticated url username password
    Left  e    -> throwError e
    
switchAuthenticated url username password = do
  state <- get
  let authData = AuthenticatedData { asdUrl = url, asdUsername = username, 
                                     asdPassword = password }
  put $ state { stateData = authData }
  loadCommands AUTHENTICATED

liftCatchedIO op = do
  res <- liftIO $ ( try op :: IO ( Either SomeException ( Either String String ) ) )
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
  putUntagged $ "IMAP4rev1" ++ authenticationMethods
  answerOk

cmdNoop = ( "NOOP", cmdNoopDo )
cmdNoopDo = do
  answerOk
  
imapGetContent input =
  fmap (\x -> imapParseTokens x WordMode "") $ hGetContents input

-- FIXME: tail recursion?
imapParseTokens (' ':rest) WordMode acc =
  token : (imapParseTokens rest WordMode "")
  where token = ImapString $ reverse acc
imapParseTokens ('\r':'\n':rest) WordMode acc = 
  let (token:restTokens) = imapParseTokens (' ':rest) WordMode acc in
  token : NL : restTokens
imapParseTokens (c:rest) WordMode acc = imapParseTokens rest WordMode (c:acc)
imapParseTokens [] WordMode acc = [ ImapString ( reverse acc ), ImapEOF ]

