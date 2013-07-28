module Main where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error
import System.IO

type ImapSrv = StateT ImapState IO
data ImapState = ImapState { getCmds :: [ ImapCmd ],
                             getNextState :: ImapSrv (), 
                             getTag :: String, 
                             imapInput :: [ ImapToken ] }
type ImapCmd = ( String, ImapSrvVoid )
type ImapSrvVoid = ErrorT String ImapSrv ()
data ImapStateName = NOTAUTHENTICATED | AUTHENTICATED | SELECTED | LOGOUT
data ImapTokenType = TypeNL |
                     TypeString |
                     TypeLiteral |
                     TypeUntagged |
                     TypeUnknown
data ImapToken = NL | Untagged | ImapString String | ImapLiteral String
data ParserMode = WordMode

instance Eq ImapTokenType where
  (==) TypeNL TypeNL = True
  (==) TypeString TypeString = True
  (==) TypeLiteral TypeLiteral = True
  (==) TypeUntagged TypeUntagged = True
  (==) _ _ = False

main = do
  content <- imapGetContent
  evalStateT imapServerStart $ ImapState{ getCmds = [],
                                          getNextState = imapServerLoop,
                                          getTag = "", 
                                          imapInput = content }

imapServerStart = do
  putUntagged "YAHUI IMAP server is happy to accept your connection"
  loadCommands NOTAUTHENTICATED
  imapServerLoop

imapServerLoop = do
  tryRunNextCmd
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
  let ( token:rest ) = imapInput state
  case getTokenType token of
    readType | readType == typeNeeded  -> do
      put $ state { imapInput = rest }
      return token
    _ -> throwError "Parse error: wrong token type"

getTokenType NL = TypeNL
getTokenType Untagged = TypeUntagged
getTokenType ( ImapString _ ) = TypeString
getTokenType ( ImapLiteral _ ) = TypeLiteral
      
readToken :: ImapSrv ImapToken
readToken = do
  state <- get
  let ( token:rest ) = imapInput state
  put $ state { imapInput = rest }
  return token

  
swallow = do
  token <- readToken
  case token of
    NL -> return ()
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
imapPutToken NL = imapPutStr "\n"
imapPutToken ( ImapString str ) = imapPutStr str

imapPutStr str = liftIO $ putStr str

loadCommands _ = do
  state <- get
  let cmds = [ cmdLogin, cmdLogout, cmdCapability, cmdNoop ]
  put $ state { getCmds = cmds }
  
cmdLogin = ( "LOGIN", cmdLoginDo )

cmdLoginDo = do
  ( username, password ) <- liftM2 (,) readWord readWord
  answer "Login failed"

cmdLogout = ( "LOGOUT", cmdLogoutDo )
cmdLogoutDo = do
  putUntagged "BYE"
  answerOk
  state <- get
  put $ state { getNextState = imapServerLogout }
  
cmdCapability = ( "CAPABILITY", cmdCapabilityDo )
cmdCapabilityDo = do
  let authenticationMethods = concatMap ( (++) " AUTH=" ) [ "PLAIN" ]
  putUntagged $ "IMAP4rev1" ++ authenticationMethods
  answerOk

cmdNoop = ( "NOOP", cmdNoopDo )
cmdNoopDo = do
  answerOk
  
imapGetContent = fmap (\x -> imapParseTokens x WordMode "") getContents

-- FIXME: tail recursion?
imapParseTokens (' ':rest) WordMode acc =
  token : (imapParseTokens rest WordMode "")
  where token = ImapString $ reverse acc
imapParseTokens ('\n':rest) WordMode acc = 
  let (token:restTokens) = imapParseTokens (' ':rest) WordMode acc in
  token : NL : restTokens
imapParseTokens (c:rest) WordMode acc = imapParseTokens rest WordMode (c:acc)
imapParseTokens [] WordMode acc = [ ImapString $ reverse acc ]
