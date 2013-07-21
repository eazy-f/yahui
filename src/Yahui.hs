module Main where

import Control.Monad.State
import System.IO

type ImapSrv = StateT ImapState IO
data ImapState = ImapState { getCmds :: [ ImapCmd ],
                             getNextState :: ImapSrv (), 
                             getTag :: String, 
                             imapInput :: [ ImapToken ] }
type ImapCmd = ( String, ImapSrvVoid )
type ImapSrvVoid = ImapSrv ()
data ImapStateName = NOTAUTHENTICATED | AUTHENTICATED | SELECTED | LOGOUT
data ImapToken = NL | Word String | LongString String 
data ParserMode = Literal

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
  cmd <- readCmd
  runCmd cmd
  swallow
  currentState <- get
  getNextState currentState
  
imapServerLogout = do
  putUntagged "BYE"
  
readCmd = do
  (tag, cmd) <- liftM2 (,) readWord readWord
  setTag tag
  return cmd
  
readWord :: ImapSrv String
readWord = do
  Word word <- readToken
  return word
  
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
       Nothing -> answer $ "Command " ++ cmdName ++ " is unknown"

answerOk = answer "OK"
  
answer msg = do
  state <- get
  let tag = getTag state
  imapPutTokens [ tag, msg ]

putUntagged msg = imapPutTokens [ "*", msg ]

imapPutTokens ( msg:rest@(_:_) ) = do
  mapM imapPutToken [ msg, " " ]
  imapPutTokens rest
imapPutTokens ( msg:rest ) = do 
  imapPutToken msg
  imapPutTokens rest
imapPutTokens [] = do
  imapPutToken "\n"
  return ()

imapPutToken msg = liftIO $ putStr msg

loadCommands _ = do
  state <- get
  let cmds = [ cmdLogin, cmdLogout, cmdCapability ]
  put $ state { getCmds = cmds }
  
cmdLogin = ( "LOGIN", cmdLoginDo )

cmdLoginDo = do
  NL <- readToken
  ( username, password ) <- liftM2 (,) readWord readWord
  answer "Login failed"

cmdLogout = ( "LOGOUT", cmdLogoutDo )
cmdLogoutDo = do
  state <- get
  put $ state { getNextState = imapServerLogout }
  
cmdCapability = ( "CAPABILITY", cmdCapabilityDo )
cmdCapabilityDo = do
  mapM_ putUntagged [ "IMAP4rev1", "AUTH=basic" ]
  answerOk

  
imapGetContent = fmap (\x -> imapParseTokens x Literal "") getContents

-- FIXME: tail recursion?
imapParseTokens (' ':rest) Literal acc =
  token : (imapParseTokens rest Literal "")
  where token = Word $ reverse acc
imapParseTokens ('\n':rest) Literal acc = 
  let (token:restTokens) = imapParseTokens (' ':rest) Literal acc in
  token : NL : restTokens
imapParseTokens (c:rest) Literal acc = imapParseTokens rest Literal (c:acc)
