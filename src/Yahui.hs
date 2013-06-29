module Main where

import Control.Monad.State
import System.IO

type ImapSrv = StateT ImapState IO
data ImapState = ImapState { getCmds :: [ ImapCmd ],
                             getNextState :: ImapSrv (), 
                             getTag :: String }
type ImapCmd = ( String, ImapSrvVoid )
type ImapSrvVoid = ImapSrv ()
data ImapStateName = NOTAUTHENTICATED | AUTHENTICATED | SELECTED | LOGOUT

main = evalStateT imapServerStart $ ImapState{ getCmds = [],
                                               getNextState = imapServerLoop,
                                               getTag = "" }

imapServerStart = do
  putUntagged "YAHUI IMAP server is happy to accept your connection"
  loadCommands NOTAUTHENTICATED
  imapServerLoop

imapServerLoop = do
  cmd <- readCmd
  runCmd cmd
  currentState <- get
  getNextState currentState
  
imapServerLogout = do
  putUntagged "BYE"
  
readCmd = do
  (tag, cmd) <- liftIO $ liftM2 (,) readWord readWord
  setTag tag
  return cmd
  
readWord =
  readWord2 []
  
readWord2 acc = do
  char <- getChar
  case char of space | space == ' ' || space == '\n' -> return $ reverse acc
               c   -> readWord2 (c:acc)
               
setTag tag = state $ \ initState -> ( (), initState { getTag = tag } )
  
runCmd :: String -> ImapSrvVoid
runCmd cmdName = do
  state <- get
  let cmd = lookup cmdName $ getCmds state
  case cmd of
       Just implementation -> implementation
       Nothing -> answer "Command is unknown"

  
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
  let cmds = [ cmdLogin, cmdQuit ]
  put $ state { getCmds = cmds }
  
cmdLogin = ( "LOGIN", cmdLoginDo )

cmdLoginDo = do
  ( username, password ) <- liftIO $ liftM2 (,) readWord readWord
  answer "Login failed"

cmdQuit = ( "QUIT", cmdQuitDo )
cmdQuitDo = do
  state <- get
  put $ state { getNextState = imapServerLogout }  
  
