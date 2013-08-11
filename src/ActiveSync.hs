module ActiveSync where

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Status as Status
import Data.Conduit
import Data.ByteString.Char8 ( pack, unpack )
import Control.Applicative
import Control.Exception ( try, SomeException )

import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as L

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Data.CaseInsensitive ( mk )

login :: String -> String -> IO ( Either String Bool )
login username password = runErrorT $ runResourceT $ do
  let hostname = getServerName username
      [ bsUser, bsPass ] = map pack [ username, password ]
  connMan <- liftIO $ HTTP.newManager HTTP.def
  url <- autodiscoverUrl hostname bsUser bsPass connMan
  req <- HTTP.parseUrl url
  let discoverReq = req { HTTP.method = pack "POST",  
                          HTTP.redirectCount = 1,
                          HTTP.secure = True }
      authenticatedRequest = HTTP.applyBasicAuth bsUser bsPass discoverReq
  res <- liftIO $ tryHttpLbs authenticatedRequest connMan
  case res of
    Right response ->
      return $ responseCode response == 200
    Left error ->
      throwError $ submitError url error

submitError url error =
  "failed to submit autodiscover request to " ++ url ++ show error

getServerName username =
  let ( _ : hostname ) = dropWhile (\ c -> c /= '@' ) username in
  hostname
  
autodiscoverUrl hostname user password connManager = do
  let connInfo = (user, password, connManager)
  evalStateT ( runReaderT pickAutodiscoverTarget connInfo ) $ autodiscoverTargets hostname
  
autodiscoverTargets hostname =
  let suffix = "/autodiscover/autodiscover.xml" in
  [ ( "POST", Nothing, "https://autodiscover." ++ hostname ++ suffix ),
    ( "POST", Nothing, "http://" ++ hostname ++ suffix ),
    ( "GET",  Nothing, "http://autodiscover." ++ hostname ++ suffix ) ]

pickAutodiscoverTarget = do
  urls <- get
  case urls of
    [] ->
      throwError "failed to find autodiscover server"
    ( target : rest ) -> do
      put rest
      tryAutodiscoverTarget target
  
tryAutodiscoverTarget ( method, auth, url ) = do
  ( user, password, connMan ) <- ask
  req <- HTTP.parseUrl url
  let discoverReq = req { HTTP.method = pack method, 
                          HTTP.redirectCount = 0,
                          HTTP.checkStatus = \_ _ _ -> Nothing }
  res <- liftIO $ tryHttpLbs ( addAuth auth discoverReq ) connMan
  case res of
    Right httpRes -> 
      let location = getResponseLocation httpRes in
      case (responseCode httpRes, location ) of
        ( 200, _ ) ->
          return url
        ( 302, Just movedTo ) -> do
          targets <- get
          put $ ( "POST", Nothing, movedTo ) : targets
          pickAutodiscoverTarget
        ( 401, _ ) | auth == Nothing -> do
          targets <- get
          put $ ( method, Just (user, password), url ) : targets
          pickAutodiscoverTarget
        _ ->
          pickAutodiscoverTarget
    Left _ ->
      pickAutodiscoverTarget
      
-- FIXME: condiser applicative functors
addAuth ( Just (user, password) ) request =
  HTTP.applyBasicAuth user password request
addAuth Nothing request = 
  request

tryHttpLbs req connMan = do
  try $ runResourceT $ HTTP.httpLbs req connMan :: IO ( Either SomeException (HTTP.Response L.ByteString) )

getResponseLocation response =
  let headerName = mk $ pack "Location" in
  unpack <$> ( lookup headerName $ HTTP.responseHeaders response )
  
responseCode res =
  Status.statusCode $ HTTP.responseStatus res