module ActiveSync where

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Header as HType
import Data.Conduit
import Data.ByteString.Char8 ( pack, unpack )
import Control.Applicative
import Control.Exception ( try, SomeException )

import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Data.CaseInsensitive ( mk )

import qualified Text.XML.Light as X

import qualified System.Log.Logger as Log

data ASConnection = ASConnection { connCredentials :: (String, String),
                                   connUrl         :: String,
                                   connMgr         :: HTTP.Manager }

login :: String -> String -> IO ( Either String ASConnection )
login username password = runErrorT $ runResourceT $ do
  let hostname = getServerName username
      [ bsUser, bsPass ] = map pack [ username, password ]
  connMan <- liftIO $ HTTP.newManager HTTP.def
  adUrl <- autodiscoverUrl hostname bsUser bsPass connMan
  req <- HTTP.parseUrl adUrl
  let body = HTTP.RequestBodyBS $ pack $ autodiscoverReq username
      xmlContent = ( HType.hContentType, pack "text/xml" )
      discoverReq = req { HTTP.method = pack "POST",  
                          HTTP.redirectCount = 1,
                          HTTP.secure = True,
                          HTTP.requestHeaders = [ xmlContent ],
                          HTTP.requestBody = body }
      authenticatedRequest = HTTP.applyBasicAuth bsUser bsPass discoverReq
  res <- liftIO $ tryHttpLbs authenticatedRequest connMan
  liftIO $ logHttpResponse res
  case res of
    Right response | responseCode response == 200 &&
                     mobileSyncUrl response /= Nothing -> do
      let Just svcUrl = mobileSyncUrl response
      return $ ASConnection { connCredentials = (username, password),
                              connUrl         = svcUrl,
                              connMgr         = connMan }
    Right _  ->
      throwError "service is not available"
    Left error ->
      throwError $ submitError adUrl error

list :: ASConnection -> IO ( ASConnection, [ String ] )
list conn = asRequest conn asListRequest

asRequest conn request = do
  let url              = connUrl conn
      (user, password) = connCredentials conn
      [ bsUser, bsPass ] = map pack [ user, password ]
      mgr              = connMgr conn
  req <- HTTP.parseUrl url
  let body = HTTP.RequestBodyBS $ wbxmlPack $ request
      xmlContent = ( HType.hContentType, pack "text/wbxml" )
      httpReq = req { HTTP.method = pack "POST",
                          HTTP.redirectCount = 0,
                          HTTP.secure = True,
                          HTTP.requestHeaders = [ xmlContent ],
                          HTTP.requestBody = body }
      authenticatedRequest = HTTP.applyBasicAuth bsUser bsPass httpReq
  res <- liftIO $ tryHttpLbs authenticatedRequest mgr
  liftIO $ logHttpResponse res
  case res of
    Right response | responseCode response == 200 -> do
      return $ ( conn, [ "something" ] )

asListRequest = "hallo"

wbxmlPack = pack  

logHttpResponse =
  either ignore ( ( Log.debugM "yahui.msas" ) . L8.unpack . HTTP.responseBody )
  where
    ignore = (\_ -> return () )

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
  
autodiscoverReq email = X.showElement root where
  root = X.node ( X.unqual "Autodiscover" ) ( [ nsAttr ], [ request ] )
  request = X.node ( X.unqual "Request" ) [ mailAddr, responseSchema ]
  mailAddr = strNode "EMailAddress" email
  responseSchema = strNode "AcceptableResponseSchema" resNs
  nsAttr = X.Attr { X.attrKey = X.unqual "xmlns", X.attrVal = reqNs }
  reqNs = "http://schemas.microsoft.com/exchange/autodiscover/mobilesync/requestschema/2006"
  resNs = "http://schemas.microsoft.com/exchange/autodiscover/mobilesync/responseschema/2006"
  
responseNs =
  "http://schemas.microsoft.com/exchange/autodiscover/responseschema/2006"
  
  
strNode name value =
  X.node ( X.unqual name ) ( X.CData { X.cdVerbatim = X.CDataText,
                                       X.cdData = value, 
                                       X.cdLine = Nothing } )

mobileSyncUrl response =
  let body  = HTTP.responseBody response
      path  =  [ "Autodiscover", "Response", "Action",
                 "Settings", "Server", "Url" ] in
  xmlFindPath body path

xmlFindPath xml path = do
  root <- X.parseXMLDoc xml
  let filter = X.filterElementName . (\ name elName -> X.qName elName == name )
  result <- foldl (>>=) ( Just root ) $ map filter path
  return $ X.strContent result
