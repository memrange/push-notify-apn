-- |
-- Module: APN
-- Copyright: (C) 2017, memrange UG
-- License: BSD3
-- Maintainer: Hans-Christian Esperer <hc@memrange.io>
-- Stability: experimental
-- Portability: portable
--
-- Send push notifications using Apple's HTTP2 APN API
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NumericUnderscores     #-}

module Network.PushNotify.APN
    ( newSession
    , newMessage
    , newMessageWithCustomPayload
    , hexEncodedToken
    , rawToken
    , sendMessage
    , sendSilentMessage
    , sendRawMessage
    , alertMessage
    , bodyMessage
    , emptyMessage
    , setAlertMessage
    , setMessageBody
    , setBadge
    , setCategory
    , setSound
    , clearAlertMessage
    , clearBadge
    , clearCategory
    , clearSound
    , addSupplementalField
    , closeSession
    , isOpen
    , ApnSession
    , JsonAps
    , JsonApsAlert
    , JsonApsMessage
    , ApnMessageResult(..)
    , ApnFatalError(..)
    , ApnTemporaryError(..)
    , ApnToken(..)
    ) where

import           Control.Concurrent
import           Control.Concurrent.QSem
import           Control.Exception.Lifted (Exception, try, bracket_, throw, throwIO)
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                      (ByteString)
import           Data.Char                            (toLower)
import           Data.Default                         (def)
import           Data.Either
import           Data.Int
import           Data.IORef
import           Data.Map.Strict                      (Map)
import           Data.Maybe
import           Data.Pool
import           Data.Semigroup                       ((<>))
import           Data.Text                            (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Typeable                        (Typeable)
import           Data.X509
import           Data.X509.CertificateStore
import           GHC.Generics
import           Network.HTTP2                        (ErrorCodeId,
                                                       toErrorCodeId)
import           Network.HTTP2.Client
import           Network.HTTP2.Client.FrameConnection
import           Network.HTTP2.Client.Helpers
import           Network.TLS                          hiding (sendData)
import           Network.TLS.Extra.Cipher
import           System.IO.Error
import           System.Mem.Weak
import           System.Random
import           System.Timeout (timeout)
import           System.X509

import qualified Data.ByteString                      as S
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Lazy                 as L
import qualified Data.List                            as DL
import qualified Data.Map.Strict                      as M
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE

import qualified Network.HPACK                        as HTTP2
import qualified Network.HTTP2                        as HTTP2

-- | A session that manages connections to Apple's push notification service
data ApnSession = ApnSession
    { apnSessionPool :: !(Pool ApnConnection)
    , apnSessionOpen :: !(IORef Bool)
    }

-- | Information about an APN connection
data ApnConnectionInfo = ApnConnectionInfo
    { aciCertPath             :: !(Maybe FilePath)
    , aciCertKey              :: !(Maybe FilePath)
    , aciCaPath               :: !(Maybe FilePath)
    , aciHostname             :: !Text
    , aciMaxConcurrentStreams :: !Int
    , aciTopic                :: !ByteString
    , aciUseJWT               :: !Bool }

-- | A connection to an APN API server
data ApnConnection = ApnConnection
    { apnConnectionConnection        :: !Http2Client
    , apnConnectionInfo              :: !ApnConnectionInfo
    , apnConnectionWorkerPool        :: !QSem
    , apnConnectionFlowControlWorker :: !ThreadId
    , apnConnectionOpen              :: !(IORef Bool)}

-- | An APN token used to uniquely identify a device
newtype ApnToken = ApnToken { unApnToken :: ByteString }

class SpecifyError a where
    isAnError :: IOError -> a

-- | Create a token from a raw bytestring
rawToken
    :: ByteString
    -- ^ The bytestring that uniquely identifies a device (APN token)
    -> ApnToken
    -- ^ The resulting token
rawToken = ApnToken . B16.encode

-- | Create a token from a hex encoded text
hexEncodedToken
    :: Text
    -- ^ The base16 (hex) encoded unique identifier for a device (APN token)
    -> ApnToken
    -- ^ The resulting token
hexEncodedToken = ApnToken . B16.encode . fst . B16.decode . TE.encodeUtf8

-- | Exceptional responses to a send request
data ApnException = ApnExceptionHTTP ErrorCodeId
                  | ApnExceptionJSON String
                  | ApnExceptionMissingHeader HTTP2.HeaderName
                  | ApnExceptionUnexpectedResponse
    deriving (Show, Typeable)

instance Exception ApnException

-- | The result of a send request
data ApnMessageResult = ApnMessageResultOk
                      | ApnMessageResultBackoff
                      | ApnMessageResultFatalError ApnFatalError
                      | ApnMessageResultTemporaryError ApnTemporaryError
                      | ApnMessageResultIOError IOError
                      | ApnMessageResultClientError ClientError
    deriving (Eq, Show)

-- | The specification of a push notification's message body
data JsonApsAlert = JsonApsAlert
    { jaaTitle :: !(Maybe Text)
    -- ^ A short string describing the purpose of the notification.
    , jaaBody  :: !Text
    -- ^ The text of the alert message.
    } deriving (Generic, Show)

instance ToJSON JsonApsAlert where
    toJSON     = genericToJSON     defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , omitNothingFields  = True
        }

instance FromJSON JsonApsAlert where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , omitNothingFields  = True
        }

-- | Push notification message's content
data JsonApsMessage
    -- | Push notification message's content
    = JsonApsMessage
    { jamAlert    :: !(Maybe JsonApsAlert)
    -- ^ A text to display in the notification
    , jamBadge    :: !(Maybe Int)
    -- ^ A number to display next to the app's icon. If set to (Just 0), the number is removed.
    , jamSound    :: !(Maybe Text)
    -- ^ A sound to play, that's located in the Library/Sounds directory of the app
    -- This should be the name of a sound file in the application's main bundle, or
    -- in the Library/Sounds directory of the app.
    , jamCategory :: !(Maybe Text)
    -- ^ The category of the notification. Must be registered by the app beforehand.
    } deriving (Generic, Show)

-- | Create an empty apn message
emptyMessage :: JsonApsMessage
emptyMessage = JsonApsMessage Nothing Nothing Nothing Nothing

-- | Set a sound for an APN message
setSound
    :: Text
    -- ^ The sound to use (either "default" or something in the application's bundle)
    -> JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
setSound s a = a { jamSound = Just s }

-- | Clear the sound for an APN message
clearSound
    :: JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
clearSound a = a { jamSound = Nothing }

-- | Set the category part of an APN message
setCategory
    :: Text
    -- ^ The category to set
    -> JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
setCategory c a = a { jamCategory = Just c }

-- | Clear the category part of an APN message
clearCategory
    :: JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
clearCategory a = a { jamCategory = Nothing }

-- | Set the badge part of an APN message
setBadge
    :: Int
    -- ^ The badge number to set. The badge number is displayed next to your app's icon. Set to 0 to remove the badge number.
    -> JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
setBadge i a = a { jamBadge = Just i }

-- | Clear the badge part of an APN message
clearBadge
    :: JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
clearBadge a = a { jamBadge = Nothing }

-- | Create a new APN message with an alert part
alertMessage
    :: Text
    -- ^ The title of the message
    -> Text
    -- ^ The body of the message
    -> JsonApsMessage
    -- ^ The modified message
alertMessage title text = setAlertMessage title text emptyMessage

-- | Create a new APN message with a body and no title
bodyMessage
    :: Text
    -- ^ The body of the message
    -> JsonApsMessage
    -- ^ The modified message
bodyMessage text = setMessageBody text emptyMessage

-- | Set the alert part of an APN message
setAlertMessage
    :: Text
    -- ^ The title of the message
    -> Text
    -- ^ The body of the message
    -> JsonApsMessage
    -- ^ The message to alter
    -> JsonApsMessage
    -- ^ The modified message
setAlertMessage title text a = a { jamAlert = Just jam }
  where
    jam = JsonApsAlert (Just title) text

-- | Set the body of an APN message without affecting the title
setMessageBody
    :: Text
    -- ^ The body of the message
    -> JsonApsMessage
    -- ^ The message to alter
    -> JsonApsMessage
    -- ^ The modified message
setMessageBody text a = a { jamAlert = Just newJaa }
  where
    newJaa = case jamAlert a of
                Nothing  -> JsonApsAlert Nothing text
                Just jaa -> jaa { jaaBody = text }

-- | Remove the alert part of an APN message
clearAlertMessage
    :: JsonApsMessage
    -- ^ The message to modify
    -> JsonApsMessage
    -- ^ The modified message
clearAlertMessage a = a { jamAlert = Nothing }

instance ToJSON JsonApsMessage where
    toJSON     = genericToJSON     defaultOptions
        { fieldLabelModifier = drop 3 . map toLower }

instance FromJSON JsonApsMessage where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 3 . map toLower }

-- | A push notification message
data JsonAps
    -- | A push notification message
    = JsonAps
    { jaAps                :: !JsonApsMessage
    -- ^ The main content of the message
    , jaAppSpecificContent :: !(Maybe Text)
    -- ^ Extra information to be used by the receiving app
    , jaSupplementalFields :: !(Map Text Value)
    -- ^ Additional fields to be used by the receiving app
    } deriving (Generic, Show)

instance FromJSON JsonAps where
    parseJSON = withObject "JsonAps" $ \o ->
      JsonAps <$> o .: "aps"
        <*> o .:? "appspecificcontent"
        <*> o .:  "data"

instance ToJSON JsonAps where
    toJSON JsonAps{..} = object (staticFields <> dynamicFields)
        where
            dynamicFields = [ "data" .= jaSupplementalFields ]
            staticFields = [ "aps" .= jaAps
                           , "appspecificcontent" .= jaAppSpecificContent
                           ]

-- | Prepare a new apn message consisting of a
-- standard message without a custom payload
newMessage
    :: JsonApsMessage
    -- ^ The standard message to include
    -> JsonAps
    -- ^ The resulting APN message
newMessage aps = JsonAps aps Nothing M.empty

-- | Prepare a new apn message consisting of a
-- standard message and a custom payload
newMessageWithCustomPayload
    :: JsonApsMessage
    -- ^ The message
    -> Text
    -- ^ The custom payload
    -> JsonAps
    -- ^ The resulting APN message
newMessageWithCustomPayload message payload =
    JsonAps message (Just payload) M.empty

-- | Add a supplemental field to be sent over with the notification
--
-- NB: The field 'aps' must not be modified; attempting to do so will
-- cause a crash.
addSupplementalField :: ToJSON record =>
       Text
    -- ^ The field name
    -> record
    -- ^ The value
    -> JsonAps
    -- ^ The APN message to modify
    -> JsonAps
    -- ^ The resulting APN message
addSupplementalField "aps"     _          _      = error "The 'aps' field may not be overwritten by user code"
addSupplementalField fieldName fieldValue oldAPN = oldAPN { jaSupplementalFields = newSupplemental }
    where
        oldSupplemental = jaSupplementalFields oldAPN
        newSupplemental = M.insert fieldName (toJSON fieldValue) oldSupplemental

-- | Start a new session for sending APN messages. A session consists of a
-- connection pool of connections to the APN servers, while each connection has a
-- pool of workers that create HTTP2 streams to send individual push
-- notifications.
newSession
    :: Maybe FilePath
    -- ^ Path to the client certificate key
    -> Maybe FilePath
    -- ^ Path to the client certificate
    -> Maybe FilePath
    -- ^ Path to the CA
    -> Bool
    -- ^ Whether to use JWT as a bearer token
    -> Bool
    -- ^ True if the apn development servers should be used, False to use the production servers
    -> Int
    -- ^ How many messages will be sent in parallel? This corresponds to the number of http2 streams open in parallel; 100 seems to be a default value.
    -> Int
    -- ^ How many connections to be opened at maximum.
    -> ByteString
    -- ^ Topic (bundle name of the app)
    -> IO ApnSession
    -- ^ The newly created session
newSession certKey certPath caPath useJwt dev maxparallel maxConnectionCount topic = do
    let hostname = if dev
            then "api.sandbox.push.apple.com"
            else "api.push.apple.com"
        connInfo = ApnConnectionInfo certPath certKey caPath hostname maxparallel topic useJwt
    unless useJwt $ do
      certsOk <- checkCertificates connInfo
      unless certsOk $ error "Unable to load certificates and/or the private key"

    isOpen <- newIORef True

    let connectionUnusedTimeout :: NominalDiffTime
        connectionUnusedTimeout = 300
    pool <-
        createPool
            (newConnection connInfo) closeApnConnection 1 connectionUnusedTimeout maxConnectionCount
    let session =
            ApnSession
            { apnSessionPool = pool
            , apnSessionOpen = isOpen
            }
    return session

-- | Manually close a session. The session must not be used anymore
-- after it has been closed. Calling this function will close
-- the worker thread, and all open connections to the APN service
-- that belong to the given session. Note that sessions will be closed
-- automatically when they are garbage collected, so it is not necessary
-- to call this function.
closeSession :: ApnSession -> IO ()
closeSession s = do
    isOpen <- atomicModifyIORef' (apnSessionOpen s) (False,)
    unless isOpen $ error "Session is already closed"
    destroyAllResources (apnSessionPool s)

-- | Check whether a session is open or has been closed
-- by a call to closeSession
isOpen :: ApnSession -> IO Bool
isOpen = readIORef . apnSessionOpen

timeoutSeconds :: Int
timeoutSeconds = 300 * 1_000_000 -- 300 seconds to microseconds

withConnection :: ApnSession -> (ApnConnection -> ClientIO a) -> ClientIO a
withConnection s action = do
    lift $ ensureOpen s
    ExceptT . try $
        withResource (apnSessionPool s) $ \conn -> do
        mRes <- timeout timeoutSeconds (runClientIO (action conn))
        case mRes of
          Nothing -> do
            throw EarlyEndOfStream
          Just eRes -> do
            case eRes of
              Left clientError ->
                  -- When there is a clientError, we think that the connetion is broken.
                  -- Throwing an exception is the way we inform the resource pool.
                  throw clientError
              Right res -> return res

checkCertificates :: ApnConnectionInfo -> IO Bool
checkCertificates aci = do
  case (aciUseJWT aci) of
    True -> pure False
    False -> do
      castore <- maybe (pure Nothing) readCertificateStore $ aciCaPath aci
      credential <- case (aciCertPath aci, aciCertKey aci) of
        (Just cert, Just key) -> credentialLoadX509 cert key
        (Nothing, Nothing) -> pure $ Left "no creds"
      return $ isJust castore && isRight credential

newConnection :: ApnConnectionInfo -> IO ApnConnection
newConnection aci = do
    let maxConcurrentStreams = aciMaxConcurrentStreams aci
        conf = [ (HTTP2.SettingsMaxFrameSize, 16384)
               , (HTTP2.SettingsMaxConcurrentStreams, maxConcurrentStreams)
               , (HTTP2.SettingsMaxHeaderBlockSize, 4096)
               , (HTTP2.SettingsInitialWindowSize, 65536)
               , (HTTP2.SettingsEnablePush, 1)
               ]
        hostname = aciHostname aci

    clip <- case (aciUseJWT aci) of
        True -> do
          castore <- getSystemCertificateStore
          let maxConcurrentStreams = aciMaxConcurrentStreams aci
              clip = ClientParams
                  { clientUseMaxFragmentLength=Nothing
                  , clientServerIdentification=(T.unpack hostname, undefined)
                  , clientUseServerNameIndication=True
                  , clientWantSessionResume=Nothing
                  , clientShared=def
                      { sharedCAStore=castore }
                  , clientHooks=def
                      { onCertificateRequest = const . return $ Nothing }
                  , clientDebug=DebugParams { debugSeed=Nothing, debugPrintSeed=const $ return (), debugVersionForced=Nothing, debugKeyLogger=const $ return () }
                  , clientSupported=def
                      { supportedVersions=[ TLS12 ]
                      , supportedCiphers=ciphersuite_strong }
                  , clientEarlyData=Nothing
                  }
          pure clip
        False -> do
          Just castore <- maybe (pure Nothing) readCertificateStore $ aciCaPath aci
          Right credential <- case (aciCertPath aci, aciCertKey aci) of
            (Just cert, Just key) -> credentialLoadX509 cert key
            (Nothing, Nothing) -> pure $ Left "no creds"
          let credentials = Credentials [credential]
              shared      = def { sharedCredentials = credentials
                                , sharedCAStore=castore }

              clip = ClientParams
                  { clientUseMaxFragmentLength=Nothing
                  , clientServerIdentification=(T.unpack hostname, undefined)
                  , clientUseServerNameIndication=True
                  , clientWantSessionResume=Nothing
                  , clientShared=shared
                  , clientHooks=def
                      { onCertificateRequest=const . return . Just $ credential }
                  , clientDebug=DebugParams { debugSeed=Nothing, debugPrintSeed=const $ return (), debugVersionForced=Nothing, debugKeyLogger=const $ return () }
                  , clientSupported=def
                      { supportedVersions=[ TLS12 ]
                      , supportedCiphers=ciphersuite_strong }
                  , clientEarlyData=Nothing
                  }
          pure clip

    isOpen <- newIORef True
    let handleGoAway rsgaf = do
            lift $ writeIORef isOpen False
            return ()
    client <-
        fmap (either throw id) . runClientIO $ do
        httpFrameConnection <- newHttp2FrameConnection (T.unpack hostname) 443 (Just clip)
        client <-
            newHttp2Client httpFrameConnection 4096 4096 conf handleGoAway ignoreFallbackHandler
        linkAsyncs client
        return client
    flowWorker <- forkIO $ forever $ do
        updated <- runClientIO $ _updateWindow $ _incomingFlowControl client
        threadDelay 1000000
    workersem <- newQSem maxConcurrentStreams
    return $ ApnConnection client aci workersem flowWorker isOpen


closeApnConnection :: ApnConnection -> IO ()
closeApnConnection connection =
    -- Ignoring ClientErrors in this place. We want to close our session, so we do not need to
    -- fail on this kind of errors.
    void $ runClientIO $ do
    lift $ writeIORef (apnConnectionOpen connection) False
    let flowWorker = apnConnectionFlowControlWorker connection
    lift $ killThread flowWorker
    _gtfo (apnConnectionConnection connection) HTTP2.NoError ""
    _close (apnConnectionConnection connection)


-- | Send a raw payload as a push notification message (advanced)
sendRawMessage
    :: ApnSession
    -- ^ Session to use
    -> ApnToken
    -- ^ Device to send the message to
    -> Maybe ByteString
    -- ^ JWT Bearer Token
    -> ByteString
    -- ^ The message to send
    -> IO ApnMessageResult
    -- ^ The response from the APN server
sendRawMessage s deviceToken mJwtToken payload = catchErrors $
    withConnection s $ \c ->
        sendApnRaw c deviceToken mJwtToken payload

-- | Send a push notification message.
sendMessage
    :: ApnSession
    -- ^ Session to use
    -> ApnToken
    -- ^ Device to send the message to
    -> Maybe ByteString
    -- ^ JWT Bearer Token
    -> JsonAps
    -- ^ The message to send
    -> IO ApnMessageResult
    -- ^ The response from the APN server
sendMessage s token mJwt payload = catchErrors $
    withConnection s $ \c ->
        sendApnRaw c token mJwt message
  where message = L.toStrict $ encode payload

-- | Send a silent push notification
sendSilentMessage
    :: ApnSession
    -- ^ Session to use
    -> ApnToken
    -- ^ Device to send the message to
    -> Maybe ByteString
    -- ^ JWT Bearer Token
    -> IO ApnMessageResult
    -- ^ The response from the APN server
sendSilentMessage s token mJwt = catchErrors $
    withConnection s $ \c ->
        sendApnRaw c token mJwt message
  where message = "{\"aps\":{\"content-available\":1}}"

ensureOpen :: ApnSession -> IO ()
ensureOpen s = do
    open <- isOpen s
    unless open $ error "Session is closed"

-- | Send a push notification message.
sendApnRaw
    :: ApnConnection
    -- ^ Connection to use
    -> ApnToken
    -- ^ Device to send the message to
    -> Maybe ByteString
    -- ^ JWT Bearer Token
    -> ByteString
    -- ^ The message to send
    -> ClientIO ApnMessageResult
sendApnRaw connection deviceToken mJwtBearerToken message = bracket_
  (lift $ waitQSem (apnConnectionWorkerPool connection))
  (lift $ signalQSem (apnConnectionWorkerPool connection)) $ do
    let aci = apnConnectionInfo connection
        requestHeaders = maybe (defaultHeaders hostname token1 topic)
                         (\bearerToken -> (defaultHeaders hostname token1 topic) <> [ ( "authorization", "bearer " <> bearerToken ) ])
                         mJwtBearerToken
        hostname = aciHostname aci
        topic = aciTopic aci
        client = apnConnectionConnection connection
        token1 = unApnToken deviceToken

    res <- _startStream client $ \stream ->
        let init = headers stream requestHeaders id
            handler isfc osfc = do
                -- sendData client stream (HTTP2.setEndStream) message
                upload message (HTTP2.setEndHeader . HTTP2.setEndStream) client (_outgoingFlowControl client) stream osfc
                let pph hStreamId hStream hHeaders hIfc hOfc =
                        lift $ print hHeaders
                response <- waitStream stream isfc pph
                let (errOrHeaders, frameResponses, _) = response
                case errOrHeaders of
                    Left err -> throwIO (ApnExceptionHTTP $ toErrorCodeId err)
                    Right hdrs1 -> do
                        let status       = getHeaderEx ":status" hdrs1
                            -- apns-id      = getHeaderEx "apns-id" hdrs1
                            [Right body] = frameResponses

                        return $ case status of
                            "200" -> ApnMessageResultOk
                            "400" -> decodeReason ApnMessageResultFatalError body
                            "403" -> decodeReason ApnMessageResultFatalError body
                            "405" -> decodeReason ApnMessageResultFatalError body
                            "410" -> decodeReason ApnMessageResultFatalError body
                            "413" -> decodeReason ApnMessageResultFatalError body
                            "429" -> decodeReason ApnMessageResultTemporaryError body
                            "500" -> decodeReason ApnMessageResultTemporaryError body
                            "503" -> decodeReason ApnMessageResultTemporaryError body
        in StreamDefinition init handler
    case res of
        Left _     -> return ApnMessageResultBackoff -- Too much concurrency
        Right res1 -> return res1

    where
        decodeReason :: FromJSON response => (response -> ApnMessageResult) -> ByteString -> ApnMessageResult
        decodeReason ctor = either (throw . ApnExceptionJSON) id . decodeBody . L.fromStrict
            where
                decodeBody body =
                    eitherDecode body
                        >>= parseEither (\obj -> ctor <$> obj .: "reason")

        getHeaderEx :: HTTP2.HeaderName -> [HTTP2.Header] -> HTTP2.HeaderValue
        getHeaderEx name headers = fromMaybe (throw $ ApnExceptionMissingHeader name) (DL.lookup name headers)

        defaultHeaders :: Text -> ByteString -> ByteString -> [(HTTP2.HeaderName, ByteString)]
        defaultHeaders hostname token topic = [ ( ":method", "POST" )
                                              , ( ":scheme", "https" )
                                              , ( ":authority", TE.encodeUtf8 hostname )
                                              , ( ":path", "/3/device/" `S.append` token )
                                              , ( "apns-topic", topic ) ]


catchErrors :: ClientIO ApnMessageResult -> IO ApnMessageResult
catchErrors = catchIOErrors . catchClientErrors
    where
        catchIOErrors :: IO ApnMessageResult -> IO ApnMessageResult
        catchIOErrors = flip catchIOError (return . ApnMessageResultIOError)

        catchClientErrors :: ClientIO ApnMessageResult -> IO ApnMessageResult
        catchClientErrors act =
            either ApnMessageResultClientError id <$> runClientIO act


-- The type of permanent error indicated by APNS
-- See https://apple.co/2RDCdWC table 8-6 for the meaning of each value.
data ApnFatalError = ApnFatalErrorBadCollapseId
                   | ApnFatalErrorBadDeviceToken
                   | ApnFatalErrorBadExpirationDate
                   | ApnFatalErrorBadMessageId
                   | ApnFatalErrorBadPriority
                   | ApnFatalErrorBadTopic
                   | ApnFatalErrorDeviceTokenNotForTopic
                   | ApnFatalErrorDuplicateHeaders
                   | ApnFatalErrorIdleTimeout
                   | ApnFatalErrorMissingDeviceToken
                   | ApnFatalErrorMissingTopic
                   | ApnFatalErrorPayloadEmpty
                   | ApnFatalErrorTopicDisallowed
                   | ApnFatalErrorBadCertificate
                   | ApnFatalErrorBadCertificateEnvironment
                   | ApnFatalErrorExpiredProviderToken
                   | ApnFatalErrorForbidden
                   | ApnFatalErrorInvalidProviderToken
                   | ApnFatalErrorMissingProviderToken
                   | ApnFatalErrorBadPath
                   | ApnFatalErrorMethodNotAllowed
                   | ApnFatalErrorUnregistered
                   | ApnFatalErrorPayloadTooLarge
                   | ApnFatalErrorOther Text
    deriving (Eq, Show, Generic)

instance FromJSON ApnFatalError where
    parseJSON json =
        let result = parse genericParser json
        in
            case result of
                Success success -> return success
                Error err -> case json of
                                String other -> return $ ApnFatalErrorOther other
                                _            -> fail err

        where
            genericParser = genericParseJSON defaultOptions {
                                constructorTagModifier = drop 13,
                                sumEncoding = UntaggedValue
                            }

-- The type of transient error indicated by APNS
-- See https://apple.co/2RDCdWC table 8-6 for the meaning of each value.
data ApnTemporaryError = ApnTemporaryErrorTooManyProviderTokenUpdates
                       | ApnTemporaryErrorTooManyRequests
                       | ApnTemporaryErrorInternalServerError
                       | ApnTemporaryErrorServiceUnavailable
                       | ApnTemporaryErrorShutdown
    deriving (Enum, Eq, Show, Generic, ToJSON)

instance FromJSON ApnTemporaryError where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = drop 17 }
