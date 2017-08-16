module Main where

import Control.Concurrent
import Data.Semigroup ((<>))
import Options.Applicative

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import APN


data ApnOptions = ApnOptions
  { certpath :: String
  , keypath  :: String
  , capath   :: String
  , topic    :: String
  , token    :: String
  , sandbox  :: Bool
  , text     :: String }

p :: Parser ApnOptions
p = ApnOptions
      <$> strOption
          ( short 'c'
         <> metavar "CERTIFICATE"
         <> help "Path to the certificate" )
      <*> strOption
          ( short 'k'
         <> metavar "PRIVATEKEY"
         <> help "Path to the certificate's private key" )
      <*> strOption
          ( short 'a'
         <> metavar "CATRUSTSTORE"
         <> help "Path to the CA truststore" )
      <*> strOption
          ( short 'b'
         <> metavar "BUNDLEID"
         <> help "Bundle ID of the app to send the notification to. Must correspond to the certificate." )
      <*> strOption
          ( short 't'
         <> metavar "TOKEN"
         <> help "Token of the device to send the notification to" )
      <*> switch
          ( long "sandbox"
         <> short 's'
         <> help "Whether to use the sandbox (non-production deployments)" )
      <*> strOption
          ( short 'm'
         <> metavar "MESSAGE"
         <> help "Message to send to the device" )
      

main :: IO ()
main = send =<< execParser opts
  where
    opts = info (p <**> helper)
      ( fullDesc
     <> progDesc "Sends a push notification to an apple device"
     <> header "apn- a test for the apn library" )

send :: ApnOptions -> IO ()
send o = do
    session <- newSession (keypath o) (certpath o) (capath o) (sandbox o) 10 (B8.pack $ topic o)
    let payload = JsonAps (JsonApsMessage (Just $ T.pack $ text o) Nothing Nothing Nothing) Nothing
    sendMessage session (B8.pack $ token o) payload >>= print
