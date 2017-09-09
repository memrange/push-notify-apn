{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Network.PushNotify.APN


data ApnOptions = ApnOptions
  { certpath    :: !String
  , keypath     :: !String
  , capath      :: !String
  , topic       :: !String
  , tokens      :: !([String])
  , sandbox     :: !Bool
  , interactive :: !Bool
  , text        :: !(Maybe String) }

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
      <*> many ( strOption
          ( short 't'
         <> metavar "TOKEN"
         <> help "Tokens of the devices to send the notification to" ) )
      <*> switch
          ( long "sandbox"
         <> short 's'
         <> help "Whether to use the sandbox (non-production deployments)" )
      <*> switch
          ( long "interactive"
         <> short 'i'
         <> help "Run an interactive mode; each line must have this format: token:message " )
      <*> optional (strOption
          ( short 'm'
         <> metavar "MESSAGE"
         <> help "Message to send to the device" ))
      

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
    if interactive o
    then forever $ do
        TI.putStrLn "Message in the form token:message please"
        line <- TI.getLine
        let parts = T.splitOn ":" line
        if length parts >= 2
        then
            let token   = hexEncodedToken $ head parts
                text    = T.intercalate ":" (tail parts)
                payload = alertMessage "push-notify-apn" text
                message = newMessage payload
            in sendMessage session token message >>= TI.putStrLn . T.pack . show
        else TI.putStrLn "Erroneous format"
    else do
        when (isNothing $ text o) $ error "You need to specify a message with -m"
        let payload  = alertMessage "push-notify-apn" (T.pack $ fromJust $ text o)
            message  = newMessage payload
        forM_ (tokens o) $ \token ->
            let apntoken = hexEncodedToken . T.pack $ token
            in sendMessage session apntoken message >>= print
