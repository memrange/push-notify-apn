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
  { certpath    :: !(Maybe String)
  , keypath     :: !(Maybe String)
  , capath      :: !(Maybe String)
  , topic       :: !String
  , jwt         :: !(Maybe B8.ByteString)
  , tokens      :: !([String])
  , sandbox     :: !Bool
  , interactive :: !Bool
  , text        :: !(Maybe String) }

p :: Parser ApnOptions
p = ApnOptions
      <$> (optional $ strOption
          ( short 'c'
         <> metavar "CERTIFICATE"
         <> help "Path to the certificate" ))
      <*> (optional $ strOption
          ( short 'k'
         <> metavar "PRIVATEKEY"
         <> help "Path to the certificate's private key" ))
      <*> (optional $ strOption
          ( short 'a'
         <> metavar "CATRUSTSTORE"
         <> help "Path to the CA truststore" ))
      <*> strOption
          ( short 'b'
         <> metavar "BUNDLEID"
         <> help "Bundle ID of the app to send the notification to. Must correspond to the certificate." )
      <*> (optional $ strOption
          ( short 'j'
         <> metavar "JWT"
         <> help "JWT Bearer token" ))
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
    let mkSession =
            newSession (keypath o) (certpath o) (capath o) (if isJust (jwt o) then True else False) (sandbox o) 10 1 (B8.pack $ topic o)
    session <- mkSession
    if interactive o
    then
        let loop sess = do
                TI.putStrLn "Message in the form token:sound:title:message please"
                line <- TI.getLine
                let parts = T.splitOn ":" line
                if length parts >= 4
                then
                    let token   = hexEncodedToken $ head parts
                        text    = T.intercalate ":" (drop 3 parts)
                        title   = parts !! 2
                        sound   = parts !! 1
                        payload = setSound sound . alertMessage title $ text
                        message = newMessage payload
                    in (sendMessage sess token (jwt o) message >>= TI.putStrLn . T.pack . show) >> loop sess
                else case line of
                    "close" -> closeSession sess >> loop sess
                    "reset" -> mkSession >>= loop
                    "quit"  -> return ()
                    _ -> TI.putStrLn "Erroneous format" >> loop sess
        in loop session
    else do
        when (isNothing $ text o) $ error "You need to specify a message with -m"
        let payload  = alertMessage "push-notify-apn" (T.pack $ fromJust $ text o)
            message  = newMessage payload
        forM_ (tokens o) $ \token ->
            let apntoken = hexEncodedToken . T.pack $ token
            in sendMessage session apntoken (jwt o) message >>= print
