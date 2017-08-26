{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI

import Network.PushNotify.APN


data ApnOptions = ApnOptions
  { certpath :: String
  , keypath  :: String
  , capath   :: String
  , topic    :: String
  , token    :: String
  , sandbox  :: Bool }

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
    forever $ do
        TI.putStrLn "what? (msg or range)"
        what <- TI.getLine
        TI.putStrLn "token> "
        token <- TI.getLine
        case what of
            "msg" -> do
                TI.putStrLn "title> "
                title <- TI.getLine
                TI.putStrLn "text> "
                text <- TI.getLine
                TI.putStrLn "sound> "
                sound <- TI.getLine
                let payload = JsonAps (JsonApsMessage (Just $ JsonApsAlert title text) (Just 23) (Just sound) Nothing) Nothing
                sendMessage session (TE.encodeUtf8 token) payload >>= print
                return ()
            "range" -> do
                TI.putStrLn "from> "
                from <- read . T.unpack <$> TI.getLine
                TI.putStrLn "to> "
                to <- read . T.unpack <$> TI.getLine
                let range = [(from :: Int) .. (to :: Int)]
                TI.putStrLn "delay (us)> "
                delay <- read . T.unpack <$> TI.getLine
                let token' = TE.encodeUtf8 token
                _ <- forkIO $ forM_ range $ \num -> do
                    let payload = JsonAps (JsonApsMessage Nothing (Just num) Nothing Nothing) Nothing
                    sendMessage session token' payload >>= print
                    threadDelay delay
                return ()
