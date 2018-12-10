{-# LANGUAGE OverloadedStrings #-}

module Network.PushNotify.APNSpec (spec) where

import           Data.Aeson
import           Network.PushNotify.APN
import           Test.Hspec

spec :: Spec
spec =
  describe "JsonApsMessage" $
    context "JSON encoder" $ do
      it "encodes an APNS message with a title and body" $
        toJSON (alertMessage "hello" "world") `shouldBe`
          object [
            "category" .= Null,
            "sound"    .= Null,
            "badge"    .= Null,
            "alert"    .= object [
              "title" .= String "hello",
              "body"  .= String "world"
            ]
          ]
      it "encodes an APNS message with a title and no body" $
        toJSON (bodyMessage "hello world") `shouldBe`
          object [
            "category" .= Null,
            "sound"    .= Null,
            "badge"    .= Null,
            "alert"    .= object [ "body"  .= String "hello world" ]
          ]
