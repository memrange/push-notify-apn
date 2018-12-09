-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

main = hspec $
  describe "Network.PushNotify.APN" $ do
    it "should run a test suite" $ do
      1 `shouldBe` 1
