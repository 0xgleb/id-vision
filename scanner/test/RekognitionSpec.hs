module RekognitionSpec (spec) where

import Rekognition

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Protolude

import Test.Hspec

spec :: Spec
spec = do
  describe "fromJSON" $ do
    it "can parse Elizabeth Henderson response" $ do
      let expected =
            [ TextDetection
                { detectionType = Line
                , detectedText  = "National Identity Card"
                }

            , TextDetection
                { detectionType = Line
                , detectedText  = "Surname/Nom Henderson"
                }

            , TextDetection
                { detectionType = Line
                , detectedText  = "Given names/ Elizabeth"
                }
            ]

      response <- BSL.readFile "./test-resources/elizabeth-henderson.json"

      (take 3 . textDetections <$> Aeson.decode response) `shouldBe` Just expected
