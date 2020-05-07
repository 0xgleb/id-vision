module AnalyserSpec (spec) where

import Analyser

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Time            as T
import           Protolude

import Test.Hspec

spec :: Spec
spec = do
  describe "extractPersonalDetails" $ do
    it "can extract Elizabeth Henderson's personal details" $ do
      let expected = PersonalDetails
            { surname      = Just "Henderson"
            , givenNames   = Just "Elizabeth"
            , dateOfBirth  = Just $ T.fromGregorian 1977 04 14
            , documentType = NationalIdentityCard
            }

      response <- Aeson.decode <$> BSL.readFile "./test-resources/elizabeth-henderson.json"

      extractPersonalDetails <$> response `shouldBe` Just expected

    it "can extract James Henderson's personal details" $ do
      let expected = PersonalDetails
            { surname      = Just "Henderson"
            , givenNames   = Just "James"
            , dateOfBirth  = Just $ T.fromGregorian 1977 04 14
            , documentType = NationalIdentityCard
            }

      response <- Aeson.decode <$> BSL.readFile "./test-resources/james-henderson.json"

      extractPersonalDetails <$> response `shouldBe` Just expected
