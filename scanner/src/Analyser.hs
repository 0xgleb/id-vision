{-# LANGUAGE DeriveAnyClass #-}

module Analyser
  ( PersonalDetails(..)
  , DocumentType(..)
  , extractPersonalDetails
  )
  where

import qualified Rekognition

import qualified Data.Aeson        as Aeson
import qualified Data.Text         as Tx
import qualified Data.Text.Metrics as Tx.Met
import qualified Data.Time         as T
import           Protolude

-- import qualified Debug.Trace as Debug

data PersonalDetails
  = PersonalDetails
      { surname      :: Maybe Text
      , givenNames   :: Maybe Text
      , dateOfBirth  :: Maybe T.Day
      , documentType :: DocumentType
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

data DocumentType
  = NationalIdentityCard
  | Unknown
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

emptyPersonalDetails :: PersonalDetails
emptyPersonalDetails
  = PersonalDetails
      { surname      = Nothing
      , givenNames   = Nothing
      , dateOfBirth  = Nothing
      , documentType = Unknown
      }


extractPersonalDetails :: Rekognition.Response -> PersonalDetails
extractPersonalDetails (Rekognition.Response detections)
  = foldl analyseLine (findDateOfBirth detections)
  $ filter (\Rekognition.TextDetection{..} -> detectionType == Rekognition.Line)
  $ detections


analyseLine
  :: PersonalDetails
  -> Rekognition.TextDetection
  -> PersonalDetails

analyseLine prev@PersonalDetails{..} Rekognition.TextDetection{..}
  | "surname" `Tx.isInfixOf` Tx.toLower detectedText
    || lowLevenshtein "Surname/Nom" (Tx.takeWhile (/= ' ') detectedText)

  = prev { surname = lastMay $ Tx.words detectedText }

  | "name" `Tx.isInfixOf` Tx.toLower detectedText
  = prev
      { givenNames
          = Just $ Tx.unwords $ drop 1
          $ dropWhile (not . Tx.isInfixOf "name" . Tx.toLower)
          $ Tx.words detectedText
      }

  | otherwise = checkDocumentType prev detectedText


checkDocumentType
  :: PersonalDetails
  -> Text
  -> PersonalDetails

checkDocumentType prev@PersonalDetails{..} lineText
  | lowLevenshtein "National Identity Card" lineText
  = prev { documentType = NationalIdentityCard }

  | otherwise = prev

lowLevenshtein :: Text -> Text -> Bool
lowLevenshtein expected actual
  = Tx.Met.levenshtein (Tx.toLower expected) (Tx.toLower actual) <= 4

findDateOfBirth :: [Rekognition.TextDetection] -> PersonalDetails
findDateOfBirth
  = foldl findDates emptyPersonalDetails
  . filter (\Rekognition.TextDetection{..} -> detectionType == Rekognition.Word)

  where
    findDates prev@PersonalDetails{..} Rekognition.TextDetection{..}
      = case T.parseTimeM True T.defaultTimeLocale "%-d-%-m-%Y" $ Tx.unpack detectedText of
          Just day | maybe True (day <) dateOfBirth ->
            prev { dateOfBirth = Just day }

          _ -> prev
