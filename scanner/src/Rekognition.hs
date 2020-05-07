module Rekognition
  ( Response(..)
  , TextDetection(..)
  , DetectionType(..)
  , detectText
  )
  where

import           Control.Lens
import qualified Control.Monad.Catch         as Catch
import qualified Control.Monad.Trans.AWS     as AWST
import           Data.Aeson                  ((.:))
import qualified Data.Aeson                  as Aeson
import qualified Network.AWS                 as AWS
import qualified Network.AWS.Data.ByteString as AWS.BS
import qualified Network.AWS.Rekognition     as AWS.Rekognition
import           Protolude

detectText
  :: forall m.
     ( MonadIO m
     , Catch.MonadCatch m

     )
  => LByteString
  -> m Response

detectText imageBS = do
  env <- awsEnv

  liftIO
    . AWS.runResourceT
    . AWST.runAWST env
    . AWST.within region
    . AWST.reconfigure AWS.Rekognition.rekognition
    $ do

    let image
          = AWS.Rekognition.image
          & AWS.Rekognition.iBytes
          .~ Just (AWS.BS.toBS imageBS)

    response <- AWS.send (AWS.Rekognition.detectText image)

    let textDetections = response ^. AWS.Rekognition.dtrsTextDetections

    pure $ Response $ textDetections <&> \textDetection ->
      TextDetection
        { detectionType
            = case textDetection ^. AWS.Rekognition.tdType of
                Just AWS.Rekognition.Line -> Line
                Just AWS.Rekognition.Word -> Word
                Nothing                   -> DetectionTypeMissing

        , detectedText
            = fromMaybe "" $ textDetection ^. AWS.Rekognition.tdDetectedText
        }

  where
    awsEnv :: m AWST.Env
    awsEnv = AWST.newEnv AWST.Discover

    region :: AWST.Region
    region = AWST.Ireland


newtype Response
  = Response { textDetections :: [TextDetection] }
  deriving stock (Eq, Show)

instance Aeson.FromJSON Response where
  parseJSON
    = Aeson.withObject "FullRekognitionResponse" $ \object ->
        Response <$> object .: "TextDetections"


data TextDetection
  = TextDetection
      { detectionType :: DetectionType
      , detectedText  :: Text
      }
  deriving stock (Eq, Show)

data DetectionType
  = Line -- ^ LINE
  | Word -- ^ WORD
  | DetectionTypeMissing
  deriving stock (Eq, Show)

instance Aeson.FromJSON TextDetection where
  parseJSON = Aeson.withObject "TextDetection" $ \object -> do
    let parseDetectionType dtype
          | dtype == ("LINE" :: Text) = Line
          | dtype == "WORD"           = Word
          | otherwise                 = DetectionTypeMissing

    detectionType <- parseDetectionType <$> object .: "Type"

    detectedText <- object .: "DetectedText"

    pure TextDetection{..}
