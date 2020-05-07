module Main where

-- import qualified Analyser
-- import qualified Rekognition
import qualified Server

-- import qualified Data.ByteString.Lazy as BSL
import Protolude

main :: IO ()
main = Server.app

  -- = do
  -- mbFileName <- headMay <$> getArgs

  -- case mbFileName of
  --   Nothing ->
  --     putStrLn @Text "Please provide a file name!"

  --   Just fileName -> do
  --     image <- BSL.readFile fileName

  --     response <- Rekognition.detectText image

  --     let Analyser.PersonalDetails{..} = Analyser.extractPersonalDetails response

  --     putStrLn @Text "\nResults:\n"

  --     putStrLn @Text $ "Document type: " <> show documentType

  --     showIfFound "Surname: "       surname
  --     showIfFound "Given names: "   givenNames
  --     showIfFound "Date of birth: " (show <$> dateOfBirth)

  --     putStrLn @Text "\n"

  -- where
  --   showIfFound prefix = \case
  --     Just txt ->
  --       putStrLn @Text $ prefix <> txt

  --     Nothing ->
  --       pure ()
