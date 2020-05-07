module Server
  ( app
  )
  where

import qualified Analyser
import qualified Rekognition

import qualified Network.Wai.Handler.Warp as Warp
import           Protolude
import           Servant                  ((:>))
import qualified Servant
import qualified Servant.Multipart        as Servant

app :: IO ()
app
  = Warp.run 8080
  $ Servant.serve api server

type API
  =  Servant.MultipartForm Servant.Mem (Servant.MultipartData Servant.Mem)
  :> Servant.Post '[Servant.JSON] Analyser.PersonalDetails

api :: Proxy API
api = Proxy

server :: Servant.Server API
server Servant.MultipartData{..}
  = case headMay files of
      Nothing ->
        throwError Servant.err400 { Servant.errBody = "We need a file!" }

      Just Servant.FileData{..} -> do
        response <- Rekognition.detectText fdPayload

        pure $ Analyser.extractPersonalDetails response
