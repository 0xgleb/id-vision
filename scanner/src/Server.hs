module Server
  ( app
  )
  where

import qualified Analyser
import qualified Rekognition

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.Cors          as Cors
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Protolude
import           Servant                              ((:>))
import qualified Servant
import qualified Servant.Multipart                    as Servant

app :: IO ()
app
  = Warp.run 8080
  $ RequestLogger.logStdout
  $ cors
  $ Servant.serve api server

cors :: Wai.Middleware
cors = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsRequestHeaders = "content-type" : Cors.simpleHeaders
  }

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
