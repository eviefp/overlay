module Main where

import Prelude

import Data.Argonaut.Encode as Argonaut
import Data.DateTime.ISO as ISO
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as DT
import Foreign as F
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Simple.JSON as JSON
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Spec (Resource)
import Swerve.API.Verb (Get)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype IsoWrapper = IsoWrapper ISO.ISO

instance isoWrapperReadForeign :: JSON.ReadForeign IsoWrapper where
  readImpl _ = pure $ IsoWrapper $ ISO.ISO $ bottom

instance isoWrapperWriteForeign :: JSON.WriteForeign IsoWrapper where
   writeImpl (IsoWrapper iso) =
       F.unsafeToForeign (Argonaut.encodeJson iso)

type OverlayAPI = StartTimeAPI :<|> DummyAPI

type StartTimeAPI = Get "start-time"
    ( Resource { startTime :: IsoWrapper } JSON
    + ()
    )

type DummyAPI = Get "dummy" (Resource String PlainText + ())

type Handlers = Handler StartTimeAPI { startTime :: IsoWrapper } :<|> Handler DummyAPI String

startTimeAPI :: Handler StartTimeAPI { startTime :: IsoWrapper }
startTimeAPI = do
    startTime <- liftEffect $ DT.nowDateTime
    pure { startTime: IsoWrapper $ ISO.ISO startTime }

dummyAPI :: Handler DummyAPI String
dummyAPI = pure "dummy"

api :: Handlers
api =  startTimeAPI :<|> dummyAPI

app :: Application
app = swerve (Proxy :: _ OverlayAPI) api

main :: Effect Unit
main = do
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop, host = "0.0.0.0" } app
