module HaskelchainMain where

import AppServer (app)
import Network.Wai ()
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)

launch :: IO ()
launch = do
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8081 $ setLogger aplogger defaultSettings
    runSettings settings app
