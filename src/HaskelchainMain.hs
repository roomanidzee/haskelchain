module HaskelchainMain where

import AppServer (app)
import Network.Wai.Handler.Warp (run)

launch :: IO ()
launch = run 8081 app
