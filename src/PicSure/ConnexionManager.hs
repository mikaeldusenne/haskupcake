module PicSure.ConnexionManager where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

createManager = newManager tlsManagerSettings
