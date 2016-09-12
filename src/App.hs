{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module App where

import ClassyPrelude
import Control.Lens
import Network.AWS (Env)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

data App = App
  { _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _db   :: Snaplet Postgres
  , _aws  :: Snaplet Env
  }

makeLenses ''App

type AppHandler = Handler App App
