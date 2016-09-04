{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings,
             TemplateHaskell #-}
module App where

import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

data App = App
  { _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _db   :: Snaplet Postgres
  }

makeLenses ''App

type AppHandler = Handler App App

