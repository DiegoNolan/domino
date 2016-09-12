{-# LANGUAGE NoImplicitPrelude,
             OverloadedStrings,
             FlexibleInstances,
             TemplateHaskell #-}
module App where

import ClassyPrelude
import Control.Lens
import Control.Monad.Representable.Reader
import Control.Monad.Representable.State
import Network.AWS (Env)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.AWS
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

instance HasPostgres (Handler App App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

instance HasAWS (Handler App App) where
  getAWSEnv = with aws get
