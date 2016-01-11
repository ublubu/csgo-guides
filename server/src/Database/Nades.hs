{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Database.Nades where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Persist.Sql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.TH (share, mkPersist, sqlSettings,
                            mkMigrate, persistLowerCase)
import GHC.Generics (Generic)

import Server.App

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Nade
  authorId Text
  images [Text]
  description Text
  tags [Text]
  deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

loadDb :: AppConfig -> IO ()
loadDb config =
  runSqlPool doMigrations $ _appConfigSqlPool config

runDb query = do
  pool <- asks _appConfigSqlPool
  liftIO $ runSqlPool query pool
