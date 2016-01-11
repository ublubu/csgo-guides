{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Nades where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Persist.Sql (SqlBackend(..), runMigration)
import Database.Persist.TH (share, mkPersist, sqlSettings,
                            mkMigrate, persistLowerCase)
import GHC.Generics (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Nade
  authorId Text
  images [Text]
  title Text
  description Text Maybe
  tags [Text]
  deriving Eq Show
NadeList
  authorId Text
  title Text
  description Text Maybe
  deriving Eq Show
NadeListing
  ordinal Int
  nade NadeId
  nadeList NadeListId
  deriving Eq Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll
