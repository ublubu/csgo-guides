{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Nades where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Int
import Data.Text (Text, isPrefixOf)
import GHC.Generics
import Servant.API

import API.SignIn
import Parsing.ImageSrc (isImgurSrc)

data Authored a = Authored { _authoredAuthor :: Text
                           , _authoredContents :: a
                           } deriving (Show, Eq, Generic)
makeLenses ''Authored

data Keyed a = Keyed { _keyedKey :: Int64
                     , _keyedContents :: a
                     } deriving (Show, Eq, Generic)
makeLenses ''Keyed

-- all fields (key & author) filled and ready for DB interaction
type DbFilled a = Authored (Keyed a)

data Nade' = Nade' { _nadeImages :: [Text]
                   , _nadeTitle :: Text
                   , _nadeDescription :: Maybe Text
                   , _nadeTags :: [Text]
                   } deriving (Show, Eq, Generic)
makeLenses ''Nade'

type Nade = DbFilled Nade'

data NadeList'' a = NadeList'' { _nadeListTitle :: Text
                               , _nadeListDescription :: Maybe Text
                               , _nadeListNades :: [a]
                               } deriving (Show, Eq, Generic)
makeLenses ''NadeList''

type NadeList = DbFilled (NadeList'' Nade)
type NadeList' = NadeList'' Int64

instance FromJSON a => FromJSON (Authored a)
instance ToJSON a => ToJSON (Authored a)

instance FromJSON a => FromJSON (Keyed a)
instance ToJSON a => ToJSON (Keyed a)

instance FromJSON Nade'
instance ToJSON Nade'

instance FromJSON a => FromJSON (NadeList'' a)
instance ToJSON a => ToJSON (NadeList'' a)

type NadeAPI =
  "nades" :> Get '[JSON] [Nade]
  :<|> "nades" :> ReqBody '[JSON] Nade' :> Cookied :> Post '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> Get '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> ReqBody '[JSON] Nade' :> Cookied :> Put '[JSON] Nade
  :<|> "nades" :> Capture "nadeId" Int64 :> Cookied :> Delete '[JSON] ()
  :<|> "myNades" :> Cookied :> Get '[JSON] [Nade]

type NadeListAPI =
  "nadeLists" :> Get '[JSON] [NadeList]
  :<|> "nadeLists" :> ReqBody '[JSON] NadeList' :> Cookied :> Post '[JSON] NadeList
  :<|> "nadeLists" :> Capture "nadeListId" Int64 :> Get '[JSON] NadeList
  :<|> "nadeLists" :> Capture "nadeListId" Int64 :> ReqBody '[JSON] NadeList' :> Cookied :> Put '[JSON] NadeList
  :<|> "nadeLists" :> Capture "nadeListId" Int64 :> Cookied :> Delete '[JSON] ()
  :<|> "myNadeLists" :> Cookied :> Get '[JSON] [NadeList]

type NadesAPI =
  NadeAPI :<|> NadeListAPI

dbFill :: Text -> Int64 -> a -> DbFilled a
dbFill author key x = Authored author (Keyed key x)

_dbFilledContents :: DbFilled a -> a
_dbFilledContents = _keyedContents . _authoredContents

_dbFilledKey :: DbFilled a -> Int64
_dbFilledKey = _keyedKey . _authoredContents

_dbFilledAuthor :: DbFilled a -> Text
_dbFilledAuthor = _authoredAuthor

data Sanitized a = Sanitized { _sanitizedValue :: a
                             } deriving (Show, Eq)
makeLenses ''Sanitized

withSanitizedNade :: (Sanitized Nade' -> a) -> Nade' -> a
withSanitizedNade f nade =
  f (Sanitized $ over nadeImages sanitizeImages nade)

sanitizeImages :: [Text] -> [Text]
sanitizeImages = filter isImgurSrc
