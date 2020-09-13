{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.PSQL.File
  ( File (..)
  , fileEmpty
  ) where

import           Article.Types.Class
import           Article.Types.Internal
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         object, parseJSON, withObject, (.:),
                                         (.=))
import           Data.Hashable          (Hashable (..))
import           Data.Maybe             (fromMaybe)
import           Database.PSQL.Types    (FromRow (..), field)
import           GHC.Generics           (Generic)

data File = File
  { fileID        :: ID
  , fileKey       :: FileKey
  , fileBucket    :: FileBucket
  , fileExtra     :: FileExtra
  , fileCreatedAt :: CreatedAt
  }
  deriving (Generic, Show)

instance Eq File where
  f1 == f2 = fileID f1 == fileID f2

instance Hashable File where
  hashWithSalt s f = hashWithSalt s $ fileID f

instance FromRow File where
  fromRow = do
    fileID <- field
    fileKey <- field
    fileBucket <- field
    fileExtra <- fromMaybe Null <$> field
    fileCreatedAt <- field
    return File {..}

fileEmpty :: File
fileEmpty = File
  { fileID        = 0
  , fileKey       = ""
  , fileBucket    = ""
  , fileExtra     = Null
  , fileCreatedAt = 0
  }

instance ToJSON File where
  toJSON File{..} = object
    [ "id"         .= fileID
    , "key"        .= fileKey
    , "bucket"     .= fileBucket
    , "extra"      .= fileExtra
    , "created_at" .= fileCreatedAt
    ]

instance FromJSON File where
  parseJSON = withObject "File" $ \o -> do
    fileID        <- o .: "id"
    fileKey       <- o .: "key"
    fileBucket    <- o .: "bucket"
    fileExtra     <- o .: "extra"
    fileCreatedAt <- o .: "created_at"
    return File{..}

instance Created File where
  createdAt = fileCreatedAt
