{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.PSQL.Tag
  (
    Tag(..)
  ) where

import           Article.Types.Class
import           Article.Types.Internal
import           Data.Aeson             (ToJSON (..), object, (.=))
import           Database.PSQL.Types    (FromRow (..), field)

data Tag = Tag
  { tagID        :: ID
  , tagName      :: TagName
  , tagCreatedAt :: CreatedAt
  }
  deriving (Show)

instance FromRow Tag where
  fromRow = do
    tagID <- field
    tagName <- field
    tagCreatedAt <- field
    return Tag {..}

instance ToJSON Tag where
  toJSON Tag{..} = object
    [ "id"         .= tagID
    , "name"       .= tagName
    , "created_at" .= tagCreatedAt
    ]

instance Created Tag where
  createdAt = tagCreatedAt
