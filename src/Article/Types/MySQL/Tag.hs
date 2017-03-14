{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.MySQL.Tag
  (
    Tag(..)
  ) where

import           Data.Aeson                         (ToJSON (..), object, (.=))
import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Article.Types.Class
import           Article.Types.Internal

data Tag = Tag { tagID        :: ID
               , tagName      :: TagName
               , tagCreatedAt :: CreatedAt
               }
  deriving (Show)

instance QueryResults Tag where
    convertResults [fa, fb, fc] [va, vb, vc] = Tag{..}
      where !tagID        = convert fa va
            !tagName      = convert fb vb
            !tagCreatedAt = convert fc vc
    convertResults fs vs  = convertError fs vs 2

instance ToJSON Tag where
  toJSON Tag{..} = object [ "id"         .= tagID
                          , "name"       .= tagName
                          , "created_at" .= tagCreatedAt
                          ]

instance Created Tag where
  createdAt = tagCreatedAt
