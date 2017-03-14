{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Article.Types.CardItem
  (
    CardItem(..)
  ) where

import           Data.Aeson               (ToJSON (..), Value (..), object,
                                           (.=))

import           Article.Types.Internal
import           Article.Types.MySQL.File

data CardItem = CardItem { cardID        :: ID
                         , cardTitle     :: Title
                         , cardSummary   :: Summary
                         , cardImage     :: Maybe File
                         , cardURL       :: String
                         , cardTags      :: [TagName]
                         , cardExtra     :: Value
                         , cardCreatedAt :: CreatedAt
                         }

instance ToJSON CardItem where
  toJSON CardItem{..} = object [ "id"          .= cardID
                               , "title"       .= cardTitle
                               , "summary"     .= cardSummary
                               , "file"        .= cardImage
                               , "tags"        .= cardTags
                               , "extra"       .= cardExtra
                               , "created_at"  .= cardCreatedAt
                               ]
