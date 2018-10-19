{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Article.GraphQL
  (
    schema
  ) where

import           Article.API
import           Article.Config        (Cache)
import           Article.Types
import           Control.Applicative   (Alternative (..))
import           Data.GraphQL.Schema   (Resolver, Schema, arrayA', object,
                                        objectA', scalar, scalarA)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (unpack)
import           Haxl.Core             (GenHaxl)
import           Yuntan.Types.HasMySQL (HasMySQL, HasOtherEnv)
import           Yuntan.Types.OrderBy  (desc)
import           Yuntan.Utils.GraphQL  (getIntValue, getTextValue, pickValue,
                                        value)

--  type Query {
--    file(key: String!): File
--    article(id: Int!): Article
--    articles(from: Int, size: Int): [Article]
--    tag(id: Int!): Tag
--    tag(name: String!): Tag
--    timeline(name: String!, from: Int, size: Int): [Article]
--    article_count: Int
--    timeline_count(name: String!): Int
--    timeline_meta(name: String!): TimelineMeta
--  }
--  type File {
--    id: Int
--    key: String
--    bucket: String
--    extra: Value
--    pick_extra(keys: [String]): Value
--    created_at: Int
--  }
--  type Article {
--    id: Int
--    title: String
--    summary: String
--    content: String
--    from_url: String
--    tags: [String]
--    timelines: [String]
--    cover: File
--    extra: Value
--    pick_extra(keys: [String]): Value
--    created_at: Int
--  }
--  type Tag {
--    id: Int
--    name: String
--    created_at: Int
--  }
--  type TimelineMeta {
--    title: String
--    summary: String
--  }

schema :: (HasMySQL u, HasOtherEnv Cache u) => Schema (GenHaxl u)
schema = file :| [article, articles, tag, timeline, articleCount, timelineCount, timelineMeta]

file :: HasMySQL u => Resolver (GenHaxl u)
file = objectA' "file" $ \argv ->
  case getTextValue "key" argv of
    Nothing  -> empty
    Just key -> maybe [] file_ <$> getFileWithKey (unpack key)

file_ :: HasMySQL u => File -> [Resolver (GenHaxl u)]
file_ File{..} =
  [ scalar    "id"         fileID
  , scalar    "key"        fileKey
  , scalar    "bucket"     fileBucket
  , value     "extra"      fileExtra
  , pickValue "pick_extra" fileExtra
  , scalar    "created_at" fileCreatedAt
  ]

article :: (HasMySQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u)
article = objectA' "article" $ \argv ->
  case getIntValue "id" argv of
    Nothing    -> empty
    Just artId -> maybe [] article_ <$> getArticleById artId

article_ :: HasMySQL u => Article -> [Resolver (GenHaxl u)]
article_ Article{..} =
  [ scalar    "id"         artID
  , scalar    "title"      artTitle
  , scalar    "summary"    artSummary
  , scalar    "content"    artContent
  , scalar    "from_url"   artFromURL
  , scalar    "tags"       artTags
  , scalar    "timelines"  artTimelines
  , object    "cover"      (maybe [] file_ artCover)
  , value     "extra"      artExtra
  , pickValue "pick_extra" artExtra
  , scalar    "created_at" artCreatedAt
  ]

articles :: (HasMySQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u)
articles = arrayA' "articles" $ \ argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  map article_ <$> getArticleList from size (desc "id")

tag :: HasMySQL u => Resolver (GenHaxl u)
tag = objectA' "tag" $ \argv -> do
  let tagId = getIntValue "id" argv
      name = getTextValue "name" argv
  case (tagId, name) of
    (Nothing, Nothing) -> empty
    (Just tagId', _)   -> maybe [] tag_ <$> getTagById tagId'
    (_, Just name')    -> maybe [] tag_ <$> getTagByName (unpack name')

tag_ :: HasMySQL u => Tag -> [Resolver (GenHaxl u)]
tag_ Tag{..} =
  [ scalar "id"         tagID
  , scalar "name"       tagName
  , scalar "created_at" tagCreatedAt
  ]

timeline :: (HasMySQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u)
timeline = arrayA' "timeline" $ \ argv -> do
  let from = fromMaybe 0  $ getIntValue "from" argv
      size = fromMaybe 10 $ getIntValue "size" argv

  case getTextValue "name" argv of
    Nothing   -> empty
    Just name ->
      map article_ <$> getArticleListByTimeline (unpack name) from size (desc "art_id")

articleCount :: (HasMySQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u)
articleCount = scalarA "article_count" $ \ case
  [] -> countArticle
  _  -> empty

timelineCount :: (HasMySQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u)
timelineCount = scalarA "timeline_count" $ \ argv ->
  case getTextValue "name" argv of
    Nothing   -> empty
    Just name -> countTimeline (unpack name)

timelineMeta :: HasMySQL u => Resolver (GenHaxl u)
timelineMeta = objectA' "timeline_meta" $ \argv ->
  case getTextValue "name" argv of
    Nothing   -> empty
    Just name -> maybe [] timelineMeta_ <$> getTimelineMeta (unpack name)

timelineMeta_ :: (Title, Summary) -> [Resolver (GenHaxl u)]
timelineMeta_ (title, summary) =
  [ scalar "title"   title
  , scalar "summary" summary
  ]
