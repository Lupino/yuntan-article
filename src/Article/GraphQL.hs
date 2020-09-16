{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Article.GraphQL
  ( schema
  , schemaByArticle
  ) where

import           Article.API
import           Article.Config      (Cache)
import           Article.Types
import           Control.Applicative (Alternative (..))
import           Data.Aeson          (Value (Null))
import           Data.Aeson.Helper   (union)
import           Data.GraphQL.Schema (Resolver, Schema, arrayA', object,
                                      objectA', scalar, scalarA)
import           Data.GraphQL.Utils  (getInt, getText, pick, value)
import           Data.List.NonEmpty  (NonEmpty ((:|)), fromList)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, unpack)
import           Database.PSQL.Types (HasOtherEnv, HasPSQL, desc)
import           Haxl.Core           (GenHaxl, memo, throw)
import           Haxl.Prelude        (NotFound (..), catchAny)


instance Alternative (GenHaxl u w) where
  a <|> b = catchAny a b
  empty = throw $ NotFound "mzero"

--  type Query {
--    file(key: String!): File
--    article(id: Int!): Article
--    article(alias: String!): Article
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
--    tags: [String]
--    aliases: [String]
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

getCache :: Text -> GenHaxl u w Value
getCache key = memo key (return Null)

schema :: (HasPSQL u, HasOtherEnv Cache u) => Schema (GenHaxl u w)
schema = file :| [article, articles, tag, timeline, articleCount, timelineCount, timelineMeta]

schemaByArticle :: (HasPSQL u, HasOtherEnv Cache u) => Value -> Value -> Article -> Schema (GenHaxl u w)
schemaByArticle aev fev art = fromList (article_ aev fev art)

file :: HasPSQL u => Resolver (GenHaxl u w)
file = objectA' "file" $ \argv -> do
  ev <- getCache "file_extra"
  case getText "key" argv of
    Nothing  -> empty
    Just key -> maybe [] (file_ ev) <$> getFileWithKey (unpack key)

file_ :: HasPSQL u => Value -> File -> [Resolver (GenHaxl u w)]
file_ ev File{..} =
  [ scalar "id"         fileID
  , scalar "key"        fileKey
  , scalar "bucket"     fileBucket
  , value  "extra"      $ union fileExtra ev
  , pick   "pick_extra" fileExtra
  , scalar "created_at" fileCreatedAt
  ]

article :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
article = objectA' "article" $ \argv -> do
  fev <- getCache "file_extra"
  aev <- getCache "article_extra"
  maid <- case getText "alias" argv of
            Nothing    -> pure $ getInt "id" argv
            Just alias -> getAlias alias

  case maid of
    Nothing    -> empty
    Just artId -> maybe [] (article_ aev fev) <$> getArticleById artId

article_ :: HasPSQL u => Value -> Value -> Article -> [Resolver (GenHaxl u w)]
article_ aev fev Article{..} =
  [ scalar "id"         artID
  , scalar "title"      artTitle
  , scalar "summary"    artSummary
  , scalar "content"    artContent
  , scalar "tags"       artTags
  , scalar "aliases"    artAliases
  , scalar "timelines"  artTimelines
  , object "cover"      (maybe [] (file_ fev) artCover)
  , value  "extra"      (artExtra `union` aev)
  , pick   "pick_extra" (artExtra `union` aev)
  , scalar "created_at" artCreatedAt
  ]

articles :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
articles = arrayA' "articles" $ \ argv -> do
  fev <- getCache "file_extra"
  aev <- getCache "article_extra"

  let from = fromMaybe 0  $ getInt "from" argv
      size = fromMaybe 10 $ getInt "size" argv

  map (article_ aev fev) <$> getArticleList from size (desc "id")

tag :: HasPSQL u => Resolver (GenHaxl u w)
tag = objectA' "tag" $ \argv -> do
  let tagId = getInt "id" argv
      name = getText "name" argv
  case (tagId, name) of
    (Nothing, Nothing) -> empty
    (Just tagId', _)   -> maybe [] tag_ <$> getTagById tagId'
    (_, Just name')    -> maybe [] tag_ <$> getTagByName (unpack name')

tag_ :: HasPSQL u => Tag -> [Resolver (GenHaxl u w)]
tag_ Tag{..} =
  [ scalar "id"         tagID
  , scalar "name"       tagName
  , scalar "created_at" tagCreatedAt
  ]

timeline :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
timeline = arrayA' "timeline" $ \ argv -> do
  let from = fromMaybe 0  $ getInt "from" argv
      size = fromMaybe 10 $ getInt "size" argv

  fev <- getCache "file_extra"
  aev <- getCache "article_extra"

  case getText "name" argv of
    Nothing   -> empty
    Just name ->
      map (article_ aev fev) <$> getArticleListByTimeline (unpack name) from size (desc "art_id")

articleCount :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
articleCount = scalarA "article_count" $ \ case
  [] -> countArticle
  _  -> empty

timelineCount :: (HasPSQL u, HasOtherEnv Cache u) => Resolver (GenHaxl u w)
timelineCount = scalarA "timeline_count" $ \ argv ->
  case getText "name" argv of
    Nothing   -> empty
    Just name -> countTimeline (unpack name)

timelineMeta :: HasPSQL u => Resolver (GenHaxl u w)
timelineMeta = objectA' "timeline_meta" $ \argv ->
  case getText "name" argv of
    Nothing   -> empty
    Just name -> maybe [] timelineMeta_ <$> getTimelineMeta (unpack name)

timelineMeta_ :: (Title, Summary) -> [Resolver (GenHaxl u w)]
timelineMeta_ (title, summary) =
  [ scalar "title"   title
  , scalar "summary" summary
  ]
