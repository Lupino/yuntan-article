{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Table
  ( mergeData

  -- tables
  , articles
  , articleTag
  , articleAlias
  , files
  , tags
  , timeline
  , timelineMeta
  ) where

import           Data.Int            (Int64)
import           Database.PSQL.Types (PSQL, TableName, VersionList,
                                      constraintPrimaryKey, createIndex,
                                      createTable, getTablePrefix,
                                      mergeDatabase)

articles :: TableName
articles = "articles"

articleTag :: TableName
articleTag = "article_tag"

articleAlias :: TableName
articleAlias = "article_alias"

files :: TableName
files = "files"

tags :: TableName
tags = "tags"

timeline :: TableName
timeline = "timeline"

timelineMeta :: TableName
timelineMeta = "timeline_meta"

createArticleTagTable :: PSQL Int64
createArticleTagTable = do
  prefix <- getTablePrefix
  createTable articleTag
    [ "art_id INT NOT NULL"
    , "tag_id INT NOT NULL"
    , "created_at INT DEFAULT '0'"
    , constraintPrimaryKey prefix articleTag ["art_id", "tag_id"]
    ]

createArticleAliasTable :: PSQL Int64
createArticleAliasTable =
  createTable articleAlias
    [ "alias VARCHAR(128) PRIMARY KEY"
    , "id INT NOT NULL"
    ]


createArticleTable :: PSQL Int64
createArticleTable =
  createTable articles
    [ "id SERIAL PRIMARY KEY"
    , "title VARCHAR(150) NOT NULL"
    , "summary VARCHAR(1500) NOT NULL"
    , "content TEXT"
    , "cover JSON"
    , "extra JSON"
    , "created_at INT DEFAULT '0'"
    ]


createFileTable :: PSQL Int64
createFileTable =
  createTable files
    [ "id SERIAL PRIMARY KEY"
    , "key VARCHAR(128) NOT NULL"
    , "bucket VARCHAR(32) NOT NULL"
    , "extra JSON"
    , "created_at INT DEFAULT '0'"
    ]

createTagTable :: PSQL Int64
createTagTable =
  createTable tags
    [ "id SERIAL PRIMARY KEY"
    , "name VARCHAR(128) NOT NULL"
    , "created_at INT DEFAULT '0'"
    ]


createTimelineTable :: PSQL Int64
createTimelineTable =
  createTable timeline
    [ "id SERIAL PRIMARY KEY"
    , "name VARCHAR(128) NOT NULL"
    , "art_id INT NOT NULL"
    , "created_at INT DEFAULT '0'"
    ]


createTimelineMetaTable :: PSQL Int64
createTimelineMetaTable =
  createTable timelineMeta
    [ "name VARCHAR(128) PRIMARY KEY"
    , "title VARCHAR(150) NOT NULL"
    , "summary VARCHAR(1500) DEFAULT NULL"
    ]

versionList :: VersionList Int64
versionList =
  [ (1, [ createArticleTagTable
        , createArticleTable
        -- , createIndex True articles "from_url_hash" ["from_url_hash"]
        , createFileTable
        , createIndex True files "key" ["key"]
        , createTagTable
        , createIndex True tags "name" ["name"]
        , createTimelineTable
        , createIndex True timeline "name_art_id" ["name", "art_id"]
        , createTimelineMetaTable
        ])
  , (2, [ createArticleAliasTable
        , createIndex False articleAlias "id" ["id"]
        ])
  ]

mergeData :: PSQL ()
mergeData = mergeDatabase versionList
