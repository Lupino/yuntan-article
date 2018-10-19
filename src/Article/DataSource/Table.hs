{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Table
  (
    mergeData
  ) where

import           Database.MySQL.Simple (execute_)
import           Yuntan.Types.HasMySQL (MySQL, VersionList, mergeDatabase)

import           Control.Monad         (void)
import           Data.String           (fromString)


createArticleTagTable :: MySQL ()
createArticleTagTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_article_tag` ("
                                  , "  `art_id` int(10) unsigned NOT NULL,"
                                  , "  `tag_id` int(10) unsigned NOT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`art_id`,`tag_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createArticleTable :: MySQL ()
createArticleTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_articles` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `title` varchar(32) NOT NULL,"
                                  , "  `summary` varchar(1500) NOT NULL,"
                                  , "  `content` longtext,"
                                  , "  `from_url` varchar(255) NOT NULL,"
                                  , "  `from_url_hash` varchar(128) NOT NULL,"
                                  , "  `cover` TEXT DEFAULT NULL,"
                                  , "  `extra` TEXT DEFAULT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `from_url_hash` (`from_url_hash`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createFileTable :: MySQL ()
createFileTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_files` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `key` varchar(128) NOT NULL,"
                                  , "  `bucket` varchar(32) DEFAULT NULL,"
                                  , "  `extra` TEXT DEFAULT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `key` (`key`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTagTable :: MySQL ()
createTagTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_tags` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `tag_name` (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTimelineTable :: MySQL ()
createTimelineTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_timeline` ("
                                  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `art_id` int(10) unsigned NOT NULL,"
                                  , "  `created_at` int(10) unsigned NOT NULL,"
                                  , "  PRIMARY KEY (`id`),"
                                  , "  UNIQUE KEY `timeline_name_art_id` (`name`,`art_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTimelineMetaTable :: MySQL ()
createTimelineMetaTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_timeline_meta` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `title` varchar(150) NOT NULL,"
                                  , "  `summary` varchar(1500) DEFAULT NULL,"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

updateTable_1511777472 :: MySQL ()
updateTable_1511777472 prefix conn =
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_articles`"
    , " MODIFY COLUMN `title` varchar(150) NOT NULL"
    ]

updateTable_1539394618 :: MySQL ()
updateTable_1539394618 prefix conn =
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_articles`"
    , " MODIFY COLUMN `content` longtext"
    ]

updateTable_1539394681 :: MySQL ()
updateTable_1539394681 prefix conn =
  void $ execute_ conn . fromString $ concat
    [ "ALTER TABLE `", prefix, "_articles`"
    , " MODIFY COLUMN `extra` longtext"
    ]

versionList :: VersionList
versionList =
  [ (1, [ createArticleTagTable
        , createArticleTable
        , createTagTable
        , createFileTable
        , createTimelineTable
        , createTimelineMetaTable
        ])
  , (2, [updateTable_1511777472])
  , (4, [updateTable_1539394618, updateTable_1539394681])
  ]

mergeData :: MySQL ()
mergeData = mergeDatabase versionList
