{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Table
  (
    createTable
  ) where

import           Database.MySQL.Simple (Connection, execute_)

import           Data.Int              (Int64)
import           Data.String           (fromString)

import           Article.Types


createArticleTag :: TablePrefix -> Connection -> IO Int64
createArticleTag prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_article_tag` ("
                                  , "`art_id` int(10) unsigned NOT NULL,"
                                  , "`tag_id` int(10) unsigned NOT NULL,"
                                  , "`created_at` int(10) unsigned NOT NULL,"
                                  , "PRIMARY KEY (`art_id`,`tag_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createArticle :: TablePrefix -> Connection -> IO Int64
createArticle prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_articles` ("
                                  , "`id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "`title` varchar(32) NOT NULL,"
                                  , "`summary` varchar(1500) NOT NULL,"
                                  , "`content` text,"
                                  , "`from_url` varchar(255) NOT NULL,"
                                  , "`from_url_hash` varchar(128) NOT NULL,"
                                  , "`cover` varchar(255) DEFAULT NULL,"
                                  , "`extra` varchar(1500) DEFAULT NULL,"
                                  , "`created_at` int(10) unsigned NOT NULL,"
                                  , "PRIMARY KEY (`id`),"
                                  , "UNIQUE KEY `from_url_hash` (`from_url_hash`),"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]
createFile :: TablePrefix -> Connection -> IO Int64
createFile prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_files` ("
                                  , "`id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "`key` varchar(128) NOT NULL,"
                                  , "`bucket` varchar(32) DEFAULT NULL,"
                                  , "`extra` varchar(450) DEFAULT NULL,"
                                  , "`created_at` int(10) unsigned NOT NULL,"
                                  , "PRIMARY KEY (`id`),"
                                  , "UNIQUE KEY `key` (`key`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTag :: TablePrefix -> Connection -> IO Int64
createTag prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_tags` ("
                                  , "`id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "`name` varchar(128) NOT NULL,"
                                  , "`created_at` int(10) unsigned NOT NULL,"
                                  , "PRIMARY KEY (`id`),"
                                  , "UNIQUE KEY `tag_name` (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTimeline :: TablePrefix -> Connection -> IO Int64
createTimeline prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_timeline` ("
                                  , "`id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
                                  , "`name` varchar(128) NOT NULL,"
                                  , "`art_id` int(10) unsigned NOT NULL,"
                                  , "`created_at` int(10) unsigned NOT NULL,"
                                  , "PRIMARY KEY (`id`),"
                                  , "UNIQUE KEY `timeline_name_art_id` (`name`,`art_id`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTimelineMeta :: TablePrefix -> Connection -> IO Int64
createTimelineMeta prefix conn = execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_timeline_meta` ("
                                  , "`name` varchar(128) NOT NULL,"
                                  , "`title` varchar(150) NOT NULL,"
                                  , "`summary` varchar(1500) DEFAULT NULL,"
                                  , "PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

createTable :: TablePrefix -> Connection -> IO Int64
createTable prefix conn = sum <$> mapM (\o -> o prefix conn) [ createArticleTag
                                                             , createArticle
                                                             , createTag
                                                             , createFile
                                                             , createTimeline
                                                             , createTimelineMeta
                                                             ]
