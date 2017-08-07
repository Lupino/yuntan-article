{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Timeline
  (
    addTimeline
  , removeTimeline
  , removeAllTimeline
  , removeAllTimelineByArtId
  , getAllTimeline
  , countTimeline
  , getAllArticleTimeline
  , saveTimelineMeta
  , removeTimelineMeta
  , getTimelineMeta
  ) where

import           Article.Types
import           Control.Monad             (void)
import           Data.Int                  (Int64)
import           Data.Maybe                (listToMaybe)
import           Data.String               (fromString)
import           Data.UnixTime
import           Database.MySQL.Simple     (Connection, Only (..), execute,
                                            insertID, query)
import           Dispatch.Types.ListResult (From, Size)
import           Dispatch.Types.OrderBy    (OrderBy)

addTimeline :: String -> ID -> TablePrefix -> Connection -> IO ID
addTimeline name aid prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, aid, show $ toEpochTime t)
  fromIntegral <$> insertID conn
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_timeline` (`name`, `art_id`, `created_at`) values ( ?, ?, ? )" ]

removeTimeline :: String -> ID -> TablePrefix -> Connection -> IO Int64
removeTimeline name aid prefix conn = execute conn sql (name, aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `name` = ? AND `art_id` = ?" ]

removeAllTimeline :: String -> TablePrefix -> Connection -> IO Int64
removeAllTimeline name prefix conn = execute conn sql (Only name)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `name` = ?" ]

removeAllTimelineByArtId :: ID -> TablePrefix -> Connection -> IO Int64
removeAllTimelineByArtId aid prefix conn = execute conn sql (Only aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `art_id` = ?" ]

getAllTimeline :: String -> From -> Size -> OrderBy -> TablePrefix -> Connection -> IO [Article]
getAllTimeline name f s o prefix conn = query conn sql (name, f, s)
  where sql = fromString $ concat [ "SELECT a.* FROM `", prefix, "_articles` AS a"
                                  , " LEFT JOIN `", prefix, "_timeline` AS t ON t.`art_id` = a.`id` "
                                  , " WHERE t.`name`=? "
                                  , show o
                                  , " LIMIT ?,?"
                                  ]

countTimeline :: String -> TablePrefix -> Connection -> IO Int64
countTimeline name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_timeline` WHERE `name`=?" ]

getAllArticleTimeline :: ID -> TablePrefix -> Connection -> IO [String]
getAllArticleTimeline aid prefix conn = map fromOnly <$> query conn sql (Only aid)
  where sql = fromString $ concat [ "SELECT `name` FROM `", prefix, "_timeline` WHERE `art_id` = ?" ]

saveTimelineMeta :: String -> Title -> Summary -> TablePrefix -> Connection -> IO Int64
saveTimelineMeta name title summary prefix conn = execute conn sql (name, title, summary)
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_timeline_meta` (`name`, `title`, `summary`) values (?, ?, ?)" ]

getTimelineMeta :: String -> TablePrefix -> Connection -> IO (Maybe (Title, Summary))
getTimelineMeta name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `title`, `summary` FROM `", prefix, "_timeline_meta` WHERE `name`=?" ]

removeTimelineMeta :: String -> TablePrefix -> Connection -> IO Int64
removeTimelineMeta name prefix conn = execute conn sql (Only name)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline_meta` WHERE `name`=?" ]
