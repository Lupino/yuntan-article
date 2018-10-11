{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Timeline
  (
    addTimeline
  , removeTimeline
  , removeTimelineList
  , removeTimelineListById
  , getIdListByTimeline
  , countTimeline
  , getTimelineListById
  , saveTimelineMeta
  , removeTimelineMeta
  , getTimelineMeta
  ) where

import           Article.Types
import           Control.Monad           (void)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.String             (fromString)
import           Data.UnixTime
import           Database.MySQL.Simple   (Only (..), execute, insertID, query)
import           Yuntan.Types.HasMySQL   (MySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

addTimeline :: String -> ID -> MySQL ID
addTimeline name aid prefix conn = do
  t <- getUnixTime
  void $ execute conn sql (name, aid, show $ toEpochTime t)
  fromIntegral <$> insertID conn
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_timeline` (`name`, `art_id`, `created_at`) values ( ?, ?, ? )" ]

removeTimeline :: String -> ID -> MySQL Int64
removeTimeline name aid prefix conn = execute conn sql (name, aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `name` = ? AND `art_id` = ?" ]

removeTimelineList :: String -> MySQL Int64
removeTimelineList name prefix conn = execute conn sql (Only name)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `name` = ?" ]

removeTimelineListById :: ID -> MySQL Int64
removeTimelineListById aid prefix conn = execute conn sql (Only aid)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline` WHERE `art_id` = ?" ]

getIdListByTimeline :: String -> From -> Size -> OrderBy -> MySQL [ID]
getIdListByTimeline name f s o prefix conn = map fromOnly <$> query conn sql (name, f, s)
  where sql = fromString $ concat [ "SELECT `art_id` FROM `", prefix, "_timeline`"
                                  , " WHERE `name`=? "
                                  , show o
                                  , " LIMIT ?,?"
                                  ]

countTimeline :: String -> MySQL Int64
countTimeline name prefix conn = maybe 0 fromOnly . listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT count(*) FROM `", prefix, "_timeline` WHERE `name`=?" ]

getTimelineListById :: ID -> MySQL [String]
getTimelineListById aid prefix conn = map fromOnly <$> query conn sql (Only aid)
  where sql = fromString $ concat [ "SELECT `name` FROM `", prefix, "_timeline` WHERE `art_id` = ?" ]

saveTimelineMeta :: String -> Title -> Summary -> MySQL Int64
saveTimelineMeta name title summary prefix conn = execute conn sql (name, title, summary)
  where sql = fromString $ concat [ "REPLACE INTO `", prefix, "_timeline_meta` (`name`, `title`, `summary`) values (?, ?, ?)" ]

getTimelineMeta :: String -> MySQL (Maybe (Title, Summary))
getTimelineMeta name prefix conn = listToMaybe <$> query conn sql (Only name)
  where sql = fromString $ concat [ "SELECT `title`, `summary` FROM `", prefix, "_timeline_meta` WHERE `name`=?" ]

removeTimelineMeta :: String -> MySQL Int64
removeTimelineMeta name prefix conn = execute conn sql (Only name)
  where sql = fromString $ concat [ "DELETE FROM `", prefix, "_timeline_meta` WHERE `name`=?" ]
