{-# LANGUAGE OverloadedStrings #-}

module Article.DataSource.Timeline
  ( addTimeline
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

import           Article.DataSource.Table (timeline, timelineMeta)
import           Article.Types
import           Control.Monad.IO.Class   (liftIO)
import           Data.Int                 (Int64)
import           Data.UnixTime
import           Database.PSQL.Types      (From, Only (..), OrderBy, PSQL, Size,
                                           count, delete, insertOrUpdate, none,
                                           selectOne, selectOnly)

addTimeline :: String -> ID -> PSQL ID
addTimeline name aid = do
  t <- liftIO getUnixTime
  insertOrUpdate timeline ["name", "art_id"] [] ["created_at"] (name, aid, show $ toEpochTime t)

removeTimeline :: String -> ID -> PSQL Int64
removeTimeline name aid =
  delete timeline "name = ? AND art_id = ?" (name, aid)

removeTimelineList :: String -> PSQL Int64
removeTimelineList name =
  delete timeline "name = ? " (Only name)

removeTimelineListById :: ID -> PSQL Int64
removeTimelineListById aid =
  delete timeline "art_id = ? " (Only aid)

getIdListByTimeline :: String -> From -> Size -> OrderBy -> PSQL [ID]
getIdListByTimeline name =
  selectOnly timeline "art_id" "name = ?" (Only name)

countTimeline :: String -> PSQL Int64
countTimeline name =
  count timeline "name = ?" (Only name)

getTimelineListById :: ID -> PSQL [String]
getTimelineListById aid =
  selectOnly timeline "name" "art_id = ?" (Only aid) 0 100 none

saveTimelineMeta :: String -> Title -> Summary -> PSQL Int64
saveTimelineMeta name title summary =
  insertOrUpdate timelineMeta ["name"] ["title", "summary"] [] (name, title, summary)

getTimelineMeta :: String -> PSQL (Maybe (Title, Summary))
getTimelineMeta name =
  selectOne timelineMeta ["title", "summary"] "name = ?" (Only name)

removeTimelineMeta :: String -> PSQL Int64
removeTimelineMeta name =
  delete timelineMeta "name = ?" (Only name)
