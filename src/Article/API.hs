{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.API
  ( getArticleById
  , updateArticle
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent
  , updateArticleCover
  , updateArticleExtra
  , removeArticle
  , getArticleList
  , countArticle

  , createArticleAndFetch

  , toCardItem

  , addArticleTag
  , removeArticleTag
  , removeAllArticleTag

  , addTimeline
  , removeTimeline
  , removeTimelineListById
  , getArticleListByTimeline
  , countTimeline
  , module X
  ) where

import           Article.Config      (Cache, redisEnv)
import           Article.RawAPI      as X (addTag, getAllArticleTagName,
                                           getArticleIdList, getFileById,
                                           getFileWithKey, getTagById,
                                           getTagByName, getTimelineMeta,
                                           mergeData, removeTimelineMeta,
                                           saveFile, saveFileWithExtra,
                                           saveTimelineMeta, updateTag)
import qualified Article.RawAPI      as RawAPI
import           Article.Types
import           Article.Utils       (cleanHtml, firstImage)
import           Data.Aeson          (Value)
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)
import           Data.Maybe          (catMaybes, isJust)
import           Data.String         (fromString)
import           Data.Traversable    (for)
import           Database.PSQL.Types (From, HasOtherEnv, HasPSQL, OrderBy, Size)
import           Haxl.Core           (GenHaxl)
import           Haxl.RedisCache     (cached, cached', remove)
import           System.FilePath     (takeFileName)

genArticleKey :: ID -> ByteString
genArticleKey id0 = fromString $ "article:" ++ show id0

genCountKey :: String -> ByteString
genCountKey k = fromString $ "count:" ++ k

($>) :: GenHaxl u w a -> GenHaxl u w () -> GenHaxl u w a
io $> a = do
  !r <- io
  !_ <- a
  return r

unCacheCount :: HasOtherEnv Cache u => String -> GenHaxl u w a -> GenHaxl u w a
unCacheCount k io = io $> remove redisEnv (genCountKey k)

unCacheTimelineCount :: HasOtherEnv Cache u => [String] -> GenHaxl u w ()
unCacheTimelineCount = mapM_ (\k -> remove redisEnv (genCountKey ("timeline:" ++ k)))

unCacheArticle :: HasOtherEnv Cache u => ID -> GenHaxl u w a -> GenHaxl u w a
unCacheArticle id0 io = io $> remove redisEnv (genArticleKey id0)

createArticle
  :: (HasPSQL u, HasOtherEnv Cache u)
  => Title -> Summary -> Content -> CreatedAt -> GenHaxl u w ID
createArticle t s c ct =
  unCacheCount "article" $ RawAPI.createArticle t s c ct

getArticleById :: (HasPSQL u, HasOtherEnv Cache u) => ID -> GenHaxl u w (Maybe Article)
getArticleById artId =
  cached redisEnv (genArticleKey artId) $
    fillArticle =<< RawAPI.getArticleById artId

fillArticle :: (HasPSQL u, HasOtherEnv Cache u) => Maybe Article -> GenHaxl u w (Maybe Article)
fillArticle Nothing = return Nothing
fillArticle (Just art) =
  fmap Just
    $ fillAllTimeline
    =<< fillArticleCover
    =<< fillAllTagName art

updateArticle :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Title -> Summary -> Content -> GenHaxl u w Int64
updateArticle artId t s c = unCacheArticle artId $ RawAPI.updateArticle artId t s c

updateArticleTitle :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Title -> GenHaxl u w Int64
updateArticleTitle artId t = unCacheArticle artId $ RawAPI.updateArticleTitle artId t

updateArticleSummary :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Summary -> GenHaxl u w Int64
updateArticleSummary artId s = unCacheArticle artId $ RawAPI.updateArticleSummary artId s

updateArticleContent :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Content -> GenHaxl u w Int64
updateArticleContent artId c = unCacheArticle artId $ RawAPI.updateArticleContent artId c

updateArticleCover :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Maybe File -> GenHaxl u w Int64
updateArticleCover artId cover = unCacheArticle artId $ RawAPI.updateArticleCover artId cover

fillArticleCover :: HasPSQL u => Article -> GenHaxl u w Article
fillArticleCover art = do
  file <- if isJust cover then return cover else getFileWithKey key
  return art { artCover = file }

  where cover = artCover art
        key   = takeFileKey . firstImage $ artSummary art

updateArticleExtra :: (HasPSQL u, HasOtherEnv Cache u) => ID -> Value -> GenHaxl u w Int64
updateArticleExtra artId extra = unCacheArticle artId $ RawAPI.updateArticleExtra artId extra

getArticleList
  :: (HasPSQL u, HasOtherEnv Cache u)
  => From -> Size -> OrderBy -> GenHaxl u w [Article]
getArticleList f s o = do
  aids <- RawAPI.getArticleIdList f s o
  catMaybes <$> for aids getArticleById

countArticle :: (HasPSQL u, HasOtherEnv Cache u) => GenHaxl u w Int64
countArticle = cached' redisEnv (genCountKey "article") RawAPI.countArticle

removeArticle :: (HasPSQL u, HasOtherEnv Cache u) => ID -> GenHaxl u w Int64
removeArticle artId =
  unCacheArticle artId $ unCacheCount "article" $ RawAPI.removeArticle artId

createArticleAndFetch
  :: (HasPSQL u, HasOtherEnv Cache u)
  => Title -> Summary -> Content -> CreatedAt -> GenHaxl u w (Maybe Article)
createArticleAndFetch t s co c = getArticleById =<< createArticle t s co c

toCardItem :: HasPSQL u => Article -> GenHaxl u w CardItem
toCardItem art = do
  file <- if isJust cover then return cover else getFileWithKey key
  return CardItem { cardID         = cid
                  , cardTitle      = title
                  , cardSummary    = summary
                  , cardImage      = file
                  , cardTags       = tags
                  , cardExtra      = extra
                  , cardCreatedAt  = created
                  }

  where cid           = artID art
        title         = take 10 $ artTitle art
        summary       = take 50 $ cleanHtml $ artSummary art

        key     = takeFileKey . firstImage $ artSummary art
        cover   = artCover art
        tags    = artTags art
        extra   = artExtra art
        created = artCreatedAt art

takeFileKey :: String -> String
takeFileKey = takeWhile (/= '.') . takeFileName . takeWhile (/= '?')

addArticleTag :: (HasPSQL u, HasOtherEnv Cache u) => ID -> ID -> GenHaxl u w ID
addArticleTag aid tid  = unCacheArticle aid $ RawAPI.addArticleTag aid tid

removeArticleTag :: (HasPSQL u, HasOtherEnv Cache u) => ID -> ID -> GenHaxl u w Int64
removeArticleTag aid tid = unCacheArticle aid $ RawAPI.removeArticleTag aid tid

removeAllArticleTag  :: (HasPSQL u, HasOtherEnv Cache u) => ID -> GenHaxl u w Int64
removeAllArticleTag aid  = unCacheArticle aid $ RawAPI.removeAllArticleTag aid

fillAllTagName :: HasPSQL u => Article -> GenHaxl u w Article
fillAllTagName art = do
  tags <- RawAPI.getAllArticleTagName $ artID art
  return $ art { artTags = tags }

addTimeline :: (HasPSQL u, HasOtherEnv Cache u) => String -> ID -> GenHaxl u w ID
addTimeline name aid =
  unCacheArticle aid $ unCacheCount ("timeline:" ++ name) $ RawAPI.addTimeline name aid

removeTimeline :: (HasPSQL u, HasOtherEnv Cache u) => String -> ID -> GenHaxl u w Int64
removeTimeline name aid =
  unCacheArticle aid $ unCacheCount ("timeline:" ++ name) $ RawAPI.removeTimeline name aid

removeTimelineListById :: (HasPSQL u, HasOtherEnv Cache u) => ID -> GenHaxl u w Int64
removeTimelineListById aid = unCacheArticle aid $ do
  names <- RawAPI.getTimelineListById aid
  r <- RawAPI.removeTimelineListById aid
  unCacheTimelineCount names
  pure r

getArticleListByTimeline
  :: (HasPSQL u, HasOtherEnv Cache u)
  => String -> From -> Size -> OrderBy -> GenHaxl u w [Article]
getArticleListByTimeline name f s o    = do
  aids <- RawAPI.getIdListByTimeline name f s o
  catMaybes <$> for aids getArticleById

countTimeline :: (HasPSQL u, HasOtherEnv Cache u) => String -> GenHaxl u w Int64
countTimeline name =
  cached' redisEnv (genCountKey ("timeline:" ++ name)) $ RawAPI.countTimeline name

fillAllTimeline :: HasPSQL u => Article -> GenHaxl u w Article
fillAllTimeline art = do
  timelines <- RawAPI.getTimelineListById $ artID art
  return $ art { artTimelines = timelines }
