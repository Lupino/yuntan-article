{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.API
  (
    getArticleById
  , createArticle
  , updateArticle
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent
  , updateArticleCover
  , updateArticleExtra
  , getAllArticle
  , countAllArticle
  , removeArticle
  , existsArticle

  , saveFile
  , saveFileWithExtra
  , getFileWithKey
  , getFileById
  , getFiles

  , createArticleAndFetch

  , toCardItem

  , addTag
  , getTagById
  , getTagByName
  , getTags
  , updateTag
  , addArticleTag
  , removeArticleTag
  , removeAllArticleTag
  , getAllArticleTagName

  , addTimeline
  , removeTimeline
  , removeAllTimeline
  , removeAllTimelineByArtId
  , getAllTimeline
  , countTimeline
  , getAllArticleTimeline
  , saveTimelineMeta
  , removeTimelineMeta
  , getTimelineMeta
  , mergeData
  ) where

import           Article.Types

import           Article.DataSource      (ArticleReq (..))
import           Data.Traversable        (for)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           Yuntan.Extra.Config     (ConfigLru, fillValue_)
import           Yuntan.Types.HasMySQL   (HasMySQL, HasOtherEnv, otherEnv)

import           Data.Int                (Int64)
import           Data.Maybe              (isJust)

import           Article.Utils           (cleanHtml, firstImage)
import qualified Data.Text               as T (length)

import           Data.Aeson              (Value)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

import           System.FilePath         (takeFileName)

getArticleById :: (HasMySQL u, HasOtherEnv ConfigLru u) => ID -> GenHaxl u (Maybe Article)
getArticleById artId = do
  mart <- dataFetch (GetArticleById artId)
  case mart of
    Just art -> fmap Just (fillArticleExtra =<< fillAllTimeline =<< fillArticleCover =<< fillAllTagName art)
    Nothing  -> return Nothing


createArticle :: HasMySQL u => Title -> Summary -> Content -> FromURL -> CreatedAt -> GenHaxl u ID
createArticle t s co f c = uncachedRequest (CreateArticle t s co f c)

updateArticle :: HasMySQL u => ID -> Title -> Summary -> Content -> GenHaxl u Int64
updateArticle artId t s c = uncachedRequest (UpdateArticle artId t s c)

updateArticleTitle :: HasMySQL u => ID -> Title -> GenHaxl u Int64
updateArticleTitle artId t = uncachedRequest (UpdateArticleTitle artId t)

updateArticleSummary :: HasMySQL u => ID -> Summary -> GenHaxl u Int64
updateArticleSummary artId s = uncachedRequest (UpdateArticleSummary artId s)

updateArticleContent :: HasMySQL u => ID -> Content -> GenHaxl u Int64
updateArticleContent artId c = uncachedRequest (UpdateArticleContent artId c)

updateArticleCover :: HasMySQL u => ID -> Maybe File -> GenHaxl u Int64
updateArticleCover artId cover = uncachedRequest (UpdateArticleCover artId cover)

fillArticleCover :: HasMySQL u => Article -> GenHaxl u Article
fillArticleCover art = do
  file <- if isJust cover then return cover else getFileWithKey key
  return art { artCover = file }

  where cover = artCover art
        key   = takeFileKey . firstImage $ artSummary art

updateArticleExtra :: HasMySQL u => ID -> Value -> GenHaxl u Int64
updateArticleExtra artId extra = uncachedRequest (UpdateArticleExtra artId extra)

getAllArticle
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => From -> Size -> OrderBy -> GenHaxl u [Article]
getAllArticle f s o =do
  arts <- dataFetch (GetAllArticle f s o)
  for arts $ \art -> fillArticleExtra =<< fillAllTimeline =<< fillArticleCover =<< fillAllTagName art

countAllArticle :: HasMySQL u => GenHaxl u Int64
countAllArticle = dataFetch CountAllArticle

removeArticle :: HasMySQL u => ID -> GenHaxl u Int64
removeArticle artId = uncachedRequest (RemoveArticle artId)

existsArticle :: HasMySQL u => FromURL -> GenHaxl u (Maybe ID)
existsArticle u = dataFetch (ExistsArticle u)

saveFile :: HasMySQL u => FileBucket -> FileKey -> GenHaxl u (Maybe File)
saveFile path fc = uncachedRequest (SaveFile path fc)


saveFileWithExtra :: HasMySQL u => FileBucket -> FileKey -> FileExtra -> GenHaxl u (Maybe File)
saveFileWithExtra path fc extra = uncachedRequest (SaveFileWithExtra path fc extra)

getFileWithKey :: HasMySQL u => FileKey -> GenHaxl u (Maybe File)
getFileWithKey k = dataFetch (GetFileWithKey k)

getFileById :: HasMySQL u => ID -> GenHaxl u (Maybe File)
getFileById fileId = dataFetch (GetFileById fileId)

getFiles :: HasMySQL u => [ID] -> GenHaxl u [File]
getFiles ids = dataFetch (GetFiles ids)

createArticleAndFetch
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => Title -> Summary -> Content -> FromURL -> CreatedAt -> GenHaxl u (Maybe Article)
createArticleAndFetch t s co f c = getArticleById =<< createArticle t s co f c

toCardItem :: HasMySQL u => Article -> GenHaxl u CardItem
toCardItem art = do
  file <- if isJust cover then return cover else getFileWithKey key
  return CardItem { cardID         = cid
                  , cardTitle      = title
                  , cardSummary    = summary
                  , cardImage      = file
                  , cardURL        = uri
                  , cardTags       = tags
                  , cardExtra      = extra
                  , cardCreatedAt  = created
                  }

  where cid           = artID art
        title         = take 10 $ artTitle art
        summary       = take 50 $ cleanHtml $ artSummary art
        contentLength = T.length $ artContent art
        uri | contentLength > 10 = "/t/" ++ show cid ++ ".html"
            | otherwise          = artFromURL art


        key     = takeFileKey . firstImage $ artSummary art
        cover   = artCover art
        tags    = artTags art
        extra   = artExtra art
        created = artCreatedAt art

takeFileKey :: String -> String
takeFileKey = takeWhile (/= '.') . takeFileName . takeWhile (/= '?')

addTag               :: HasMySQL u => TagName -> GenHaxl u ID
getTagById           :: HasMySQL u => ID -> GenHaxl u (Maybe Tag)
getTagByName         :: HasMySQL u => TagName -> GenHaxl u (Maybe Tag)
getTags              :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [Tag]
updateTag            :: HasMySQL u => ID -> TagName -> GenHaxl u Int64
addArticleTag        :: HasMySQL u => ID -> ID -> GenHaxl u ID
removeArticleTag     :: HasMySQL u => ID -> ID -> GenHaxl u Int64
removeAllArticleTag  :: HasMySQL u => ID -> GenHaxl u Int64
getAllArticleTagName :: HasMySQL u => ID -> GenHaxl u [TagName]

addTag name              = uncachedRequest (AddTag name)
getTagById tid           = dataFetch (GetTagById tid)
getTagByName name        = dataFetch (GetTagByName name)
getTags from size o      = dataFetch (GetTags from size o)
updateTag tid name       = uncachedRequest (UpdateTag tid name)
addArticleTag aid tid    = uncachedRequest (AddArticleTag aid tid)
removeArticleTag aid tid = uncachedRequest (RemoveArticleTag aid tid)
removeAllArticleTag aid  = uncachedRequest (RemoveAllArticleTag aid)
getAllArticleTagName aid = dataFetch (GetAllArticleTagName aid)

fillAllTagName :: HasMySQL u => Article -> GenHaxl u Article
fillAllTagName art = do
  tags <- getAllArticleTagName $ artID art
  return $ art { artTags = tags }

addTimeline              :: HasMySQL u => String -> ID -> GenHaxl u ID
removeTimeline           :: HasMySQL u => String -> ID -> GenHaxl u Int64
removeAllTimeline        :: HasMySQL u => String -> GenHaxl u Int64
removeAllTimelineByArtId :: HasMySQL u => ID -> GenHaxl u Int64
getAllTimeline
  :: (HasMySQL u, HasOtherEnv ConfigLru u)
  => String -> From -> Size -> OrderBy -> GenHaxl u [Article]
countTimeline            :: HasMySQL u => String -> GenHaxl u Int64
getAllArticleTimeline    :: HasMySQL u => ID -> GenHaxl u [String]
saveTimelineMeta         :: HasMySQL u => String -> Title -> Summary -> GenHaxl u Int64
removeTimelineMeta       :: HasMySQL u => String -> GenHaxl u Int64
getTimelineMeta          :: HasMySQL u => String -> GenHaxl u (Maybe (Title, Summary))

addTimeline name aid         = uncachedRequest (AddTimeline name aid)
removeTimeline name aid      = uncachedRequest (RemoveTimeline name aid)
removeAllTimeline name       = uncachedRequest (RemoveAllTimeline name)
removeAllTimelineByArtId aid = uncachedRequest (RemoveAllTimelineByArtId aid)
getAllTimeline name f s o    = do
  arts <- dataFetch (GetAllTimeline name f s o)
  for arts $ \art -> fillArticleExtra =<< fillAllTimeline =<< fillArticleCover =<< fillAllTagName art

countTimeline name           = dataFetch (CountTimeline name)
getAllArticleTimeline aid    = dataFetch (GetAllArticleTimeline aid)
saveTimelineMeta name t s    = uncachedRequest (SaveTimelineMeta name t s)
removeTimelineMeta name      = uncachedRequest (RemoveTimelineMeta name)
getTimelineMeta name         = dataFetch (GetTimelineMeta name)

fillAllTimeline :: HasMySQL u => Article -> GenHaxl u Article
fillAllTimeline art = do
  timelines <- getAllArticleTimeline $ artID art
  return $ art { artTimelines = timelines }

mergeData :: HasMySQL u => GenHaxl u ()
mergeData = uncachedRequest MergeData

fillArticleExtra :: (HasMySQL u, HasOtherEnv ConfigLru u) => Article -> GenHaxl u Article
fillArticleExtra = fillValue_ otherEnv "article-extra" artExtra update
  where update :: Value -> Article -> Article
        update v u = u {artExtra = v}
