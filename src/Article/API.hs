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

  , uploadFile
  , uploadFileWithExtra
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
  , createTable
  ) where

import           Article.Types

import           Article.DataSource      (ArticleReq (..))
import           Data.Traversable        (for)
import           Haxl.Core               (dataFetch, uncachedRequest)

import           Data.Int                (Int64)
import           Data.Maybe              (isJust)

import           Article.Utils           (cleanHtml, firstImage)
import qualified Data.Text               as T (length)

import           Data.ByteString.Lazy    (ByteString)

import           Article.UserEnv         (ArticleM)
import           Data.Aeson              (Value)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

import           System.FilePath         (takeFileName)

getArticleById :: ID -> ArticleM (Maybe Article)
getArticleById artId = do
  mart <- dataFetch (GetArticleById artId)
  case mart of
    Just art -> return . Just =<< fillAllTimeline =<< fillArticleCover =<< fillAllTagName art
    Nothing  -> return Nothing


createArticle :: Title -> Summary -> Content -> FromURL -> CreatedAt -> ArticleM ID
createArticle t s co f c = uncachedRequest (CreateArticle t s co f c)

updateArticle :: ID -> Title -> Summary -> Content -> ArticleM Int64
updateArticle artId t s c = uncachedRequest (UpdateArticle artId t s c)

updateArticleTitle :: ID -> Title -> ArticleM Int64
updateArticleTitle artId t = uncachedRequest (UpdateArticleTitle artId t)

updateArticleSummary :: ID -> Summary -> ArticleM Int64
updateArticleSummary artId s = uncachedRequest (UpdateArticleSummary artId s)

updateArticleContent :: ID -> Content -> ArticleM Int64
updateArticleContent artId c = uncachedRequest (UpdateArticleContent artId c)

updateArticleCover :: ID -> Maybe File -> ArticleM Int64
updateArticleCover artId cover = uncachedRequest (UpdateArticleCover artId cover)

fillArticleCover :: Article -> ArticleM Article
fillArticleCover art = do
  file <- if isJust cover then return cover else getFileWithKey key
  return art { artCover = file }

  where cover = artCover art
        key   = takeFileKey . firstImage $ artSummary art

updateArticleExtra :: ID -> Value -> ArticleM Int64
updateArticleExtra artId extra = uncachedRequest (UpdateArticleExtra artId extra)

getAllArticle :: From -> Size -> OrderBy -> ArticleM [Article]
getAllArticle f s o =do
  arts <- dataFetch (GetAllArticle f s o)
  for arts $ \art -> do
    fillAllTimeline =<< fillArticleCover =<< fillAllTagName art

countAllArticle :: ArticleM Int64
countAllArticle = dataFetch CountAllArticle

removeArticle :: ID -> ArticleM Int64
removeArticle artId = uncachedRequest (RemoveArticle artId)

existsArticle :: FromURL -> ArticleM (Maybe ID)
existsArticle u = dataFetch (ExistsArticle u)

uploadFile :: FileBucket -> ByteString -> ArticleM (Maybe File)
uploadFile path fc = uncachedRequest (UploadFile path fc)


uploadFileWithExtra :: FileBucket -> ByteString -> FileExtra -> ArticleM (Maybe File)
uploadFileWithExtra path fc extra = uncachedRequest (UploadFileWithExtra path fc extra)

getFileWithKey :: FileKey -> ArticleM (Maybe File)
getFileWithKey k = dataFetch (GetFileWithKey k)

getFileById :: ID -> ArticleM (Maybe File)
getFileById fileId = dataFetch (GetFileById fileId)

getFiles :: [ID] -> ArticleM [File]
getFiles ids = dataFetch (GetFiles ids)

createArticleAndFetch :: Title -> Summary -> Content -> FromURL -> CreatedAt -> ArticleM (Maybe Article)
createArticleAndFetch t s co f c = do
  getArticleById =<< createArticle t s co f c

toCardItem :: Article -> ArticleM CardItem
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

addTag               :: TagName -> ArticleM ID
getTagById           :: ID -> ArticleM (Maybe Tag)
getTagByName         :: TagName -> ArticleM (Maybe Tag)
getTags              :: From -> Size -> OrderBy -> ArticleM [Tag]
updateTag            :: ID -> TagName -> ArticleM Int64
addArticleTag        :: ID -> ID -> ArticleM ID
removeArticleTag     :: ID -> ID -> ArticleM Int64
removeAllArticleTag  :: ID -> ArticleM Int64
getAllArticleTagName :: ID -> ArticleM [TagName]

addTag name              = uncachedRequest (AddTag name)
getTagById tid           = dataFetch (GetTagById tid)
getTagByName name        = dataFetch (GetTagByName name)
getTags from size o      = dataFetch (GetTags from size o)
updateTag tid name       = uncachedRequest (UpdateTag tid name)
addArticleTag aid tid    = uncachedRequest (AddArticleTag aid tid)
removeArticleTag aid tid = uncachedRequest (RemoveArticleTag aid tid)
removeAllArticleTag aid  = uncachedRequest (RemoveAllArticleTag aid)
getAllArticleTagName aid = dataFetch (GetAllArticleTagName aid)

fillAllTagName :: Article -> ArticleM Article
fillAllTagName art = do
  tags <- getAllArticleTagName $ artID art
  return $ art { artTags = tags }

addTimeline              :: String -> ID -> ArticleM ID
removeTimeline           :: String -> ID -> ArticleM Int64
removeAllTimeline        :: String -> ArticleM Int64
removeAllTimelineByArtId :: ID -> ArticleM Int64
getAllTimeline           :: String -> From -> Size -> OrderBy -> ArticleM [Article]
countTimeline            :: String -> ArticleM Int64
getAllArticleTimeline    :: ID -> ArticleM [String]
saveTimelineMeta         :: String -> Title -> Summary -> ArticleM Int64
removeTimelineMeta       :: String -> ArticleM Int64
getTimelineMeta          :: String -> ArticleM (Maybe (Title, Summary))

addTimeline name aid         = uncachedRequest (AddTimeline name aid)
removeTimeline name aid      = uncachedRequest (RemoveTimeline name aid)
removeAllTimeline name       = uncachedRequest (RemoveAllTimeline name)
removeAllTimelineByArtId aid = uncachedRequest (RemoveAllTimelineByArtId aid)
getAllTimeline name f s o    = do
  arts <- dataFetch (GetAllTimeline name f s o)
  for arts $ \art -> do
    fillAllTimeline =<< fillArticleCover =<< fillAllTagName art

countTimeline name           = dataFetch (CountTimeline name)
getAllArticleTimeline aid    = dataFetch (GetAllArticleTimeline aid)
saveTimelineMeta name t s    = uncachedRequest (SaveTimelineMeta name t s)
removeTimelineMeta name      = uncachedRequest (RemoveTimelineMeta name)
getTimelineMeta name         = dataFetch (GetTimelineMeta name)

fillAllTimeline :: Article -> ArticleM Article
fillAllTimeline art = do
  timelines <- getAllArticleTimeline $ artID art
  return $ art { artTimelines = timelines }

createTable :: ArticleM Int64
createTable = uncachedRequest CreateTable
