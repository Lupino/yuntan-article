{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.RawAPI
  (
    getArticleById
  , createArticle
  , updateArticle
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent
  , updateArticleCover
  , updateArticleExtra
  , getArticleIdList
  , countAllArticle
  , removeArticle
  , existsArticle

  , saveFile
  , saveFileWithExtra
  , getFileWithKey
  , getFileById

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
  , removeTimelineList
  , removeTimelineListById
  , getTimelineListById
  , countTimeline
  , getIdListByTimeline
  , saveTimelineMeta
  , removeTimelineMeta
  , getTimelineMeta
  , mergeData
  ) where

import           Article.Types

import           Article.DataSource      (ArticleReq (..))
import           Data.Aeson              (Value)
import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)


getArticleById :: HasMySQL u => ID -> GenHaxl u (Maybe Article)
getArticleById artId = dataFetch (GetArticleById artId)


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

updateArticleExtra :: HasMySQL u => ID -> Value -> GenHaxl u Int64
updateArticleExtra artId extra = uncachedRequest (UpdateArticleExtra artId extra)

getArticleIdList :: (HasMySQL u) => From -> Size -> OrderBy -> GenHaxl u [ID]
getArticleIdList f s o = dataFetch (GetArticleIdList f s o)

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

addTag :: HasMySQL u => TagName -> GenHaxl u ID
addTag name = uncachedRequest (AddTag name)

getTagById :: HasMySQL u => ID -> GenHaxl u (Maybe Tag)
getTagById tid = dataFetch (GetTagById tid)

getTagByName :: HasMySQL u => TagName -> GenHaxl u (Maybe Tag)
getTagByName name = dataFetch (GetTagByName name)

getTags :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [Tag]
getTags from size o = dataFetch (GetTags from size o)

updateTag :: HasMySQL u => ID -> TagName -> GenHaxl u Int64
updateTag tid name = uncachedRequest (UpdateTag tid name)

addArticleTag :: HasMySQL u => ID -> ID -> GenHaxl u ID
addArticleTag aid tid = uncachedRequest (AddArticleTag aid tid)

removeArticleTag :: HasMySQL u => ID -> ID -> GenHaxl u Int64
removeArticleTag aid tid = uncachedRequest (RemoveArticleTag aid tid)

removeAllArticleTag :: HasMySQL u => ID -> GenHaxl u Int64
removeAllArticleTag aid = uncachedRequest (RemoveAllArticleTag aid)

getAllArticleTagName :: HasMySQL u => ID -> GenHaxl u [TagName]
getAllArticleTagName aid = dataFetch (GetAllArticleTagName aid)

addTimeline :: HasMySQL u => String -> ID -> GenHaxl u ID
addTimeline name aid = uncachedRequest (AddTimeline name aid)

removeTimeline :: HasMySQL u => String -> ID -> GenHaxl u Int64
removeTimeline name aid = uncachedRequest (RemoveTimeline name aid)

removeTimelineList :: HasMySQL u => String -> GenHaxl u Int64
removeTimelineList name = uncachedRequest (RemoveTimelineList name)

removeTimelineListById :: HasMySQL u => ID -> GenHaxl u Int64
removeTimelineListById aid = uncachedRequest (RemoveTimelineListById aid)

getIdListByTimeline
  :: HasMySQL u => String -> From -> Size -> OrderBy -> GenHaxl u [ID]
getIdListByTimeline name f s o = dataFetch (GetIdListByTimeline name f s o)

countTimeline :: HasMySQL u => String -> GenHaxl u Int64
countTimeline name = dataFetch (CountTimeline name)

getTimelineListById :: HasMySQL u => ID -> GenHaxl u [String]
getTimelineListById aid = dataFetch (GetTimelineListById aid)

saveTimelineMeta :: HasMySQL u => String -> Title -> Summary -> GenHaxl u Int64
saveTimelineMeta name t s = uncachedRequest (SaveTimelineMeta name t s)

removeTimelineMeta :: HasMySQL u => String -> GenHaxl u Int64
removeTimelineMeta name = uncachedRequest (RemoveTimelineMeta name)

getTimelineMeta :: HasMySQL u => String -> GenHaxl u (Maybe (Title, Summary))
getTimelineMeta name = dataFetch (GetTimelineMeta name)

mergeData :: HasMySQL u => GenHaxl u ()
mergeData = uncachedRequest MergeData
