{-# LANGUAGE FlexibleContexts #-}
module Article.RawAPI
  ( getArticleById
  , createArticle
  , updateArticle
  , updateArticleTitle
  , updateArticleSummary
  , updateArticleContent
  , updateArticleCover
  , updateArticleExtra
  , getArticleIdList
  , countArticle
  , removeArticle

  , saveFile
  , saveFileWithExtra
  , getFileWithKey
  , getFileById
  , removeFile

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

  , saveAlias
  , getAlias
  , removeAlias
  , getAllAlias
  , removeAllAlias
  ) where

import           Article.DataSource  (ArticleReq (..))
import           Article.Types
import           Data.Aeson          (Value)
import           Data.Int            (Int64)
import           Data.Text           (Text)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)


getArticleById :: HasPSQL u => ID -> GenHaxl u w (Maybe Article)
getArticleById artId = dataFetch (GetArticleById artId)


createArticle :: HasPSQL u => Title -> Summary -> Content -> CreatedAt -> GenHaxl u w ID
createArticle t s co c = uncachedRequest (CreateArticle t s co c)

updateArticle :: HasPSQL u => ID -> Title -> Summary -> Content -> GenHaxl u w Int64
updateArticle artId t s c = uncachedRequest (UpdateArticle artId t s c)

updateArticleTitle :: HasPSQL u => ID -> Title -> GenHaxl u w Int64
updateArticleTitle artId t = uncachedRequest (UpdateArticleTitle artId t)

updateArticleSummary :: HasPSQL u => ID -> Summary -> GenHaxl u w Int64
updateArticleSummary artId s = uncachedRequest (UpdateArticleSummary artId s)

updateArticleContent :: HasPSQL u => ID -> Content -> GenHaxl u w Int64
updateArticleContent artId c = uncachedRequest (UpdateArticleContent artId c)

updateArticleCover :: HasPSQL u => ID -> Maybe File -> GenHaxl u w Int64
updateArticleCover artId cover = uncachedRequest (UpdateArticleCover artId cover)

updateArticleExtra :: HasPSQL u => ID -> Value -> GenHaxl u w Int64
updateArticleExtra artId extra = uncachedRequest (UpdateArticleExtra artId extra)

getArticleIdList :: (HasPSQL u) => From -> Size -> OrderBy -> GenHaxl u w [ID]
getArticleIdList f s o = dataFetch (GetArticleIdList f s o)

countArticle :: HasPSQL u => GenHaxl u w Int64
countArticle = dataFetch CountArticle

removeArticle :: HasPSQL u => ID -> GenHaxl u w Int64
removeArticle artId = uncachedRequest (RemoveArticle artId)

saveFile :: HasPSQL u => FileBucket -> FileKey -> GenHaxl u w (Maybe File)
saveFile path fc = uncachedRequest (SaveFile path fc)


saveFileWithExtra :: HasPSQL u => FileBucket -> FileKey -> FileExtra -> GenHaxl u w (Maybe File)
saveFileWithExtra path fc extra = uncachedRequest (SaveFileWithExtra path fc extra)

getFileWithKey :: HasPSQL u => FileKey -> GenHaxl u w (Maybe File)
getFileWithKey k = dataFetch (GetFileWithKey k)

getFileById :: HasPSQL u => ID -> GenHaxl u w (Maybe File)
getFileById fileId = dataFetch (GetFileById fileId)

removeFile :: HasPSQL u => ID -> GenHaxl u w Int64
removeFile fid = uncachedRequest (RemoveFile fid)

addTag :: HasPSQL u => TagName -> GenHaxl u w ID
addTag name = uncachedRequest (AddTag name)

getTagById :: HasPSQL u => ID -> GenHaxl u w (Maybe Tag)
getTagById tid = dataFetch (GetTagById tid)

getTagByName :: HasPSQL u => TagName -> GenHaxl u w (Maybe Tag)
getTagByName name = dataFetch (GetTagByName name)

getTags :: HasPSQL u => From -> Size -> OrderBy -> GenHaxl u w [Tag]
getTags from size o = dataFetch (GetTags from size o)

updateTag :: HasPSQL u => ID -> TagName -> GenHaxl u w Int64
updateTag tid name = uncachedRequest (UpdateTag tid name)

addArticleTag :: HasPSQL u => ID -> ID -> GenHaxl u w ID
addArticleTag aid tid = uncachedRequest (AddArticleTag aid tid)

removeArticleTag :: HasPSQL u => ID -> ID -> GenHaxl u w Int64
removeArticleTag aid tid = uncachedRequest (RemoveArticleTag aid tid)

removeAllArticleTag :: HasPSQL u => ID -> GenHaxl u w Int64
removeAllArticleTag aid = uncachedRequest (RemoveAllArticleTag aid)

getAllArticleTagName :: HasPSQL u => ID -> GenHaxl u w [TagName]
getAllArticleTagName aid = dataFetch (GetAllArticleTagName aid)

addTimeline :: HasPSQL u => String -> ID -> GenHaxl u w ID
addTimeline name aid = uncachedRequest (AddTimeline name aid)

removeTimeline :: HasPSQL u => String -> ID -> GenHaxl u w Int64
removeTimeline name aid = uncachedRequest (RemoveTimeline name aid)

removeTimelineList :: HasPSQL u => String -> GenHaxl u w Int64
removeTimelineList name = uncachedRequest (RemoveTimelineList name)

removeTimelineListById :: HasPSQL u => ID -> GenHaxl u w Int64
removeTimelineListById aid = uncachedRequest (RemoveTimelineListById aid)

getIdListByTimeline
  :: HasPSQL u => String -> From -> Size -> OrderBy -> GenHaxl u w [ID]
getIdListByTimeline name f s o = dataFetch (GetIdListByTimeline name f s o)

countTimeline :: HasPSQL u => String -> GenHaxl u w Int64
countTimeline name = dataFetch (CountTimeline name)

getTimelineListById :: HasPSQL u => ID -> GenHaxl u w [String]
getTimelineListById aid = dataFetch (GetTimelineListById aid)

saveTimelineMeta :: HasPSQL u => String -> Title -> Summary -> GenHaxl u w Int64
saveTimelineMeta name t s = uncachedRequest (SaveTimelineMeta name t s)

removeTimelineMeta :: HasPSQL u => String -> GenHaxl u w Int64
removeTimelineMeta name = uncachedRequest (RemoveTimelineMeta name)

getTimelineMeta :: HasPSQL u => String -> GenHaxl u w (Maybe (Title, Summary))
getTimelineMeta name = dataFetch (GetTimelineMeta name)

mergeData :: HasPSQL u => GenHaxl u w ()
mergeData = uncachedRequest MergeData

getAlias :: HasPSQL u => Text -> GenHaxl u w (Maybe ID)
getAlias a = dataFetch (GetAlias a)

getAllAlias :: HasPSQL u => ID -> GenHaxl u w [Text]
getAllAlias a = dataFetch (GetAllAlias a)

removeAllAlias :: HasPSQL u => ID -> GenHaxl u w Int64
removeAllAlias a = uncachedRequest (RemoveAllAlias a)

removeAlias :: HasPSQL u => Text -> GenHaxl u w Int64
removeAlias a = uncachedRequest (RemoveAlias a)

saveAlias :: HasPSQL u => Text -> ID -> GenHaxl u w Int64
saveAlias a b = uncachedRequest (SaveAlias a b)
