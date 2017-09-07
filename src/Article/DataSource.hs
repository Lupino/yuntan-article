{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Article.DataSource (
    ArticleReq(..)
  , initArticleState
  ) where

import           Data.Hashable               (Hashable (..))
import           Data.Typeable               (Typeable)
import           Haxl.Core                   (BlockedFetch (..), DataSource,
                                              DataSourceName, Flags,
                                              PerformFetch (..), ShowP, State,
                                              StateKey, dataSourceName, fetch,
                                              putFailure, putSuccess, showp)

import           Article.DataSource.Article
import           Article.DataSource.File
import           Article.DataSource.Table
import           Article.DataSource.Tag
import           Article.DataSource.Timeline
import           Article.Types
import           Yuntan.Types.HasMySQL       (HasMySQL, mysqlPool, tablePrefix)
import           Yuntan.Types.ListResult     (From, Size)
import           Yuntan.Types.OrderBy        (OrderBy)

import qualified Control.Exception           (SomeException, bracket_, try)
import           Data.Aeson                  (Value (..))
import           Data.Int                    (Int64)
import           Data.Pool                   (withResource)
import           Database.MySQL.Simple       (Connection)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data ArticleReq a where
  CreateArticle        :: Title -> Summary -> Content -> FromURL -> CreatedAt -> ArticleReq ID
  GetArticleById       :: ID -> ArticleReq (Maybe Article)
  UpdateArticle        :: ID -> Title -> Summary -> Content -> ArticleReq Int64
  UpdateArticleTitle   :: ID -> Title -> ArticleReq Int64
  UpdateArticleSummary :: ID -> Summary -> ArticleReq Int64
  UpdateArticleContent :: ID -> Content -> ArticleReq Int64
  UpdateArticleCover   :: ID -> Maybe File -> ArticleReq Int64
  UpdateArticleExtra   :: ID -> Value -> ArticleReq Int64
  GetAllArticle        :: From -> Size -> OrderBy -> ArticleReq [Article]
  CountAllArticle      :: ArticleReq Int64
  RemoveArticle        :: ID -> ArticleReq Int64

  SaveFile             :: FileBucket -> FileKey -> ArticleReq (Maybe File)
  SaveFileWithExtra    :: FileBucket -> FileKey -> FileExtra -> ArticleReq (Maybe File)
  GetFileWithKey       :: FileKey -> ArticleReq (Maybe File)
  GetFileById          :: ID -> ArticleReq (Maybe File)
  GetFiles             :: [ID] -> ArticleReq [File]

  ExistsArticle        :: FromURL -> ArticleReq (Maybe Int64)

  AddTag               :: TagName -> ArticleReq ID
  GetTagById           :: ID -> ArticleReq (Maybe Tag)
  GetTagByName         :: TagName -> ArticleReq (Maybe Tag)
  GetTags              :: From -> Size -> OrderBy -> ArticleReq [Tag]
  UpdateTag            :: ID -> TagName -> ArticleReq Int64
  AddArticleTag        :: ID -> ID -> ArticleReq ID
  RemoveArticleTag     :: ID -> ID -> ArticleReq Int64
  RemoveAllArticleTag  :: ID -> ArticleReq Int64
  GetAllArticleTagName :: ID -> ArticleReq [TagName]

  AddTimeline              :: String -> ID -> ArticleReq ID
  RemoveTimeline           :: String -> ID -> ArticleReq Int64
  RemoveAllTimeline        :: String -> ArticleReq Int64
  RemoveAllTimelineByArtId :: ID -> ArticleReq Int64
  GetAllTimeline           :: String -> From -> Size -> OrderBy -> ArticleReq [Article]
  CountTimeline            :: String -> ArticleReq Int64
  GetAllArticleTimeline    :: ID -> ArticleReq [String]
  SaveTimelineMeta         :: String -> Title -> Summary -> ArticleReq Int64
  RemoveTimelineMeta       :: String -> ArticleReq Int64
  GetTimelineMeta          :: String -> ArticleReq (Maybe (Title, Summary))

  CreateTable              :: ArticleReq Int64

  deriving (Typeable)

deriving instance Eq (ArticleReq a)
instance Hashable (ArticleReq a) where
  hashWithSalt s (CreateArticle a b c d e)    = hashWithSalt s (0::Int, a, b, c, d, e)
  hashWithSalt s (GetArticleById a)           = hashWithSalt s (1::Int, a)
  hashWithSalt s (UpdateArticle a b c d)      = hashWithSalt s (2::Int, a, b, c, d)
  hashWithSalt s (UpdateArticleTitle a b)     = hashWithSalt s (3::Int, a, b)
  hashWithSalt s (UpdateArticleSummary a b)   = hashWithSalt s (4::Int, a, b)
  hashWithSalt s (UpdateArticleContent a b)   = hashWithSalt s (5::Int, a, b)
  hashWithSalt s (UpdateArticleCover a b)     = hashWithSalt s (6::Int, a, b)
  hashWithSalt s (UpdateArticleExtra a b)     = hashWithSalt s (7::Int, a, b)
  hashWithSalt s (GetAllArticle a b c)        = hashWithSalt s (8::Int, a, b, c)
  hashWithSalt s CountAllArticle              = hashWithSalt s (9::Int)
  hashWithSalt s (RemoveArticle a)            = hashWithSalt s (10::Int, a)

  hashWithSalt s (SaveFile a b)               = hashWithSalt s (11::Int, a, b)
  hashWithSalt s (SaveFileWithExtra a b c)    = hashWithSalt s (12::Int, a, b, c)
  hashWithSalt s (GetFileWithKey a)           = hashWithSalt s (13::Int, a)
  hashWithSalt s (GetFileById a)              = hashWithSalt s (14::Int, a)
  hashWithSalt s (GetFiles a)                 = hashWithSalt s (15::Int, a)
  hashWithSalt s (ExistsArticle a)            = hashWithSalt s (16::Int, a)

  hashWithSalt s (AddTag a)                   = hashWithSalt s (17::Int, a)
  hashWithSalt s (GetTagById a)               = hashWithSalt s (18::Int, a)
  hashWithSalt s (GetTagByName a)             = hashWithSalt s (19::Int, a)
  hashWithSalt s (GetTags a b c)              = hashWithSalt s (20::Int, a, b, c)
  hashWithSalt s (UpdateTag a b)              = hashWithSalt s (21::Int, a, b)
  hashWithSalt s (AddArticleTag a b)          = hashWithSalt s (22::Int, a, b)
  hashWithSalt s (RemoveArticleTag a b)       = hashWithSalt s (23::Int, a, b)
  hashWithSalt s (RemoveAllArticleTag a)      = hashWithSalt s (24::Int, a)
  hashWithSalt s (GetAllArticleTagName a)     = hashWithSalt s (25::Int, a)

  hashWithSalt s (AddTimeline a b)            = hashWithSalt s (26::Int, a, b)
  hashWithSalt s (RemoveTimeline a b)         = hashWithSalt s (27::Int, a, b)
  hashWithSalt s (RemoveAllTimeline a)        = hashWithSalt s (28::Int, a)
  hashWithSalt s (RemoveAllTimelineByArtId a) = hashWithSalt s (29::Int, a)
  hashWithSalt s (GetAllTimeline a b c d)     = hashWithSalt s (30::Int, a, b, c, d)
  hashWithSalt s (GetAllArticleTimeline a)    = hashWithSalt s (31::Int, a)
  hashWithSalt s (CountTimeline a)            = hashWithSalt s (32::Int, a)
  hashWithSalt s (SaveTimelineMeta a b c)     = hashWithSalt s (33::Int, a, b, c)
  hashWithSalt s (RemoveTimelineMeta a)       = hashWithSalt s (34::Int, a)
  hashWithSalt s (GetTimelineMeta a)          = hashWithSalt s (35::Int, a)

  hashWithSalt s CreateTable                  = hashWithSalt s (36::Int)


deriving instance Show (ArticleReq a)
instance ShowP ArticleReq where showp = show

instance StateKey ArticleReq where
  data State ArticleReq = ArticleState { numThreads :: Int }

instance DataSourceName ArticleReq where
  dataSourceName _ = "ArticleDataSource"

instance HasMySQL u => DataSource u ArticleReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State ArticleReq
  -> Flags
  -> u
  -> [BlockedFetch ArticleReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
    sem <- newQSem $ numThreads _state
    asyncs <- mapM (fetchAsync sem _user) blockedFetches
    inner
    mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch ArticleReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch ArticleReq -> TablePrefix -> Connection -> IO ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ArticleReq a -> TablePrefix -> Connection -> IO a
fetchReq (CreateArticle t s co f c)        = createArticle t s co f c

fetchReq (GetArticleById artId)            = getArticle artId

fetchReq (UpdateArticle artId t s c)       = updateArticle artId t s c
fetchReq (UpdateArticleTitle artId t)      = updateArticleTitle artId t
fetchReq (UpdateArticleSummary artId s)    = updateArticleSummary artId s
fetchReq (UpdateArticleContent artId c)    = updateArticleContent artId c
fetchReq (UpdateArticleCover artId c)      = updateArticleCover artId c
fetchReq (UpdateArticleExtra artId e)      = updateArticleExtra artId e

fetchReq (RemoveArticle artId)             = removeArticle artId
fetchReq (GetAllArticle f s o)             = getAllArticle f s o
fetchReq CountAllArticle                   = countAllArticle

fetchReq (SaveFile path fc)                = saveFile path fc
fetchReq (SaveFileWithExtra path fc extra) = saveFileWithExtra path fc extra
fetchReq (GetFileWithKey key)              = getFileWithKey key
fetchReq (GetFileById fileId)              = getFile fileId
fetchReq (GetFiles fileIds)                = getFiles fileIds

fetchReq (ExistsArticle u)                 = existsArticle u

fetchReq (AddTag name)                     = addTag name
fetchReq (GetTagById tid)                  = getTagById tid
fetchReq (GetTagByName name)               = getTagByName name
fetchReq (GetTags f s o)                   = getTags f s o
fetchReq (UpdateTag tid name)              = updateTag tid name
fetchReq (AddArticleTag aid tid)           = addArticleTag aid tid
fetchReq (RemoveArticleTag aid tid)        = removeArticleTag aid tid
fetchReq (RemoveAllArticleTag aid)         = removeAllArticleTag aid
fetchReq (GetAllArticleTagName aid)        = getAllArticleTagName aid

fetchReq (AddTimeline name aid)            = addTimeline name aid
fetchReq (RemoveTimeline name aid)         = removeTimeline name aid
fetchReq (RemoveAllTimeline name)          = removeAllTimeline name
fetchReq (RemoveAllTimelineByArtId aid)    = removeAllTimelineByArtId aid
fetchReq (GetAllTimeline name f s o)       = getAllTimeline name f s o
fetchReq (CountTimeline name)              = countTimeline name
fetchReq (GetAllArticleTimeline aid)       = getAllArticleTimeline aid
fetchReq (SaveTimelineMeta name t s)       = saveTimelineMeta name t s
fetchReq (RemoveTimelineMeta name)         = removeTimelineMeta name
fetchReq (GetTimelineMeta name)            = getTimelineMeta name

fetchReq CreateTable                       = createTable

initArticleState :: Int -> State ArticleReq
initArticleState = ArticleState
