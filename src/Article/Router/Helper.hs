{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Helper
  (
    pageParam
  , timeline
  , timelineTitle
  , timelineMeta
  , articles
  , article
  , requireArticle
  , requireTag
  , requireTagAndArticle
  , resultOK
  ) where

import           Article
import           Article.UserEnv         (ActionM)
import           Control.Monad.Reader    (lift)
import           Data.Int                (Int64)
import           Data.Text.Lazy          (Text)
import           Web.Scotty.Trans        (param)
import           Yuntan.Types.ListResult (From, ListResult (..), Size,
                                          emptyListResult)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Utils.Scotty     (errNotFound, ok, safeParam)

pageParam :: ActionM (Int64, Int64)
pageParam = do
  page <- safeParam "page" (1 :: Int64)
  from <- safeParam "from" (0 :: Int64)
  size <- safeParam "size" 10
  return (max ((page - 1) * size) from, size)

timelineMeta :: String -> ActionM (Maybe (Title, Summary))
timelineMeta = lift . getTimelineMeta

timelineTitle :: String -> ActionM Title
timelineTitle name = maybe name fst <$> timelineMeta name

timeline :: String -> ActionM (ListResult Article)
timeline name = articles' s t
  where s = flip' (getAllTimeline name) $ desc "art_id"
        t = countTimeline name

articles :: ActionM (ListResult Article)
articles = articles' s t
  where s = flip' getAllArticle $ desc "id"
        t = countAllArticle

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f = g
  where g c a b = f a b c


articles' :: (From -> Size -> ArticleM [Article]) -> ArticleM Int64 -> ActionM (ListResult Article)
articles' s t = do
  (from, size) <- pageParam
  lift $ do
    arts <- s from size
    total <- t

    return emptyListResult { getResult = arts
                           , getFrom   = from
                           , getSize   = size
                           , getTotal  = total
                           }

article :: ActionM (Maybe Article)
article = do
  aid <- param "art_id"
  lift $ getArticleById aid

requireArticle :: (Article -> ActionM ()) -> ActionM ()
requireArticle action = do
  artId <- param "art_id"

  art <- lift $ getArticleById artId
  maybe (notFound artId) action art

  where notFound artId = errNotFound $ concat [ "Article (", show artId, ") not found" ]

requireTag :: (Tag -> ActionM ()) -> ActionM ()
requireTag action = do
  tid <- safeParam "tag_id" (0::ID)
  name <- safeParam "tag" (""::String)

  tag <- lift $
    if tid > 0 then getTagById tid
               else getTagByName name

  maybe notFound action tag
  where notFound = errNotFound "Not Found."

merge :: Monad m => ((a -> m ()) -> m ()) -> ((b -> m ()) -> m ()) -> (a -> b -> m ()) -> m ()
merge f g t = f $ \a -> g $ \b -> t a b

requireTagAndArticle :: (Tag -> Article -> ActionM ()) -> ActionM ()
requireTagAndArticle = merge requireTag requireArticle

resultOK :: ActionM ()
resultOK = ok "result" ("OK" :: Text)
