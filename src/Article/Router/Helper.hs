{-# LANGUAGE OverloadedStrings #-}
module Article.Router.Helper
  (
    safeParam
  , pageParam
  , runWithEnv
  , withTransaction
  , timeline
  , timelineTitle
  , timelineMeta
  , articles
  , article
  , resultOK
  ) where

import           Article
import           Article.UserEnv           (ActionM, UserEnv (..))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (lift)
import           Data.Aeson                (object, (.=))
import           Data.Int                  (Int64)
import           Data.Pool                 (withResource)
import qualified Data.Text                 as T (pack)
import           Data.Text.Lazy            (Text)
import qualified Database.MySQL.Simple     as MySQL (withTransaction)
import           Dispatch.Types.ListResult (From, ListResult (..), Size,
                                            emptyListResult)
import           Dispatch.Types.OrderBy    (desc)
import           Haxl.Core                 (Env (..), env, initEnv, runHaxl,
                                            withEnv)
import           Web.Scotty.Trans          (Parsable (..), json, param, rescue)

safeParam ::(Parsable a) => Text -> a -> ActionM a
safeParam key def = param key `rescue` (\_ -> return def)

pageParam :: ActionM (Int64, Int64)
pageParam = do
  page <- safeParam "page" (1 :: Int64)
  from <- safeParam "from" (0 :: Int64)
  size <- safeParam "size" 10
  return (max ((page - 1) * size) from, size)

runWithEnv :: ArticleM a -> ArticleM a
runWithEnv act = do
  state <- env states
  ue <- env userEnv
  env0 <- liftIO $ initEnv state ue
  withEnv env0 act

withTransaction :: ArticleM a -> ArticleM a
withTransaction act = do
  state <- env states
  ue <- env userEnv
  liftIO $ withResource (mySQLPool ue) $ \conn -> do
    env0 <- initEnv state $ ue { mySQLConn = Just conn }
    MySQL.withTransaction conn $ runHaxl env0 act

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

resultOK :: ActionM ()
resultOK = json $ object ["result" .= T.pack "OK"]
