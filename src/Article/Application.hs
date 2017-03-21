{-# LANGUAGE OverloadedStrings #-}
module Article.Application
  (
    application
  ) where

import           Web.Scotty.Trans (delete, get, middleware, post, put)

import           Network.Wai      (Middleware)

import           Article.Router
import           Article.UserEnv  (ScottyM)

application :: [Middleware] -> ScottyM ()
application mids = do
  mapM_ middleware mids

  put    "/api/upload/" uploadHandler

  post   "/api/articles/" createArticleHandler
  post   "/api/articles/:art_id/" updateArticleHandler
  post   "/api/articles/:art_id/cover" updateArticleCoverHandler
  delete "/api/articles/:art_id/cover" removeArticleCoverHandler
  post   "/api/articles/:art_id/extra" updateArticleExtraHandler
  delete "/api/articles/:art_id/extra" removeArticleExtraHandler
  post   "/api/articles/:art_id/extra/clear" clearArticleExtraHandler
  delete "/api/articles/:art_id/" removeArticleHandler
  get    "/api/articles/:art_id/" getArticleHandler
  get    "/api/articles/" getAllArticleHandler

  get    "/api/tags/:tag_id/" getTagHandler
  post   "/api/tags/" createTagHandler
  get    "/api/tags/" getTagHandler
  post   "/api/tags/:tag_id/" updateTagHandler

  get    "/api/timeline/:timeline/" getAllTimelineHandler
  post   "/api/timeline/:timeline/" createTimelineHandler
  post   "/api/timeline/:timeline/meta" saveTimelineMetaHandler
  delete "/api/timeline/:timeline/meta" removeTimelineMetaHandler
  delete "/api/timeline/:timeline/:art_id/" removeTimelineHandler

  post   "/api/articles/:art_id/tags/" addArticleTagHandler
  delete "/api/articles/:art_id/tags/" removeArticleHandler


  get    "/api/check/" existsArticleHandler
