{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Article.Application
  (
    application
  ) where

import           Web.Scotty.Trans      (delete, get, middleware, post)

import           Network.Wai           (Middleware)
import           Yuntan.Extra.Config   (ConfigLru)
import           Yuntan.Types.HasMySQL (HasMySQL, HasOtherEnv)
import           Yuntan.Types.Scotty   (ScottyH)

import           Article.Router

application :: (HasMySQL u, HasOtherEnv ConfigLru u) => [Middleware] -> ScottyH u ()
application mids = do
  mapM_ middleware mids

  post   "/api/articles/"                    createArticleHandler
  post   "/api/articles/:art_id/"            $ requireArticle updateArticleHandler
  post   "/api/articles/:art_id/cover"       $ requireArticle updateArticleCoverHandler
  delete "/api/articles/:art_id/cover"       $ requireArticle removeArticleCoverHandler
  post   "/api/articles/:art_id/extra"       $ requireArticle updateArticleExtraHandler
  delete "/api/articles/:art_id/extra"       $ requireArticle removeArticleExtraHandler
  post   "/api/articles/:art_id/extra/clear" $ requireArticle clearArticleExtraHandler
  delete "/api/articles/:art_id/"            removeArticleHandler
  get    "/api/articles/:art_id/"            $ requireArticle getArticleHandler
  get    "/api/articles/"                    getAllArticleHandler

  get    "/api/tags/:tag_id/"                $ requireTag getTagHandler
  post   "/api/tags/"                        createTagHandler
  get    "/api/tags/"                        $ requireTag getTagHandler
  post   "/api/tags/:tag_id/"                updateTagHandler

  get    "/api/timeline/:timeline/"          getAllTimelineHandler
  post   "/api/timeline/:timeline/"          $ requireArticle createTimelineHandler
  post   "/api/timeline/:timeline/:art_id/"  $ requireArticle createTimelineHandler
  get    "/api/timeline/:timeline/meta"      getTimelineMetaHandler
  post   "/api/timeline/:timeline/meta"      saveTimelineMetaHandler
  delete "/api/timeline/:timeline/meta"      removeTimelineMetaHandler
  delete "/api/timeline/:timeline/:art_id/"  $ requireArticle removeTimelineHandler

  post   "/api/articles/:art_id/tags/"       $ requireTagAndArticle addArticleTagHandler
  delete "/api/articles/:art_id/tags/"       $ requireTagAndArticle removeArticleTagHandler

  get    "/api/check/"                       existsArticleHandler

  get    "/api/file/:key"                    getFileHandler
  post   "/api/file/:key"                    saveFileHandler

  post "/api/graphql/"                       graphqlHandler

  get "/api/config/:key/" getConfigHandler
  post "/api/config/:key/" setConfigHandler
