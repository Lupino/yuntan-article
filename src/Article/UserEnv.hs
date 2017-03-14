{-# LANGUAGE OverloadedStrings #-}

module Article.UserEnv
  (
    UserEnv (..)
  , ActionM
  , ScottyM
  , ArticleM
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Pool              (Pool)
import qualified Data.Text.Lazy         as LT (Text)
import           Database.MySQL.Simple  (Connection)
import           Haxl.Core              (GenHaxl)
import           Haxl.Core.Monad        (unsafeLiftIO)
import           Web.Scotty.Trans       (ActionT, ScottyT)

data UserEnv = UserEnv { uploadPath  :: FilePath
                       , mySQLPool   :: Pool Connection
                       , mySQLConn   :: Maybe Connection
                       , tablePrefix :: String
                       }

type ArticleM = GenHaxl UserEnv

instance MonadIO (GenHaxl u) where
  liftIO = unsafeLiftIO

type ActionM a = ActionT LT.Text ArticleM a
type ScottyM a = ScottyT LT.Text ArticleM a
