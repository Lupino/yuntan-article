{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Article.Config
  (
    MySQLConfig (..)
  , Config (..)
  , genMySQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , lruEnv
  , redisEnv
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Database.Redis            (Connection)
import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)
import           Yuntan.Config.RedisConfig (RedisConfig (..),
                                            defaultRedisConfig,
                                            genRedisConnection)
import           Yuntan.Extra.Config       (ConfigLru)
import           Yuntan.Types.HasMySQL     (HasOtherEnv, otherEnv)

data Config = Config
  { mysqlConfig  :: MySQLConfig
  , redisConfig  :: RedisConfig
  , lruCacheSize :: Int
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig  <- o .: "mysql"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    lruCacheSize <- o .:? "lru-size" .!= 10
    return Config{..}

data Cache = Cache
  { lru   :: ConfigLru
  , redis :: Maybe Connection
  }

lruEnv :: (HasOtherEnv Cache u) => u -> ConfigLru
lruEnv = lru . otherEnv

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: ConfigLru -> Maybe Connection -> Cache
mkCache = Cache
