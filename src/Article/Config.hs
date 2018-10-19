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
  , redisEnv
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))
import           Database.Redis            (Connection)
import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)
import           Yuntan.Config.RedisConfig (RedisConfig (..),
                                            defaultRedisConfig,
                                            genRedisConnection)
import           Yuntan.Types.HasMySQL     (HasOtherEnv, otherEnv)

data Config = Config
  { mysqlConfig :: MySQLConfig
  , redisConfig :: RedisConfig
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig  <- o .: "mysql"
    redisConfig  <- o .:? "redis" .!= defaultRedisConfig
    return Config{..}

newtype Cache = Cache
  { redis :: Maybe Connection
  }

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: Maybe Connection -> Cache
mkCache = Cache
