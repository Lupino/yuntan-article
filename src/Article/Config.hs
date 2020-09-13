{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Article.Config
  ( PSQL (..)
  , Config (..)
  , genPSQLPool
  , genRedisConnection
  , RedisConfig (..)
  , Cache
  , mkCache
  , redisEnv
  ) where

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.!=),
                                       (.:), (.:?))
import           Database.PSQL.Config (PSQL (..), genPSQLPool)
import           Database.PSQL.Types  (HasOtherEnv, otherEnv)
import           Database.Redis       (Connection)
import           Haxl.RedisConfig     (RedisConfig (..), defaultRedisConfig,
                                       genRedisConnection)

data Config = Config
  { psqlConfig  :: PSQL
  , redisConfig :: RedisConfig
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    psqlConfig  <- o .: "psql"
    redisConfig <- o .:? "redis" .!= defaultRedisConfig
    return Config{..}

newtype Cache = Cache
  { redis :: Maybe Connection
  }

redisEnv :: (HasOtherEnv Cache u) => u -> Maybe Connection
redisEnv = redis . otherEnv

mkCache :: Maybe Connection -> Cache
mkCache = Cache
