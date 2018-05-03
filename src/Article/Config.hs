{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Article.Config
  (
    MySQLConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson                (FromJSON, parseJSON, withObject,
                                            (.!=), (.:), (.:?))

import           Yuntan.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)

data Config = Config
  { mysqlConfig  :: MySQLConfig
  , lruCacheSize :: Int
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig  <- o .: "mysql"
    lruCacheSize <- o .:? "lru-size" .!= 10
    return Config{..}
