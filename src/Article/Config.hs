{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Article.Config
  (
    MySQLConfig (..)
  , Config (..)
  , genMySQLPool
  ) where

import           Data.Aeson                  (FromJSON, parseJSON, withObject,
                                              (.:))

import           Dispatch.Config.MySQLConfig (MySQLConfig (..), genMySQLPool)

data Config = Config { mysqlConfig :: MySQLConfig
                     }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    mysqlConfig <- o .: "mysql"
    return Config{..}