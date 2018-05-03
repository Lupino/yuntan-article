{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Web.Scotty.Trans                     (scottyOptsT, settings)

import           Article                              (mergeData)
import           Article.Application
import           Article.DataSource                   (initArticleState)
import           Data.LruCache.IO                     (newLruHandle)
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)
import           Yuntan.Types.HasMySQL                (ConfigLru, HasMySQL,
                                                       SimpleEnv,
                                                       initConfigState,
                                                       simpleEnv)

import           Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Article.Config                       as C
import qualified Data.Yaml                            as Y

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Article micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Article micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "Article micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Article micro server"
     <> header "article - Article micro server" )

program :: Options -> IO ()
program Options { getConfigFile  = confFile
                , getTablePrefix = prefix
                , getHost        = host
                , getPort        = port
                } = do

  (Right conf) <- Y.decodeFileEither confFile

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig
      lruCacheSize = C.lruCacheSize conf

  pool <- C.genMySQLPool mysqlConfig
  lru <- newLruHandle lruCacheSize

  let state = stateSet (initConfigState mysqlThreads)
            $ stateSet (initArticleState mysqlThreads) stateEmpty

  let u = simpleEnv pool prefix (Just lru) :: SimpleEnv ConfigLru

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  runIO u state mergeData

  scottyOptsT opts (runIO u state) $ application [logStdout]
  where
        runIO ::  HasMySQL u => u -> StateStore -> GenHaxl u b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m
