{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Article                              (mergeData)
import           Article.Application
import qualified Article.Config                       as C
import           Article.DataSource                   (initArticleState)
import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Data.String                          (fromString)
import qualified Data.Yaml                            as Y
import           Database.PSQL.Types                  (HasOtherEnv, HasPSQL,
                                                       simpleEnv)
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)
import           Haxl.RedisCache                      (initRedisState)
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Options.Applicative
import           Web.Scotty.Trans                     (scottyOptsT, settings)

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

  let psqlConfig   = C.psqlConfig conf
      psqlThreads  = C.psqlHaxlNumThreads psqlConfig
      redisConfig  = C.redisConfig conf
      redisThreads = C.redisHaxlNumThreads redisConfig

  pool <- C.genPSQLPool psqlConfig
  redis <- C.genRedisConnection redisConfig

  let state = stateSet (initRedisState redisThreads $ fromString prefix)
            $ stateSet (initArticleState psqlThreads) stateEmpty

  let u = simpleEnv pool (fromString prefix) $ C.mkCache redis

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  runIO u state mergeData

  scottyOptsT opts (runIO u state) $ application [logStdout]
  where runIO :: (HasPSQL u, HasOtherEnv C.Cache u) => u -> StateStore -> GenHaxl u w b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m
