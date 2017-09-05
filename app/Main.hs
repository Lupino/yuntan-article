{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Web.Scotty.Trans                     (scottyOptsT, settings)

import           Article                              (createTable)
import           Article.Application
import           Article.DataSource                   (initArticleState)
import           Article.UserEnv
import           Haxl.Core                            (StateStore, initEnv,
                                                       runHaxl, stateEmpty,
                                                       stateSet)

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

  (Just conf) <- Y.decodeFile confFile :: IO (Maybe C.Config)

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initArticleState mysqlThreads) stateEmpty

  let userEnv = UserEnv { mySQLPool   = pool
                        , tablePrefix = prefix
                        }

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  _ <- runIO userEnv state createTable

  scottyOptsT opts (runIO userEnv state) $ application [logStdout]
  where
        runIO :: UserEnv -> StateStore -> ArticleM b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m
