{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Configuration.Dotenv           (defaultConfig, loadFile)
import           Control.Monad                  (void)

import           Control.Concurrent.MVar        (newMVar)
import           Control.Concurrent             (forkIO)
import           Data.Int                       (Int64)
import           Data.Maybe                     (fromMaybe)
import           Database.Redis                 (connectHost
                                                ,defaultConnectInfo)
import           System.Hworker
import           System.Environment             (lookupEnv)

import           Worker.RFPWorker               (State(..))

main = do
    void $ loadFile defaultConfig
    state <- newMVar (0 :: Int64)

    host <- lookupEnv "REDIS_HOST"
    let 
      connectInfo = defaultConnectInfo 
          { connectHost = fromMaybe "localhost" host 
          }
      workerConfig = (defaultHworkerConfig "RFPWorker" (State state)) 
        { hwconfigDebug = True
        , hwconfigRedisConnectInfo =  RedisConnectInfo connectInfo
        , hwconfigTimeout = 120
        }

    sworker <- createWith workerConfig

    -- Start the worker
    worker sworker
