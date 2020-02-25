{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Worker.Base where

import           Control.Monad.Trans.Reader     (runReaderT)
import           Data.Aeson                     (FromJSON
                                                ,ToJSON
                                                ,encode)
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Char8          as BS
import qualified Database.Redis                 as R
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.UUID                      as UUID
import qualified Data.UUID.V4                   as UUID

import Cache                                    (runCache)
import Config                                   (getConfig)

addNewJob :: (ToJSON a, FromJSON a) 
          => a
          -> T.Text 
          -> IO Integer
addNewJob job workerName = do
    let jobId = T.encodeUtf8 $ "hworker-jobs-" <> workerName
    conf   <- getConfig
    jobStr <- createJob job
    jobAdded <- runReaderT (runCache $ R.lpush jobId [jobStr]) conf
    case jobAdded of
        Left _ -> return 0
        Right i -> return i

createJob :: (FromJSON a, ToJSON a) 
          => a 
          -> IO BS.ByteString
createJob job = do
    job_id <- UUID.toString <$> UUID.nextRandom
    return $ LB.toStrict $ encode (job_id, job)
