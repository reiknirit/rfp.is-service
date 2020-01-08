{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.Submission where


import Servant
import Control.Monad.IO.Class       (MonadIO,liftIO)
import Data.Maybe                   (Maybe(..), fromMaybe)
import Data.Time.Clock              (getCurrentTime)
import Database.Persist.Sql         (Entity(..)
                                    ,Filter(..)
                                    ,SelectOpt(..)
                                    ,fromSqlKey
                                    ,toSqlKey
                                    ,selectFirst
                                    ,selectList
                                    ,insert
                                    ,updateGet
                                    ,delete
                                    ,(=.)
                                    ,(==.))
import Models
import Config                       (AppT (..))

type SubmissionAPI = "submissions" :>
                     ReqBody '[JSON] Submission :>
                     Post '[JSON] (Maybe (Entity Submission))

submissionServer :: MonadIO m => ServerT SubmissionAPI (AppT m)
submissionServer = createSubmission

createSubmission :: MonadIO m => Submission -> AppT m (Maybe (Entity Submission))
createSubmission sub = do
    newSub <- runDb $ insert sub
    return $ Just $ Entity newSub sub
