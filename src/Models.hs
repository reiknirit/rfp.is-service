{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Reader     (MonadIO, MonadReader, asks, liftIO)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Database.Persist.Sql     (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH      (mkMigrate, mkPersist, persistLowerCase,
                                share, sqlSettings)

import Config                   (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Attachment json
    name      Text
    src       Text
    thumbnail Text Maybe
    createdAt UTCTime
    updatedAt UTCTime Maybe

Submission json
    fullName    Text
    pronoun     Text Maybe
    refund      Bool
    airport     Text Maybe
    title       Text Maybe
    abstract    Text Maybe
    bio         Text
    comment     Text Maybe
    email       Text
    phoneNumber Text
    website     Text Maybe
    attachments [AttachmentId]
    createdAt   UTCTime
    updatedAt   UTCTime Maybe
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
