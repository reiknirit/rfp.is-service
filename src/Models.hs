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
RFPService json
    deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
