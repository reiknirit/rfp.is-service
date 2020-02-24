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

import           Crypto.BCrypt              (hashPasswordUsingPolicy
                                            ,slowerBcryptHashingPolicy)
import           Control.Monad.Reader       (MonadIO
                                            ,MonadReader
                                            ,asks
                                            ,liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson                 (FromJSON
                                            ,ToJSON
                                            ,(.:)
                                            ,(.:?)
                                            ,(.=)
                                            ,object
                                            ,parseJSON
                                            ,toJSON
                                            ,withObject)

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (UTCTime
                                            ,getCurrentTime)
import           Database.Persist.Sql       (Entity(..)
                                            ,SqlPersistT
                                            ,(<-.)
                                            ,(=.)
                                            ,(==.)
                                            ,fromSqlKey
                                            ,runMigration
                                            ,runSqlPool
                                            ,selectFirst
                                            ,selectList
                                            ,insert)
import           Database.Persist.TH        (mkMigrate
                                            ,mkPersist
                                            ,persistLowerCase
                                            ,share
                                            ,sqlSettings)
import           GHC.Generics               (Generic)

import           Config                     (AppT
                                            ,Config
                                            ,configPool)


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


User json
    username        Text
    password        Text
    email           Text Maybe
    isAdmin         Bool
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic
|]

data SubmissionJSON = 
    SubmissionJSON
    (Entity Submission)
    [Entity Attachment]

instance ToJSON SubmissionJSON where
    toJSON (SubmissionJSON submission attachments) = 
        object [ "id" .= (fromSqlKey $ entityKey submission) 
               , "fullName" .= (submissionFullName $ entityVal submission)
               , "pronoun" .= (submissionPronoun $ entityVal submission)
               , "refund"  .= (submissionRefund $ entityVal submission)
               , "airport" .= (submissionAirport $ entityVal submission)
               , "title"   .= (submissionTitle $ entityVal submission)
               , "abstract" .= (submissionAbstract $ entityVal submission)
               , "bio" .= (submissionBio $ entityVal submission)
               , "comment" .= (submissionComment $ entityVal submission)
               , "email" .= (submissionEmail $ entityVal submission)
               , "phoneNumber" .= (submissionPhoneNumber $ entityVal submission)
               , "website" .= (submissionWebsite $ entityVal submission)
               , "attachments" .= attachments
               , "createdAt" .= (submissionCreatedAt $ entityVal submission)
               , "updatedAt" .= (submissionUpdatedAt $ entityVal submission)
               ]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

subToSubJSON :: MonadIO m 
             => Entity Submission 
             -> AppT m SubmissionJSON
subToSubJSON sub = do
    let
      attachmentIds = submissionAttachments $ entityVal sub
    attachments <- runDb $ selectList [ AttachmentId <-. attachmentIds ] []
    return $SubmissionJSON sub attachments

createUser :: Config 
           -> User
           -> IO (Maybe (Entity User))
createUser config user = do
    now <- getCurrentTime
    passw <- hashPasswordUsingPolicy 
             slowerBcryptHashingPolicy
             (TE.encodeUtf8 $ userPassword user) 
    case passw of
      Just p -> do
          let nUser = user { userPassword = TE.decodeUtf8 p
                           , userCreatedAt = now
                           }
          newUserId <- runReaderT (runDb $ insert nUser) config
          return $ Just $ Entity newUserId nUser
      Nothing -> return Nothing

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
