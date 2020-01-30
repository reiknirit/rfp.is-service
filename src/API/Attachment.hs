{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module API.Attachment where

import Control.Applicative.Combinators  (optional)
import Control.Monad.Except             (MonadIO)
import Control.Monad.IO.Class           (liftIO)
import Data.Int                         (Int64)
import Data.Maybe                       (Maybe(..), fromMaybe)
import qualified Data.Text              as T
import Data.Time.Calendar               (fromGregorian)
import Data.Time.Clock                  (UTCTime(..)
                                        ,getCurrentTime, secondsToDiffTime)
import Data.UUID                        (toString)
import Data.UUID.V4                     (nextRandom)
import Database.Persist.Postgresql      (Entity(..)
                                        ,Filter(..)
                                        ,SelectOpt(..)
                                        ,(=.)
                                        ,(==.)
                                        ,delete
                                        ,fromSqlKey
                                        ,insert
                                        ,selectFirst
                                        ,selectList
                                        ,toSqlKey
                                        ,updateGet)
import Servant
import Servant.Multipart
import System.Directory                 (doesFileExist, renameFile)
import System.FilePath.Posix            (takeBaseName
                                        ,takeExtensions, takeFileName)

import Config                           (AppT(..), filePath)
import Models
import Utils                            (processImage)

instance FromMultipart Tmp Attachment where
    fromMultipart multipartData =
        Attachment <$> fmap fdFileName (lookupFile "file" multipartData) <*>
        fmap T.pack (fmap fdPayload (lookupFile "file" multipartData)) <*>
        (optional $ lookupInput "thumbnail" multipartData) <*>
        (Just $ UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)) <*>
        (optional Nothing)

type AttachmentAPI = MultipartForm Tmp Attachment :> 
                     "attachments" :> 
                     "upload" :> 
                     Post '[JSON] (Maybe (Entity Attachment))

attachmentServer :: MonadIO m => ServerT AttachmentAPI (AppT m)
attachmentServer = uploadAttachment

isImage :: T.Text -> Bool
isImage txt = elem extension attachmentCases
    where
        extension = takeExtensions $ T.unpack txt
        attachmentCases = [".png",".jpg", ".jpeg", ".webp"]

uploadAttachment :: MonadIO m 
                 => Attachment
                 -> AppT m (Maybe (Entity Attachment))
uploadAttachment attachment = do
    -- Get currentTime for create UTCTime
    currentTime <- liftIO getCurrentTime
    -- Get full filename with static path
    let fileName = T.pack filePath <> attachmentName attachment
    -- Check if file already exists
    fileExists <- liftIO $ doesFileExist (T.unpack fileName)
    finalName
        -- | If file exists we create a new path
        --   appending a unique uuid string to the filename
         <-
        case fileExists of
            True -> do
                uuid <- liftIO nextRandom
                let uuidStr = toString uuid
                    fnBase = takeBaseName (T.unpack fileName)
                    ext = takeExtensions (T.unpack fileName)
                    final = T.pack $ filePath ++ fnBase ++ uuidStr ++ ext
                return final
            False -> return fileName
    -- Get only the filename
    let onlyName = T.pack (takeFileName $ T.unpack finalName)
    -- Rename the temporary upload file to final destination
    liftIO $
        renameFile (T.unpack $ attachmentSrc attachment) (filePath <> T.unpack onlyName)
    -- process attachment and create thumbnail
    -- Disabling this for now since we are receiving
    -- attachments meaning (pdf, .docx etc) and 
    -- not only profile images
    -- thumbnail <- liftIO $ processImage finalName
    let attach =
            attachment
                { attachmentName = onlyName
                , attachmentSrc = finalName
                , attachmentCreatedAt = currentTime
                }
    dbImage <- runDb $ insert attach
    return $ Just $ Entity dbImage attach
