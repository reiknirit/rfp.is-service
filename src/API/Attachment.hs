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
isImage txt = elem extension imageCases
    where
        extension = takeExtensions $ T.unpack txt
        imageCases = [".png",".jpg", ".jpeg", ".webp"]

uploadAttachment :: MonadIO m 
                 => Attachment
                 -> AppT m (Maybe (Entity Attachment))
uploadAttachment attachment = do
    currentTime <- liftIO getCurrentTime
    let fileName = T.pack filePath <> attachmentName attachment
    fileExists <- liftIO $ doesFileExist (T.unpack fileName)
    tmpFileExists <- liftIO $ doesFileExist (T.unpack $ attachmentSrc attachment)
    finalName <-
        case fileExists of
            True -> do
                uuid <- liftIO nextRandom
                let uuidStr = toString uuid
                let fnBase = takeBaseName (T.unpack fileName)
                let ext = takeExtensions (T.unpack fileName)
                return $ T.pack $ filePath ++ fnBase ++ uuidStr ++ ext
            False -> return fileName
    tmpFileExists2 <- liftIO $ doesFileExist (T.unpack $ attachmentSrc attachment)
    let onlyName = T.pack (takeFileName $ T.unpack finalName)
    liftIO $
        renameFile (T.unpack $ attachmentSrc attachment) ("static/" <> T.unpack onlyName)
    -- process attachment and create thumbnail if image
    thumbnail <- case isImage finalName of
                   True -> do
                       thumb <- liftIO $ processImage finalName
                       return $ Just thumb
                   False -> return Nothing
    let 
        finalThumb = case thumbnail of
                       Just t -> Just $ T.replace "static/" "static/uploads/" t
                       Nothing -> Nothing
        attach =
            attachment
                { attachmentSrc = finalName
                , attachmentName = onlyName
                , attachmentThumbnail = finalThumb
                , attachmentCreatedAt = currentTime
                }
    dbKey <- runDb $ insert attach
    return $ Just $ Entity dbKey attach
