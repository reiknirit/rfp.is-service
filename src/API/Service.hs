{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.Service where

import Servant
import Control.Monad.IO.Class       (MonadIO)

import API.Attachment               (AttachmentAPI
                                    ,attachmentServer)
import API.Submission               (SubmissionAPI
                                    ,submissionServer)
import API.User                     (UserAPI
                                    ,userServer)
import Config                       (AppT (..))

type ServiceAPI =  AttachmentAPI 
              :<|> SubmissionAPI
              :<|> UserAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer =  attachmentServer 
            :<|> submissionServer
            :<|> userServer
