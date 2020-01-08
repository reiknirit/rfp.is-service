{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.Service where

import Servant
import Control.Monad.IO.Class      (MonadIO)

import Config                      (AppT (..))

type ServiceAPI = "test" :> Get '[JSON] String

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer = testEndpoint

testEndpoint :: MonadIO m => AppT m String
testEndpoint = return "Hello World"
