{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Worker.RFPWorker where

import           Configuration.Dotenv           (defaultConfig, loadFile)
import           Control.Concurrent.MVar        (MVar)
import           Control.Monad                  (void)
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Int                       (Int64)
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)
import           System.Hworker

import           Config
import           Models
import           Utils                          (sendEmail)

data RFPJob
    = SendEmail 
      T.Text        -- ^ to Email
      deriving (Eq, Show, Generic)

data State = State (MVar Int64)

instance ToJSON RFPJob
instance FromJSON RFPJob

fromEmail :: T.Text
fromEmail = "rfp@rfp.is"

defaultSubject :: T.Text
defaultSubject = "Thank you for your submission."

defaultContent :: T.Text
defaultContent = "\
\We have received your submission for review and will notify you once we have further details. If you have any questions feel free to write us at: ft@ft.is \n\n\
\About the conference \n\n\
\The Computer Science Society invites software developers, testers and researchers from around the world to submit abstracts for presentation at the conference Software Correctness 2020 in Reykjavík, Iceland. \n\
    \Presentations and lectures will take place on  Tuesday, Wednesday, and Thursday, August 18.–20. 2020. \n\
    \Abstracts will be reviewed by faculty of Computer Science of the University of Iceland. \n\
    \All accepted presenters will be offered travel expense reimbursement, a speaker's networking event at an exotic destination in Iceland and a chance to publish an article in the conference proceedings. \n\n\
    \Hosted in cooperation with Reykjavík Functional Programming and the University of Iceland, this new conference will bring together software developers, quality assurance engineers and researchers. \n\
    \Prospective topics include the specification and verification of software and software design, type-driven code generation, test case generation, static analysis and advances in programming language design. \n\
    \Other possible topics are stories of past and present successes and failures of methods for ensuring correctness as well as predictions for future methods.\n\n\
    \Thank you for your contribution. \n\n"

instance Job State RFPJob where
    job (State mVar) (SendEmail toEmail) = do
        void $ loadFile defaultConfig
        config <- getConfig
        status <- sendEmail (sendgridAPIKey config) fromEmail toEmail defaultSubject defaultContent
        case (status >= 200 && status <= 300) of
          True -> return Success
          False -> return $ Failure "Error in Sendgrid sendMail function"
