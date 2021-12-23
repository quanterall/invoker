{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import Lens.Micro.TH (makeLenses)
import Network.AWS.QAWS (EnvironmentFile)
import Network.AWS.QAWS.SQS.Types (QueueUrl)
import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { verbose :: !Bool,
    defaultQueueUrl :: !(Maybe QueueUrl),
    environmentFile :: !EnvironmentFile
  }
  deriving (Show, Eq, Generic)

instance FromJSON Options where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Options where
  toJSON = genericToJSON defaultOptions

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

data MessageTemplate = MessageTemplate
  { _templateName :: !Text,
    _templateMessage :: !Text
  }
  deriving (Eq, Show)

makeLenses ''MessageTemplate
