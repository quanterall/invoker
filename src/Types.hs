{-# LANGUAGE TemplateHaskell #-}

module Types where

import Network.AWS.QAWS.SQS.Types (QueueUrl)
import Qtility
import RIO.Process

-- | Command line arguments
data Options = Options
  { verbose :: !Bool,
    defaultQueueUrl :: !(Maybe QueueUrl),
    environmentFile :: !EnvironmentFile
  }
  deriving (Show, Eq, Generic)

instance FromJSON Options where
  parseJSON = genericParseJSON defaultAesonOptions

instance ToJSON Options where
  toJSON = genericToJSON defaultAesonOptions

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
