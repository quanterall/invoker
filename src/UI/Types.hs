{-# LANGUAGE TemplateHaskell #-}

module UI.Types where

import Brick.BChan (BChan)
import Brick.Forms (Form)
import qualified Network.AWS as AWS
import Network.AWS.QAWS.SQS.Types (QueueAttributes, QueueUrl)
import Qtility.Standard
import Types

data Name
  = QueueUrlField
  | MessageField
  | TemplateListField
  deriving (Eq, Ord, Show)

data Screen
  = SendMessageScreen
  | LoadTemplateScreen
  | MenuScreen Screen
  | HelpScreen Screen
  deriving (Eq, Ord, Show)

data FlashMessageEvent
  = RemoveFlashMessage !Int
  deriving (Eq, Show)

data Event
  = FlashEvent !FlashMessageEvent
  | CurrentQueueAttributes !(Maybe QueueAttributes)
  deriving (Eq, Show)

data SendMessageData = SendMessageData
  { _queueUrl :: !QueueUrl,
    _message :: !Text
  }
  deriving (Eq, Show, Generic)

data LoadTemplateData = LoadTemplateData
  { _chosenTemplate :: !(Maybe MessageTemplate),
    templates :: !(Vector MessageTemplate)
  }
  deriving (Eq, Show)

data FlashMessage
  = FlashSuccess !Text
  | FlashError !Text
  deriving (Eq, Show)

data MenuItem
  = MenuHelp
  | MenuQuit
  deriving (Eq, Show)

data MenuZipper = MenuZipper
  { _menuUp :: ![MenuItem],
    _menuFocus :: !MenuItem,
    _menuDown :: ![MenuItem]
  }
  deriving (Eq)

data UIState = UIState
  { _screen :: !Screen,
    sendMessageForm :: !(Form SendMessageData Event Name),
    loadTemplateForm :: !(Form LoadTemplateData Event Name),
    _awsEnv :: !AWS.Env,
    _flashMessages :: !(Map Int FlashMessage),
    _flashMessageCounter :: !Int,
    _queueAttributes :: !(Maybe QueueAttributes),
    _eventChannel :: !(BChan Event),
    _currentQueueUrlRef :: !(TVar (Maybe QueueUrl)),
    _menuZipper :: !MenuZipper
  }

foldMapM makeLenses [''UIState, ''SendMessageData, ''MenuZipper, ''LoadTemplateData]
