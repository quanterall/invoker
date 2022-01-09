{-# LANGUAGE TemplateHaskell #-}

module UI.Types where

import Brick.BChan (BChan)
import Brick.BChan.Class (HasEventChannel (..))
import Brick.Forms (Form)
import Brick.Widgets.FlashMessages.Class (AsFlashMessageEvent (..), HasFlashMessages (..))
import Brick.Widgets.FlashMessages.Types (FlashMessage, FlashMessageEvent)
import qualified Network.AWS as AWS
import Network.AWS.QAWS.SQS.Types (QueueAttributes, QueueUrl)
import Qtility
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

data Event
  = FlashEvent !FlashMessageEvent
  | CurrentQueueAttributes !(Maybe QueueAttributes)
  deriving (Eq, Show)

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

data MenuZipper = MenuZipper
  { _menuUp :: ![MenuItem],
    _menuFocus :: !MenuItem,
    _menuDown :: ![MenuItem]
  }
  deriving (Eq)

data MenuItem
  = MenuHelp
  | MenuQuit
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

makeClassyPrisms ''Event

instance AsFlashMessageEvent Event where
  _FlashMessageEvent = _FlashEvent

instance HasFlashMessages UIState where
  flashMessagesL = lens _flashMessages (\s f -> s {_flashMessages = f})
  flashMessageIdL = lens _flashMessageCounter (\s f -> s {_flashMessageCounter = f})

instance HasEventChannel UIState Event where
  eventChannelL = lens _eventChannel (\s f -> s {_eventChannel = f})

foldMapM makeLenses [''UIState, ''SendMessageData, ''MenuZipper, ''LoadTemplateData]
