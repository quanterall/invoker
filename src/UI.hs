{-# LANGUAGE TemplateHaskell #-}

module UI (startUI) where

import Brick
import Brick.BChan
import Brick.Focus (focusRingCursor)
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as Vty
import Import hiding (App)
import Lens.Micro.TH (makeLenses)
import qualified Network.AWS as AWS
import qualified RIO.Map as Map
import RIO.Text (pack, unpack)
import RIO.Vector ((!?))
import qualified RIO.Vector as Vector
import qualified SQS
import Templates

data Name
  = QueueUrlField
  | MessageField
  | TemplateListField
  deriving (Eq, Ord, Show)

data Screen
  = SendMessageScreen
  | LoadTemplateScreen
  deriving (Eq, Ord, Show)

data Event
  = RemoveFlashMessage !Int
  deriving (Eq, Ord, Show)

data SendMessageData = SendMessageData
  { _queueUrl :: !QueueUrl,
    _message :: !Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''SendMessageData

data LoadTemplateData = LoadTemplateData
  { _chosenTemplate :: !(Maybe MessageTemplate),
    templates :: !(Vector MessageTemplate)
  }
  deriving (Eq, Show)

makeLenses ''LoadTemplateData

data FlashMessageType
  = FlashSuccess
  | FlashError
  deriving (Eq, Ord, Show)

data FlashMessage = FlashMessage
  { _flashMessageTitle :: !Text,
    _flashMessageBody :: !Text,
    _flashMessageType :: !FlashMessageType
  }
  deriving (Eq, Show)

data UIState = UIState
  { _screen :: !Screen,
    sendMessageForm :: !(Form SendMessageData Event Name),
    loadTemplateForm :: !(Form LoadTemplateData Event Name),
    _awsEnv :: !AWS.Env,
    _flashMessages :: !(Map Int FlashMessage),
    _flashMessageCounter :: !Int,
    _eventChannel :: !(BChan Event)
  }

makeLenses ''UIState

startUI :: AWS.Env -> Maybe QueueUrl -> [MessageTemplate] -> IO ()
startUI awsEnv' maybeQueueUrl templates' = do
  _eventChannel <- newBChan 20
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  let initialState =
        UIState
          { _screen = SendMessageScreen,
            sendMessageForm,
            loadTemplateForm,
            _awsEnv = awsEnv',
            _flashMessages = mempty,
            _flashMessageCounter = 0,
            _eventChannel
          }
      sendMessageForm =
        makeSendMessageForm $
          SendMessageData
            { _queueUrl = fromMaybe (QueueUrl "") maybeQueueUrl,
              _message = ""
            }
      loadTemplateForm =
        makeLoadTemplateForm $ LoadTemplateData {_chosenTemplate = templates !? 0, templates}
      templates = Vector.fromList templates'
  void $ customMain initialVty buildVty (Just _eventChannel) app initialState

app :: App UIState Event Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = chooseCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = attrMapForState
    }

handleEvent :: UIState -> BrickEvent Name Event -> EventM Name (Next UIState)
handleEvent state (AppEvent e@(RemoveFlashMessage _)) = handleFlashMessageEvent state e
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])) = halt state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl])) =
  continue $ state & screen .~ LoadTemplateScreen
handleEvent state@UIState {_screen = SendMessageScreen, sendMessageForm} event =
  handleSendMessageForm state sendMessageForm event
handleEvent state@UIState {_screen = LoadTemplateScreen, loadTemplateForm} event =
  handleLoadTemplateForm state loadTemplateForm event

handleFlashMessageEvent :: UIState -> Event -> EventM Name (Next UIState)
handleFlashMessageEvent state (RemoveFlashMessage i) =
  continue $ state & flashMessages %~ Map.delete i

handleSendMessageForm ::
  UIState ->
  Form SendMessageData Event Name ->
  BrickEvent Name Event ->
  EventM Name (Next UIState)
handleSendMessageForm state form (VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl])) = do
  let url = formState form ^. queueUrl
      message' = formState form ^. message
      awsEnv' = state ^. awsEnv
  result <- liftIO $ AWS.runResourceT $ AWS.runAWS awsEnv' $ SQS.sendMessage url message'
  case result of
    Right (Just messageId) -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashMessage "Success" ("Message sent with id: " <> messageId) FlashSuccess
      continue newState
    Right Nothing -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashMessage "Error" "Failed to send message" FlashError
      continue newState
    Left err -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashMessage "Error" (awsErrorToText err) FlashError
      continue newState
handleSendMessageForm state form event = do
  formState <- handleFormEvent event form
  continue $ state {sendMessageForm = formState}

handleLoadTemplateForm ::
  UIState ->
  Form LoadTemplateData Event Name ->
  BrickEvent Name Event ->
  EventM Name (Next UIState)
handleLoadTemplateForm s form (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = do
  reloadedTemplates <- liftIO $ loadTemplates `catchIO` \_ -> pure []
  let newState = s {loadTemplateForm = newForm'}
      oldFormState = formState form
      newForm' =
        makeLoadTemplateForm (oldFormState {templates, _chosenTemplate = templates !? 0})
      templates = Vector.fromList reloadedTemplates
  continue newState
handleLoadTemplateForm state form (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  let formData = formState form
  continue $ case _chosenTemplate formData of
    Nothing -> state
    Just template -> do
      let newFormState = formState (sendMessageForm state) & message .~ _templateMessage template
          newForm' = updateFormState newFormState (sendMessageForm state)
      state {sendMessageForm = newForm'} & screen .~ SendMessageScreen
handleLoadTemplateForm state form event = do
  formState <- handleFormEvent event form
  continue $ state {loadTemplateForm = formState}

drawUI :: UIState -> [Widget Name]
drawUI state@UIState {_screen = SendMessageScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawSendMessageScreen state]
drawUI state@UIState {_screen = LoadTemplateScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawLoadTemplateScreen state]

drawFlashMessages :: Map Int FlashMessage -> Widget Name
drawFlashMessages m = vBox $ map drawFlashMessage $ Map.elems m

drawFlashMessage :: FlashMessage -> Widget Name
drawFlashMessage FlashMessage {_flashMessageTitle, _flashMessageBody, _flashMessageType} = do
  let flashAttribute =
        case _flashMessageType of
          FlashSuccess -> flashSuccessAttr
          FlashError -> flashErrorAttr
  withAttr flashAttribute $
    padAll 1 $
      borderWithLabel (str $ unpack _flashMessageTitle) (str $ unpack _flashMessageBody)

drawSendMessageScreen :: UIState -> Widget Name
drawSendMessageScreen UIState {sendMessageForm} = renderForm sendMessageForm

drawLoadTemplateScreen :: UIState -> Widget Name
drawLoadTemplateScreen UIState {loadTemplateForm} = do
  let templateText =
        maybe "" (_templateMessage >>> unpack) $ _chosenTemplate $ formState loadTemplateForm
  hBox
    [ hLimit 32 (renderForm loadTemplateForm),
      borderWithLabel (str "Content") (hCenter $ str templateText <=> fill ' ')
    ]

makeSendMessageForm :: SendMessageData -> Form SendMessageData e Name
makeSendMessageForm =
  newForm
    [ borderWithLabel (str "Queue URL")
        @@= editTextField (queueUrl . queueUrl') QueueUrlField (Just 1),
      borderWithLabel (str "Message") @@= editTextField message MessageField Nothing
    ]

makeLoadTemplateForm :: LoadTemplateData -> Form LoadTemplateData e Name
makeLoadTemplateForm =
  newForm
    [ borderWithLabel (str "Template")
        @@= listField templates chosenTemplate render' 1 TemplateListField
    ]
  where
    render' isFocused (MessageTemplate name _) =
      if isFocused
        then withAttr focusedFormInputAttr $ str $ unpack name
        else str $ unpack name

attrMapForState :: UIState -> AttrMap
attrMapForState _state =
  attrMap
    Vty.defAttr
    [ (focusedFormInputAttr, Vty.black `Brick.on` Vty.yellow),
      (invalidFormInputAttr, Vty.white `Brick.on` Vty.red),
      (flashSuccessAttr, Vty.white `Brick.on` Vty.green),
      (flashErrorAttr, Vty.white `Brick.on` Vty.red)
    ]

chooseCursor :: UIState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor UIState {_screen = SendMessageScreen, sendMessageForm} =
  focusRingCursor formFocus sendMessageForm
chooseCursor UIState {_screen = LoadTemplateScreen, loadTemplateForm} =
  focusRingCursor formFocus loadTemplateForm

addFlashMessage :: UIState -> FlashMessage -> IO UIState
addFlashMessage state msg = do
  let currentId = state ^. flashMessageCounter + 1
  async $ do
    threadDelay $ 4 * 1000 * 1000
    writeBChan (state ^. eventChannel) $ RemoveFlashMessage currentId
  pure $
    state
      & flashMessages %~ Map.insert currentId msg
      & flashMessageCounter %~ (+ 1)

flashSuccessAttr :: AttrName
flashSuccessAttr = "flash-success"

flashErrorAttr :: AttrName
flashErrorAttr = "flash-error"

awsErrorToText :: AWS.Error -> Text
awsErrorToText (AWS.TransportError httpException) =
  tshow httpException
awsErrorToText (AWS.SerializeError AWS.SerializeError' {AWS._serializeMessage = e}) =
  pack e
awsErrorToText
  ( AWS.ServiceError
      AWS.ServiceError' {AWS._serviceMessage = Just (AWS.ErrorMessage e)}
    ) =
    e
awsErrorToText
  ( AWS.ServiceError
      AWS.ServiceError'
        { AWS._serviceMessage = Nothing,
          AWS._serviceCode = AWS.ErrorCode code
        }
    ) =
    "Unknown service error: " <> code
