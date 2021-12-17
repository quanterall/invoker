{-# LANGUAGE TemplateHaskell #-}

module UI (startUI, Event (..)) where

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
import SQS (QueueAttributes (..))
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
  | MenuScreen Screen
  | HelpScreen Screen
  deriving (Eq, Ord, Show)

data FlashMessageEvent
  = RemoveFlashMessage !Int
  deriving (Eq, Show)

data Event
  = FlashEvent !FlashMessageEvent
  | CurrentQueueAttributes !(Maybe SQS.QueueAttributes)
  deriving (Eq, Show)

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

makeLenses ''MenuZipper

data UIState = UIState
  { _screen :: !Screen,
    sendMessageForm :: !(Form SendMessageData Event Name),
    loadTemplateForm :: !(Form LoadTemplateData Event Name),
    _awsEnv :: !AWS.Env,
    _flashMessages :: !(Map Int FlashMessage),
    _flashMessageCounter :: !Int,
    _queueAttributes :: !(Maybe SQS.QueueAttributes),
    _eventChannel :: !(BChan Event),
    _currentQueueUrlRef :: !(TVar (Maybe QueueUrl)),
    _menuZipper :: !MenuZipper
  }

makeLenses ''UIState

startUI :: TVar (Maybe QueueUrl) -> BChan Event -> AWS.Env -> Maybe QueueUrl -> [MessageTemplate] -> IO ()
startUI _currentQueueUrlRef _eventChannel awsEnv' maybeQueueUrl templates' = do
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
            _queueAttributes = Nothing,
            _eventChannel,
            _currentQueueUrlRef,
            _menuZipper = newMenuZipper
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
handleEvent state (AppEvent (FlashEvent e)) = handleFlashMessageEvent state e
handleEvent state (AppEvent (CurrentQueueAttributes maybeAttributes)) = do
  continue $ state & queueAttributes .~ maybeAttributes
handleEvent state (VtyEvent (Vty.EvKey (Vty.KFun 2) [])) =
  continue $ state & screen .~ MenuScreen (state ^. screen)
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])) = halt state
handleEvent state (VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl])) =
  continue $ state & screen .~ LoadTemplateScreen
handleEvent state@UIState {_screen = SendMessageScreen, sendMessageForm} event =
  handleSendMessageForm state sendMessageForm event
handleEvent state@UIState {_screen = LoadTemplateScreen, loadTemplateForm} event =
  handleLoadTemplateForm state loadTemplateForm event
handleEvent state@UIState {_screen = MenuScreen previousScreen, _menuZipper} event =
  handleMenuScreen state event previousScreen
handleEvent state@UIState {_screen = HelpScreen previousScreen, _menuZipper} event =
  handleHelpScreen state event previousScreen

handleFlashMessageEvent :: UIState -> FlashMessageEvent -> EventM Name (Next UIState)
handleFlashMessageEvent state (RemoveFlashMessage i) =
  continue $ state & flashMessages %~ Map.delete i

handleSendMessageForm ::
  UIState ->
  Form SendMessageData Event Name ->
  BrickEvent Name Event ->
  EventM Name (Next UIState)
handleSendMessageForm state form (VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl])) =
  handleSendMessage state form
handleSendMessageForm state form (VtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl])) =
  handlePurgeQueue state form
handleSendMessageForm state form event = do
  formState <- handleFormEvent event form
  continue $ state {sendMessageForm = formState}

handleSendMessage ::
  UIState ->
  Form SendMessageData Event Name ->
  EventM Name (Next UIState)
handleSendMessage state form = do
  let url = formState form ^. queueUrl
      message' = formState form ^. message
      awsEnv' = state ^. awsEnv
  result <- liftIO $ AWS.runResourceT $ AWS.runAWS awsEnv' $ SQS.sendMessage url message'
  liftIO $ atomically $ writeTVar (state ^. currentQueueUrlRef) (Just url)
  case result of
    Right (Just messageId) -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashSuccess $ "Message sent with id: " <> messageId
      continue newState
    Right Nothing -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashError "Failed to send message"
      continue newState
    Left err -> do
      newState <-
        liftIO $
          addFlashMessage state $
            FlashError $ awsErrorToText err
      continue newState

handlePurgeQueue :: UIState -> Form SendMessageData Event Name -> EventM Name (Next UIState)
handlePurgeQueue state form = do
  let url = formState form ^. queueUrl
  liftIO $ atomically $ writeTVar (state ^. currentQueueUrlRef) (Just url)
  result <- liftIO $ AWS.runResourceT $ AWS.runAWS (state ^. awsEnv) $ SQS.purgeQueue url
  newState <- case result of
    Right () -> do
      liftIO $ addFlashMessage state $ FlashSuccess "Queue purged"
    Left err -> do
      liftIO $ addFlashMessage state $ FlashError $ awsErrorToText err
  continue newState

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

handleMenuScreen ::
  UIState ->
  BrickEvent Name Event ->
  Screen ->
  EventM Name (Next UIState)
handleMenuScreen state (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) _previousScreen =
  continue $ state & menuZipper %~ menuFocusDown
handleMenuScreen state (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) _previousScreen =
  continue $ state & menuZipper %~ menuFocusUp
handleMenuScreen state (VtyEvent (Vty.EvKey Vty.KEsc [])) previousScreen =
  continue $ state & screen .~ previousScreen
handleMenuScreen state (VtyEvent (Vty.EvKey Vty.KEnter [])) previousScreen =
  state ^. menuZipper . menuFocus & menuItemEnter state previousScreen
handleMenuScreen state _event _previousScreen =
  continue state

handleHelpScreen ::
  UIState ->
  BrickEvent Name Event ->
  Screen ->
  EventM Name (Next UIState)
handleHelpScreen state (VtyEvent (Vty.EvKey Vty.KEsc [])) previousScreen =
  continue $ state & screen .~ previousScreen
handleHelpScreen state _event _previousScreen =
  continue state

drawUI :: UIState -> [Widget Name]
drawUI state@UIState {_screen = SendMessageScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawSendMessageScreen state]
drawUI state@UIState {_screen = LoadTemplateScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawLoadTemplateScreen state]
drawUI state@UIState {_screen = MenuScreen _previousScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawMenuScreen state]
drawUI UIState {_screen = HelpScreen _previousScreen, _flashMessages} =
  [centerLayer $ drawFlashMessages _flashMessages, drawHelpScreen]

drawFlashMessages :: Map Int FlashMessage -> Widget Name
drawFlashMessages m = vBox $ map drawFlashMessage $ Map.elems m

drawFlashMessage :: FlashMessage -> Widget Name
drawFlashMessage flashMessage = do
  let (flashAttribute, title, body) =
        case flashMessage of
          FlashSuccess b ->
            (flashSuccessAttr, "Success", b)
          FlashError b ->
            (flashErrorAttr, "Error", b)
  withAttr flashAttribute $
    padAll 1 $
      borderWithLabel (str $ unpack title) (str $ unpack body)

drawSendMessageScreen :: UIState -> Widget Name
drawSendMessageScreen UIState {sendMessageForm, _queueAttributes} =
  vBox [renderQueueAttributes _queueAttributes, renderForm sendMessageForm]

renderQueueAttributes :: Maybe QueueAttributes -> Widget Name
renderQueueAttributes Nothing = emptyWidget
renderQueueAttributes
  ( Just
      QueueAttributes
        { queueAttributesMessages,
          queueAttributesDelayedMessages,
          queueAttributesNotVisibleMessages
        }
    ) = do
    borderWithLabel (str "Queue Attributes") $
      hCenter $
        hBox
          [ str "Messages: ",
            str $ maybe "N/A" show queueAttributesMessages,
            str " | ",
            str "Delayed: ",
            str $ maybe "N/A" show queueAttributesDelayedMessages,
            str " | ",
            str "Not Visible: ",
            str $ maybe "N/A" show queueAttributesNotVisibleMessages
          ]

drawLoadTemplateScreen :: UIState -> Widget Name
drawLoadTemplateScreen UIState {loadTemplateForm} = do
  let templateText =
        maybe "" (_templateMessage >>> unpack) $ _chosenTemplate $ formState loadTemplateForm
  hBox
    [ hLimit 32 (renderForm loadTemplateForm),
      borderWithLabel (str "Content") (hCenter $ str templateText <=> fill ' ')
    ]

drawMenuScreen :: UIState -> Widget Name
drawMenuScreen UIState {_menuZipper} =
  renderMenuZipper _menuZipper

drawHelpScreen :: Widget Name
drawHelpScreen =
  borderWithLabel (str "Help") $
    hCenter $
      vBox
        [ str "Ctrl + s: Send message to queue",
          str "Ctrl + t: Load a template into the message field",
          str "F2: Show menu",
          str "Ctrl + q: Quit"
        ]

renderMenuZipper :: MenuZipper -> Widget Name
renderMenuZipper zipper = do
  let upItems = zipper ^. menuUp & map (renderMenuItem False)
      downItems = zipper ^. menuDown & map (renderMenuItem False)
      focusItem = zipper ^. menuFocus & renderMenuItem True
  borderWithLabel (str "Menu") $
    hCenter (vBox $ upItems <> [focusItem] <> downItems) <=> fill ' '

renderMenuItem :: Bool -> MenuItem -> Widget Name
renderMenuItem isFocused item = do
  let itemText = case item of
        MenuHelp -> "Help"
        MenuQuit -> "Quit"
  withAttr (if isFocused then menuItemFocusAttr else menuItemAttr) $ str itemText

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
      (flashErrorAttr, Vty.white `Brick.on` Vty.red),
      (menuItemAttr, fg Vty.white),
      (menuItemFocusAttr, Vty.black `Brick.on` Vty.yellow)
    ]

chooseCursor :: UIState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor UIState {_screen = SendMessageScreen, sendMessageForm} =
  focusRingCursor formFocus sendMessageForm
chooseCursor UIState {_screen = LoadTemplateScreen, loadTemplateForm} =
  focusRingCursor formFocus loadTemplateForm
chooseCursor UIState {_screen = MenuScreen _previousScreen} =
  const Nothing
chooseCursor UIState {_screen = HelpScreen _previousScreen} =
  const Nothing

addFlashMessage :: UIState -> FlashMessage -> IO UIState
addFlashMessage state msg = do
  let currentId = state ^. flashMessageCounter + 1
  async $ do
    threadDelay $ 4 * 1000 * 1000
    writeBChan (state ^. eventChannel) $ FlashEvent $ RemoveFlashMessage currentId
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

menuItemEnter :: UIState -> Screen -> MenuItem -> EventM Name (Next UIState)
menuItemEnter state previousScreen MenuHelp =
  continue $ state & screen .~ HelpScreen previousScreen
menuItemEnter state _previousScreen MenuQuit =
  halt state

newMenuZipper :: MenuZipper
newMenuZipper = MenuZipper {_menuUp = [], _menuFocus = MenuHelp, _menuDown = [MenuQuit]}

menuFocusUp :: MenuZipper -> MenuZipper
menuFocusUp MenuZipper {_menuUp = [], _menuFocus, _menuDown} =
  MenuZipper {_menuUp = [], _menuFocus, _menuDown}
menuFocusUp MenuZipper {_menuUp = (u : up), _menuFocus, _menuDown = down} =
  MenuZipper {_menuUp = up, _menuFocus = u, _menuDown = _menuFocus : down}

menuFocusDown :: MenuZipper -> MenuZipper
menuFocusDown MenuZipper {_menuUp, _menuFocus, _menuDown = []} =
  MenuZipper {_menuUp, _menuFocus, _menuDown = []}
menuFocusDown MenuZipper {_menuUp, _menuFocus, _menuDown = (d : down)} =
  MenuZipper {_menuUp = _menuFocus : _menuUp, _menuFocus = d, _menuDown = down}

menuItemFocusAttr :: AttrName
menuItemFocusAttr = "menu-item-focus"

menuItemAttr :: AttrName
menuItemAttr = "menu-item"
