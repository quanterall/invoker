module Run (run) where

import Brick.BChan (newBChan, writeBChan)
import Import
import qualified Network.AWS as AWS
import Network.AWS.QAWS (loadAWSEnvironment)
import qualified Network.AWS.QAWS.SQS as SQS
import Network.AWS.QAWS.SQS.Types
import RIO.FilePath ((</>))
import qualified Templates
import qualified UI

run :: Options -> RIO App ()
run Options {defaultQueueUrl, environmentFile} = do
  env <- loadAWSEnvironment environmentFile
  _eventChannel <- liftIO $ newBChan 20
  templates' <- Templates.loadTemplates `catchIO` \_ -> pure []
  defaultQueueUrlFromFile <- (Just <$> loadQueueUrlFromFile) `catchIO` const (pure Nothing)
  defaultQueueUrlFromEnv <-
    catching
      _ReadEnvironmentMissingValue
      (Just <$> readEnvironmentVariable (EnvironmentKey "QUEUE_URL"))
      $ const $ pure Nothing
  let defaultUrl = defaultQueueUrl <|> defaultQueueUrlFromEnv <|> defaultQueueUrlFromFile
  currentQueueUrlRef <- newTVarIO defaultUrl
  _queueAttributeThread <- liftIO $
    async $
      forever $ do
        currentQueueUrl <- readTVarIO currentQueueUrlRef
        forM_ currentQueueUrl $ \url -> do
          maybeAttributes <-
            catching AWS._Error (Just <$> SQS.getQueueAttributes' env url) $ const $ pure Nothing
          writeBChan _eventChannel (UI.CurrentQueueAttributes maybeAttributes)
        threadDelay $ 2 * 1000 * 1000
  UI.startUI currentQueueUrlRef _eventChannel env defaultUrl templates'

loadQueueUrlFromFile :: (MonadIO m) => m QueueUrl
loadQueueUrlFromFile = do
  QueueUrl <$> readFileUtf8 (".invoker" </> "queue")
