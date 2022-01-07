module Run (run) where

import Brick.BChan (newBChan, writeBChan)
import Import
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
    (Just <$> readEnvironmentVariable (EnvironmentKey "QUEUE_URL"))
      `catchAny` const (pure Nothing)
  let defaultUrl = defaultQueueUrl <|> defaultQueueUrlFromEnv <|> defaultQueueUrlFromFile
  currentQueueUrlRef <- liftIO $ newTVarIO defaultUrl
  _queueAttributeThread <- liftIO $
    async $
      forever $ do
        currentQueueUrl <- readTVarIO currentQueueUrlRef
        forM_ currentQueueUrl $ \url -> do
          response <- SQS.getQueueAttributes' env url
          let maybeAttributes = either (const Nothing) Just response
          writeBChan _eventChannel (UI.CurrentQueueAttributes maybeAttributes)
        threadDelay $ 2 * 1000 * 1000
  UI.startUI currentQueueUrlRef _eventChannel env defaultUrl templates'

loadQueueUrlFromFile :: (MonadIO m) => m QueueUrl
loadQueueUrlFromFile = do
  QueueUrl <$> readFileUtf8 (".invoker" </> "queue")
