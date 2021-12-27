module Run (run) where

import Brick.BChan (newBChan, writeBChan)
import Import
import Network.AWS.QAWS
import qualified Network.AWS.QAWS.SQS as SQS
import Network.AWS.QAWS.SQS.Types
import Qtility.Data (hush)
import Qtility.Environment
import Qtility.Environment.Types
import RIO.FilePath ((</>))
import Templates
import UI

run :: Options -> RIO App ()
run Options {defaultQueueUrl, environmentFile} = do
  awsEnv' <- liftIO $ loadAWSEnvironment environmentFile
  _eventChannel <- liftIO $ newBChan 20

  case awsEnv' of
    Left e -> do
      logError $ displayShow e
      exitFailure
    Right env -> do
      templates' <- liftIO loadTemplates `catchIO` \_ -> pure []
      defaultQueueUrlFromFile <- liftIO loadQueueUrlFromFile
      defaultQueueUrlFromEnv <-
        hush <$> liftIO (loadEnvironmentVariable $ EnvironmentKey "QUEUE_URL")
      let defaultUrl =
            defaultQueueUrl
              <|> defaultQueueUrlFromEnv
              <|> either (const Nothing) Just defaultQueueUrlFromFile
      currentQueueUrlRef <- liftIO $ newTVarIO defaultUrl
      _queueAttributeThread <- liftIO $
        async $
          forever $ do
            currentQueueUrl <- readTVarIO currentQueueUrlRef
            case currentQueueUrl of
              Nothing -> pure ()
              Just queueUrl -> do
                response <- SQS.getQueueAttributes' env queueUrl
                let maybeAttributes = either (const Nothing) Just response
                writeBChan _eventChannel (CurrentQueueAttributes maybeAttributes)
            threadDelay $ 2 * 1000 * 1000
      liftIO $ startUI currentQueueUrlRef _eventChannel env defaultUrl templates'

loadQueueUrlFromFile :: IO (Either IOException QueueUrl)
loadQueueUrlFromFile = do
  fmap QueueUrl <$> tryIO (readFileUtf8 (".invoker" </> "queue"))
