module Run (run) where

import qualified Control.Monad.Trans.AWS as AWS
import Import
import qualified Network.AWS.Data.Text as AWS
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import qualified System.Environment as Environment
import Templates
import UI

run :: Options -> RIO App ()
run Options {defaultQueueUrl} = do
  awsEnv' <- liftIO getAwsEnv
  case awsEnv' of
    Nothing -> exitFailure
    Just env -> do
      templates' <- liftIO loadTemplates `catchIO` \_ -> pure []
      defaultQueueUrlFromFile <- liftIO loadQueueUrl
      let defaultUrl = defaultQueueUrl <|> either (const Nothing) Just defaultQueueUrlFromFile
      liftIO $ startUI env defaultUrl templates'

loadQueueUrl :: IO (Either IOException QueueUrl)
loadQueueUrl = do
  fmap QueueUrl <$> tryIO (readFileUtf8 (".invoker" </> "queue"))

getAwsEnv :: IO (Maybe AWS.Env)
getAwsEnv = do
  region <- fromTextOrFail "REGION"
  accessKeyId <- fromTextOrFail "AWS_ACCESS_KEY_ID"
  secretAccessKey <- fromTextOrFail "AWS_SECRET_ACCESS_KEY"
  awsEnv' <- AWS.newEnv (AWS.FromKeys accessKeyId secretAccessKey)
  let modifiedAwsEnv = awsEnv' & AWS.envRegion .~ region

  pure $ Just modifiedAwsEnv

fromTextOrFail :: (AWS.FromText a) => String -> IO a
fromTextOrFail key = do
  maybeValue <- (Text.pack >>> AWS.fromText) <$> Environment.getEnv key
  either error pure maybeValue
