module Run (run) where

import qualified Control.Monad.Trans.AWS as AWS
import Import
import qualified Network.AWS.Data.Text as AWS
import RIO.FilePath ((</>))
import qualified RIO.Text as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import Templates
import UI

run :: Options -> RIO App ()
run Options {defaultQueueUrl} = do
  liftIO $ loadEnvFile ".env"
  awsEnv' <- liftIO getAwsEnv
  case awsEnv' of
    Nothing -> exitFailure
    Just env -> do
      templates' <- liftIO loadTemplates `catchIO` \_ -> pure []
      defaultQueueUrlFromFile <- liftIO loadQueueUrlFromFile
      defaultQueueUrlFromEnv <- liftIO loadQueueUrlFromEnv
      let defaultUrl =
            defaultQueueUrl
              <|> defaultQueueUrlFromEnv
              <|> either (const Nothing) Just defaultQueueUrlFromFile
      liftIO $ startUI env defaultUrl templates'

-- | Loads a `.env` file if it's available, changing the current environment.
loadEnvFile :: FilePath -> IO ()
loadEnvFile path = do
  dotEnvExists <- Directory.doesFileExist path
  when dotEnvExists $ do
    dotEnvValues <- parseDotEnv ".env"
    forM_ dotEnvValues $ \(key, value) -> do
      Environment.setEnv key value

-- | Parses a `.env` file into a list of key value pairs.
parseDotEnv :: FilePath -> IO [(String, String)]
parseDotEnv filePath = do
  ( Text.lines
      >>> fmap Text.strip
      >>> filter (\l -> l /= "" && not (Text.isPrefixOf "#" l))
      >>> fmap (Text.break (== '='))
      >>> fmap (bimap sanitizeKey sanitizeValue)
    )
    <$> readFileUtf8 filePath
  where
    sanitizeKey :: Text -> String
    sanitizeKey = Text.dropWhile (`elem` [' ', '#']) >>> Text.unpack

    sanitizeValue :: Text -> String
    sanitizeValue = Text.dropWhile (== '=') >>> Text.filter (/= '"') >>> Text.unpack

loadQueueUrlFromFile :: IO (Either IOException QueueUrl)
loadQueueUrlFromFile = do
  fmap QueueUrl <$> tryIO (readFileUtf8 (".invoker" </> "queue"))

loadQueueUrlFromEnv :: IO (Maybe QueueUrl)
loadQueueUrlFromEnv = do
  queueUrlString <- Environment.getEnv "QUEUE_URL"
  pure $
    if null queueUrlString
      then Nothing
      else queueUrlString & Text.pack & QueueUrl & Just

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
