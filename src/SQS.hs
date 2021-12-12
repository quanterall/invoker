module SQS where

import Network.AWS (MonadAWS)
import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as AWSSQS
import RIO
import Types

sendMessage ::
  (MonadAWS m, MonadUnliftIO m) =>
  QueueUrl ->
  Text ->
  m (Either AWS.Error (Maybe Text))
sendMessage (QueueUrl queueUrl) message = do
  maybeResponse <- try $ AWS.send $ AWSSQS.sendMessage queueUrl message
  case maybeResponse of
    Right response -> pure $ Right $ response ^. AWSSQS.smrsMessageId
    Left e -> pure $ Left e
