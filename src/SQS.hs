module SQS where

import Network.AWS (MonadAWS)
import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as AWSSQS
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as Text
import Types

data QueueAttributes = QueueAttributes
  { queueAttributesArn :: !(Maybe Text),
    queueAttributesUrl :: !QueueUrl,
    queueAttributesMessages :: !(Maybe Int),
    queueAttributesDelayedMessages :: !(Maybe Int),
    queueAttributesNotVisibleMessages :: !(Maybe Int)
  }
  deriving (Eq, Show)

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

getQueueAttributes ::
  (MonadAWS m, MonadUnliftIO m) =>
  QueueUrl ->
  m (Either AWS.Error QueueAttributes)
getQueueAttributes queueUrl = do
  let command =
        queueUrl & _queueUrl' & AWSSQS.getQueueAttributes & AWSSQS.gqaAttributeNames
          .~ [ AWSSQS.QANQueueARN,
               AWSSQS.QANApproximateNumberOfMessages,
               AWSSQS.QANApproximateNumberOfMessagesNotVisible,
               AWSSQS.QANApproximateNumberOfMessagesDelayed
             ]
  maybeResponse <- command & AWS.send & try
  case maybeResponse of
    Right response -> pure $ Right $ createQueueAttributes queueUrl response
    Left e -> pure $ Left e

createQueueAttributes :: QueueUrl -> AWSSQS.GetQueueAttributesResponse -> QueueAttributes
createQueueAttributes queueUrl response =
  let m = response ^. AWSSQS.gqarsAttributes
      queueAttributesArn = HashMap.lookup AWSSQS.QANQueueARN m
      queueAttributesMessages =
        HashMap.lookup AWSSQS.QANApproximateNumberOfMessages m >>= treadMaybe
      queueAttributesDelayedMessages =
        HashMap.lookup AWSSQS.QANApproximateNumberOfMessagesDelayed m >>= treadMaybe
      queueAttributesNotVisibleMessages =
        HashMap.lookup AWSSQS.QANApproximateNumberOfMessagesNotVisible m >>= treadMaybe
   in QueueAttributes
        { queueAttributesArn,
          queueAttributesUrl = queueUrl,
          queueAttributesMessages,
          queueAttributesDelayedMessages,
          queueAttributesNotVisibleMessages
        }

treadMaybe :: (Read a) => Text -> Maybe a
treadMaybe = Text.unpack >>> readMaybe
