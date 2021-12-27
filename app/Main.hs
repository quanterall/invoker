{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Import
import Network.AWS.QAWS.SQS.Types (QueueUrl (..))
import Options.Applicative.Simple
import qualified Paths_invoker
import Qtility.Environment.Types (EnvironmentFile (..))
import RIO.Process
import RIO.Text (pack)
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_invoker.version)
      "invoker"
      "A tool for sending messages to SQS queues"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> option
            (maybeReader readQueueUrl)
            ( long "queue"
                <> short 'q'
                <> help "Queue name"
                <> value Nothing
            )
          <*> option
            (maybeReader readEnvironmentFile)
            ( long "environment-file"
                <> short 'e'
                <> help "Environment file to read configuration from"
                <> value (EnvironmentFile ".env")
            )
      )
      empty
  lo <- logOptionsHandle stderr (verbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app (run options)

readQueueUrl :: String -> Maybe (Maybe QueueUrl)
readQueueUrl = pack >>> QueueUrl >>> Just >>> Just

readEnvironmentFile :: String -> Maybe EnvironmentFile
readEnvironmentFile = EnvironmentFile >>> Just
