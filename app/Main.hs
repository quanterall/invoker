{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_invoker
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
