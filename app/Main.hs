module Main
  ( main,
  )
where

import Control.Concurrent.Async (wait, withAsync)
import Game.Controller.Terminal
import Game.Env
import Game.Sound
import Game.Graphic
import System.Environment

test = do
  n <- getLine
  print n

main :: IO ()
main = do
  args <- getArgs
  env <- initEnv
  if length args == 1
    then mainPlayer env (Just $ head args)
    else mainReader env
  where
    -- Read an audio input
    mainReader env = withAsync (runReader env) $ \_ -> mainLoop env
    -- Play an audio file
    mainPlayer env arg =
      withAsync (runPlayer env) $
        \_ -> withAsync (runProducer env arg) $
          \_ -> mainLoop env
    -- The main loop
    mainLoop env = withAsync (runController env) $
      \keyboard -> withAsync (runGraphic env) $
        \_ -> do
          print "Running"
          wait keyboard
