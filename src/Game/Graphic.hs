-- |

module Game.Graphic where

import Control.Concurrent
import Control.Monad (forever)
import Game.Env
import Game.Clock

runGraphic :: Env -> IO ()
runGraphic env = initClock 25 >>= go
  where
    go :: Clock -> IO ()
    go c = do
      c <- tick c
      audio <- takeAudioIn env
      print $ "Current audio max:" <> show (maximum audio)
      go c
