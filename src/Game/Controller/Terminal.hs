
module Game.Controller.Terminal (runController) where

-- import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever)
import Data.IORef (IORef, readIORef, writeIORef)
-- import Game.Clock
import Game.Env
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stdin)

runController :: Env -> IO ()
runController env = print ("Controlling" :: String) >> initTerm >> forever go
  where
    initTerm :: IO ()
    initTerm = do
      hSetEcho stdin False
      hSetBuffering stdin NoBuffering
    posRef :: IORef Int
    posRef = pos env
    go :: IO ()
    go = do
      posVal <- readIORef posRef
      volumeVal <- readIORef (volume env)
      l <- getChar
      case l of
        'a' -> writeIORef posRef (posVal - 1)
        'd' -> writeIORef posRef (posVal + 1)
        '/' -> writeIORef (volume env) (volumeVal - 0.01)
        '*' -> writeIORef (volume env) (volumeVal + 0.01)
        ' ' -> print ("NotImplemented: pause" :: String)
        'q' -> error "Over!"
        _ ->
          print $
            "Unknown: ["
              <> [l]
              <> "], press 'quit' to quit"

{-
mainThread :: Env -> IO ()
mainThread env = void $ forkIO (initClock 25 >>= go)
  where
    go :: Clock -> IO ()
    go clock = do
      nextClock <- tick clock
      pos' <- readIORef $ pos env
      print $ "Env: " <> show pos'
      writeIORef (pos env) (pos' + 1)
      threadDelay 100_000
      go nextClock

main :: IO ()
main = do
  env <- initEnv
  mainThread env
  void $ runController env
-}
