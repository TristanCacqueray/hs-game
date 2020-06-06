module Game.Env
  ( Env (..),
    initEnv,
    get,
    update,
    readAudio,
    writeAudio,
    putAudio,
    putAudioIn,
    takeAudio,
    takeAudioIn,
    audioChunk,
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, swapMVar, takeMVar, tryTakeMVar)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

get :: IORef a -> IO a
get = readIORef

update :: IORef a -> a -> IO ()
update = writeIORef

readAudio :: MVar a -> IO a
readAudio = readMVar

writeAudio :: MVar a -> a -> IO ()
writeAudio v a = void $ swapMVar v a

data Env = Env
  { fps :: Int,
    audioFreq :: Int,
    audioPlayChunk :: Int,
    pos :: IORef Int,
    volume :: IORef Float,
    audio :: MVar [Float],
    audioIn :: MVar [Float]
  }

putAudio :: Env -> [Float] -> IO ()
putAudio env = putMVar (audio env)

putAudioIn :: Env -> [Float] -> IO ()
putAudioIn env = putMVar (audioIn env)

takeAudio :: Env -> IO [Float]
takeAudio env = takeMVar (audio env)

takeAudioIn :: Env -> IO [Float]
takeAudioIn env =
  do
    init <- takeMVar (audioIn env)
    go init
  where
    go cur = do
      tryNext <- tryTakeMVar (audioIn env)
      case tryNext of
        Nothing -> return cur
        Just a -> do
          putStrLn "Not reading fast enough..."
          go a

audioChunk :: Env -> Int
audioChunk env = (audioFreq env) `div` (fps env)

initEnv :: IO Env
initEnv = Env defaultFps freq audioPlay <$> newIORef 0 <*> newIORef 0.1 <*> newEmptyMVar <*> newEmptyMVar
  where
    freq = 44100
    --    audioPlay = freq `div` 2
    defaultFps = 25
    audioPlay = freq `div` defaultFps
