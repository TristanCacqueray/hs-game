-- | Demo
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Data.IORef
import Data.Maybe (fromMaybe)
import Game.Env (initEnv, takeAudioIn)
import Game.Sound (runReader)
import Hadertoy

smooth :: IORef Float -> Float -> IO Float
smooth ref cur = do
  prev <- readIORef ref
  let new =
        if cur > prev
          then cur
          else prev - (prev / 10)
  writeIORef ref new
  return new

updateSeed :: IORef Float -> Float -> IO Float
updateSeed ref inc = do
  val <- readIORef ref
  let new = val + inc
  writeIORef ref new
  return new

main :: IO ()
main =
  do
    (shader, version) <- readShader "./shaders/orbit.glsl"
    env <- initEnv
    withAsync (runReader env) $ \_ ->
      withGLFW (fromMaybe "120" version) $ \glfw ->
        withWindow glfw 1424 801 shader $ \win -> do
          let start = -1.3
          setEnvSeed win (start, -1.13)
          smoothAudio <- newIORef 0.0
          realSeed <- newIORef start
          run 25 (update env win smoothAudio realSeed)
  where
    update env win smoothAudio realSeed = do
      audio <- takeAudioIn env
      val <- smooth smoothAudio ((maximum audio) * 5e-3)
      print $ "Smooth audio max: " <> show val
      seedReal <- updateSeed realSeed val
      setEnvSeed win (seedReal, -1.13)
      render win
