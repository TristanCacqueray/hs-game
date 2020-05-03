-- | Abstract pulse-simple
module Game.Sound
  ( open,
    close,
    play,
    encodeAudio,
    runPlayer,
    runReader,
    runProducer,
  )
where

import Codec.Audio.Wave
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString.Lazy
import Game.Env
import Sound.Pulse.Simple
import System.IO

-- Pulse Simple interface
open :: Direction -> Maybe Int -> IO Simple
open dir fragment =
  simpleNew
    Nothing
    "hs-play"
    dir
    Nothing
    "demo code application"
    (SampleSpec (F32 LittleEndian) 44100 1)
    Nothing
    bufferAttr
  where
    bufferAttr = case fragment of
      Just _ -> Just (BufferAttr Nothing Nothing Nothing Nothing fragment)
      Nothing -> Nothing

openWrite :: IO Simple
openWrite = open Play Nothing

close :: Simple -> IO ()
close = simpleFree

play :: Simple -> [Float] -> IO ()
play = simpleWrite

playRaw :: Simple -> BS.ByteString -> IO ()
playRaw = simpleWriteRaw

readRaw :: Simple -> Int -> IO BS.ByteString
readRaw = simpleReadRaw

-- Audio utility
decodeAudio :: ByteString -> [Float]
decodeAudio = runGet getFloats
  where
    getFloats :: Get [Float]
    getFloats = do
      empty' <- isEmpty
      if empty'
        then return []
        else do
          float <- getFloat
          floats <- getFloats
          return (float : floats)
    getFloat :: Get Float
    getFloat = getFloatle

encodeAudio :: [Float] -> ByteString
encodeAudio = runPut . doPut
  where
    doPut :: [Float] -> Put
    doPut [] = return ()
    doPut (x : xs) = do
      putFloatle x
      doPut xs

-- Play generated audio (take from env)
runPlayer :: Env -> IO ()
runPlayer env = openWrite >>= go
  where
    go :: Simple -> IO ()
    go s = do
      print "Playing..."
      buf <- takeAudio env
      play s buf
      -- TODO: Check if audio producer is too slow
      go s

-- Read audio input (put to env)
runReader :: Env -> IO ()
runReader env = open Record (Just audioChunkLen) >>= go
  where
    audioChunkLen = audioChunk env

    go :: Simple -> IO ()
    go s = do
      audioBuf <- simpleRead s audioChunkLen
      -- TODO: Check if audio reader is too slow
      putAudioIn env audioBuf
      go s

maxF :: [Float] -> Float
maxF = max' 0.0
  where
    max' m [] = m
    max' m (x : xs) = max' (if m > x then m else x) xs

-- Read audio file (put to env)
runProducer :: Env -> Maybe String -> IO ()
runProducer env fn = do
  print fn
  case fn of
    Just f -> do
      buf <- readWav f
      go (readFileChunk buf)
    Nothing -> go render
  where
    render :: IO [Float]
    render = do
      let envAudioChunk = audioPlayChunk env
      let envFreq = audioFreq env
      posVal <- get (pos env)
      volVal <- get (volume env)

      let buf =
            [ (*) volVal
                $ sin
                $ 2
                  * pi
                  * 440
                  * (fromIntegral t / fromIntegral envFreq)
              | t <-
                  [ posVal
                    .. ( posVal + envAudioChunk - 1 -- (-1) to avoid overlap
                       )
                  ]
            ] ::
              [Float]
      return buf

    readWav :: String -> IO [Float]
    readWav fn' = do
      waveFile <- readWaveFile fn'
      print $ "Duration: " <> show (waveDuration waveFile)
      --      print $ "Pos: " <> show (waveDataOffset waveFile)
      openf <- openFile fn' ReadMode
      Data.ByteString.Lazy.hGetContents openf
      -- TODO: decode audio
      return [0.0]

    readFileChunk :: [Float] -> IO [Float]
    readFileChunk _ = render
    -- TODO: get actual audio chunk
    --      let envAudioChunk = audioPlayChunk env
    --      return ([], Data.ByteString.Lazy.take (fromInteger (toInteger  (envAudioChunk * 4))) buf)

    go :: IO [Float] -> IO ()
    go producer = do
      posVal <- get (pos env)
      audioBuf <- producer
      print $ "Reading: " <> show posVal
      putAudio env audioBuf
      putAudioIn env audioBuf
      update (pos env) (posVal + audioPlayChunk env)
      go producer
