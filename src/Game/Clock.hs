module Game.Clock
  ( Clock,
    tick,
    initClock,
  )
where

import qualified System.Clock as C

--           Clock fps time
data Clock = Clock Int Integer

getClock :: IO Integer
getClock = C.toNanoSecs <$> C.getTime C.Monotonic

initClock :: Int -> IO Clock
initClock fps = Clock fps <$> getClock

-- Count real fps
tick :: Clock -> IO Clock
tick (Clock fps prevTs) = do
  ts <- getClock
  let curFps = 1.0 / (fromIntegral (ts - prevTs) / 1e9)
  print $ "fps: " <> show (curFps :: Double)
  return (Clock fps ts)
