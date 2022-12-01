import System.Random

flipCoin :: IO Int
flipCoin = getStdRandom (randomR (0,1))

avgdev :: Int -> Int -> IO Double
avgdev k n = do 
  let ex = map (\_ -> do 
        let vec = map (\_ -> flipCoin) [1..n]
        x <- sequence vec
        let s = abs $ ((fromIntegral $ sum x) :: Double) - (fromIntegral n :: Double) / 2.0
        return s) [1..k]
  a <- sequence ex
  let s = sum a :: Double
  return $ s / (fromIntegral k :: Double)