import FRP.Yampa as Y
import YampaUtils

-- Compile with ghc -XArrows askAndPause.hs

-- Our signal function will be of type SF Double Bool
sigFun :: SF Double Bool
sigFun = proc inp -> do
  t <- time -< inp
  b <- arr (uncurry (>)) -< (t, inp)
  returnA -< b

output :: Bool -> IO Bool
output False = return False -- Continue the reactimate
output True = putStrLn "Done" >> return True

main :: IO ()
main = do
  putStrLn "Enter a time to wait:"
  ins <- getLine
  yampaMain (return $ read ins) (return Nothing) output sigFun
