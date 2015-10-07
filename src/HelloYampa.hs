-- Example 1
-- Hello world, with an intermediate (2 second) pause

import FRP.Yampa as Y
import YampaUtils

initialise :: IO ()
initialise = putStrLn "Hello..."

output :: Bool -> IO Bool
output b = if b then putStrLn "...Yampa!" >> return True else return False

waitTwo :: SF () Bool
waitTwo = time >>> arr (>=2)

main :: IO ()
main = yampaMain initialise (return Nothing) output waitTwo
