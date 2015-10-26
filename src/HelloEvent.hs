-- Example 3
-- Hello event, with an intermediate (2 second) pause

import FRP.Yampa as Y
import YampaUtils

initialise :: IO ()
initialise = putStrLn "Hello..."

output :: Bool -> IO Bool
output b = if b then putStrLn "...Event!" >> return True else return False

-- twoElapsed generates an event after two seconds
twoElapsed :: SF a (Event ())
twoElapsed = time >>> arr (>=2) >>> edge

-- waitTwo outputs false, but upon receiving an event from
-- twoElapsed, switches to output True
waitTwo :: SF () Bool
waitTwo = ((constant False) &&& twoElapsed) `switch` (\_ -> constant True)

main :: IO ()
main = yampaMain initialise (return Nothing) output waitTwo
