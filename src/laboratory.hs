import Graphics.UI.SDL as SDL
import FRP.Yampa as Y
import YampaUtils

import SDLTools

-- It would be nice to structure this program using monads,
-- but the type of reactimate is presently too restrictive.

type Position = (Double, Double)
data Object = Object Position

data ProcessedSample = ProcessedSample [Object] Bool

wasQuitEvent :: IO Bool
wasQuitEvent = do
  events <- pollEvents
  let quitAsked = or $ map (==SDL.Quit) events
  return quitAsked

renderTo :: Surface ->  Object -> Surface -> IO Bool
renderTo surface (Object (x,y)) image =
  blitSurface image Nothing surface (Just $ Rect (round x) (round y) 0 0)

output :: Surface -> [Surface] -> ProcessedSample -> IO Bool
output surface sfs pss = do
    wipe surface
    let (ProcessedSample obs exit) = pss
    sequence $ zipWith (renderTo surface) obs sfs
    SDL.flip surface
    return exit

sigFun :: SF Bool ProcessedSample
sigFun = ((time >>>
           (arr (\x -> 300 - exp (5-x)) &&& arr (\y -> (+100).(*20) $ sin (5*y)))
           >>> arr (Object) >>> arr (\x -> [x]))
           &&& identity)
          >>> arr (uncurry(ProcessedSample))

input :: IO (Maybe Bool)
input = fmap (Just) wasQuitEvent

main = do
    SDL.init [InitEverything]

    screen <- setVideoMode 640 480 32 [SWSurface] -- We want to thread this through the application
    person  <- loadBMP "manSprite.bmp"
    
    yampaMain (return False) input (output screen [person]) sigFun

    SDL.quit
