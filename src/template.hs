import Graphics.UI.SDL as SDL
import FRP.Yampa as Y
import YampaUtils

import SDLTools

type Position = (Double, Double)
data Object = Object Position

data ProcessedSample = ProcessedSample [Object] Bool

-- input :: IO (Maybe ..)
-- input = ..

output :: Surface -> [Surface] -> ProcessedSample -> IO Bool
output surface sfs pss = do
    wipe surface
    let (ProcessedSample obs exit) = pss
    sequence $ zipWith (renderTo surface) obs sfs
    SDL.flip surface
    return exit

-- sigFun :: SF ...
-- sigFun = 

main = do
    SDL.init [InitEverything]
    screen <- setVideoMode 640 480 32 [SWSurface]
    person  <- loadBMP "manSprite.bmp"
    
    yampaMain (return False) input (output screen [person]) sigFun

    SDL.quit

wasQuitEvent :: IO Bool
wasQuitEvent = do
  events <- pollEvents
  let quitAsked = or $ map (==SDL.Quit) events
  return quitAsked

renderTo :: Surface ->  Object -> Surface -> IO Bool
renderTo surface (Object (x,y)) image =
  blitSurface image Nothing surface (Just $ Rect (round x) (round y) 0 0)
