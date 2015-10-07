import Graphics.UI.SDL as SDL
import FRP.Yampa as Y
import YampaUtils

--type Position = (Double, Double)

--type Interface = Interface { screen :: Surface }

--data Object = Object Sprite (Double, Double)
--data Sprite = Sprite String Surface

output :: Surface -> Surface -> ((Int, Int), Bool) -> IO Bool
output object surface ((x,y), exit) = do
    fillRect surface Nothing (Pixel $ 0)
    blitSurface object Nothing surface (Just $ Rect x y 0 0)
    SDL.flip surface
    return exit

sigFun :: SF Bool ((Int, Int), Bool)
sigFun = ((time >>> arr (\x -> floor $ 300 - exp (5-x))) &&& (time >>> arr (\x -> (+100).ceiling.(*20) $ sin (5*x)))) &&& identity

-- Poll Events gets all the pending events in one go for processing
pollEvents :: IO [SDL.Event]
pollEvents = do
  event <- pollEvent
  if event == SDL.NoEvent then return [] else (fmap (event:)  pollEvents)

wasQuitEvent :: IO Bool
wasQuitEvent = do
  events <- pollEvents
  let quitAsked = or $ map (==SDL.Quit) events
  return quitAsked

input :: IO (Maybe Bool)
input = fmap (Just) wasQuitEvent

main = do
    SDL.init [InitEverything]

    screen <- setVideoMode 640 480 32 [SWSurface] -- We want to thread this through the application
    hello  <- loadBMP "manSprite.bmp"
    
    yampaMain (return False) input (output hello screen) sigFun

    SDL.quit
