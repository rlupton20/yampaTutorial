module SDLTools
( wipe
, pollEvents ) where

import Graphics.UI.SDL as SDL

-- wipe clears a surface (paints it black)
wipe :: Surface ->  IO Bool
wipe surface = fillRect surface Nothing (Pixel $ 0)

-- Poll Events gets all the pending events in one go for processing
pollEvents :: IO [SDL.Event]
pollEvents = do
  event <- pollEvent
  if event == SDL.NoEvent then return [] else (fmap (event:)  pollEvents)
