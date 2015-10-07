module YampaUtils
( yampaMain ) where

import FRP.Yampa as Y
import Data.Time.Clock.POSIX
import Data.IORef

sInput :: IO (DTime, Maybe a) -> Bool -> IO (DTime, Maybe a)
sInput inp _ = inp

sOutput :: (b -> IO Bool) -> Bool -> b -> IO Bool
sOutput out _ = out

sReactimate :: IO a -> IO (DTime, Maybe a) -> (b -> IO Bool) -> SF a b -> IO ()
sReactimate init inp out sigFun = Y.reactimate init (sInput inp) (sOutput out) sigFun

yampaMain :: IO a -> IO (Maybe a) -> (b -> IO Bool) -> SF a b -> IO ()
yampaMain init input output sigFun = do
  t <- getPOSIXTime
  timeRef <- newIORef t
  let timeWrapInput ins = do
        inV <- ins
        t' <- getPOSIXTime
        t <- readIORef timeRef
        let dt = realToFrac (t' - t)
        writeIORef timeRef t'
        return (dt, inV)
  sReactimate init (timeWrapInput input) output sigFun
