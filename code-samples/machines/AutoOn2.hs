module AutoOn2 where

import AutoOn (AutoOn(..))

newtype AutoOn2 a b = ACons2 { runAutoOn2 :: Maybe a -> (Maybe b, AutoOn2 a b) }


onFor :: Int -> AutoOn2 a a
onFor 0 = ACons2 $ \_ -> (Nothing, onFor 0)
onFor n = ACons2 $ \x -> (x, onFor (n - 1))

autoOn :: AutoOn a b -> AutoOn2 a b
autoOn a = ACons2 $ \x ->
             case x of
               Just _x ->
                 let (y, a') = runAutoOn a _x
                 in  (y, autoOn a')
               Nothing ->
                 (Nothing, autoOn a)
