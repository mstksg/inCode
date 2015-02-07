-- http://blog.jle.im/entry/id/28

module AutoOn2 where

import AutoOn (AutoOn(..))

-- | The AutoOn2 type: AutoOn but with a `Maybe` intput.
newtype AutoOn2 a b = ACons2 { runAutoOn2 :: Maybe a -> (Maybe b, AutoOn2 a b) }

-- | AutoOn2 Test Autos
--
-- onFor: Lets all input pass through for the given amount of steps, and
--      then stops.  Continues counting even when short-circuited.
onFor :: Int -> AutoOn2 a a
onFor 0 = ACons2 $ \_ -> (Nothing, onFor 0)
onFor n = ACons2 $ \x -> (x, onFor (n - 1))

-- | Auto converters
--
-- autoOn: Converts an `AutoOn` into an `AutoOn2` by defining behavior on
--    what to do when it receives a `Nothing`.
autoOn :: AutoOn a b -> AutoOn2 a b
autoOn a = ACons2 $ \x ->
             case x of
               Just _x ->
                 let (y, a') = runAutoOn a _x
                 in  (y, autoOn a')
               Nothing ->
                 (Nothing, autoOn a)

-- | Instances
--
-- hm...nothing is here :)  anyone want to submit a pull request? :D
