{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
-- http://blog.jle.im/entry/effectful-recursive-real-world-autos-intro-to-machine
--
-- Auto with on/off behavior

module AutoOn where

import Auto hiding         (onFor)
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Prelude hiding      ((.), id)

-- | The AutoOn type: Auto with on/off behavior
newtype AutoOn a b = AConsOn { runAutoOn :: a -> (Maybe b, AutoOn a b) }

-- | Instances
instance Category AutoOn where
    id    = AConsOn $ \x -> (Just x, id)
    g . f = AConsOn $ \x ->
              let (y, f') = runAutoOn f x
                  (z, g') = case y of
                              Just _y -> runAutoOn g _y
                              Nothing -> (Nothing, g)
              in  (z, g' . f')

instance Functor (AutoOn r) where
    fmap f a = AConsOn $ \x ->
                 let (y, a') = runAutoOn a x
                 in  (fmap f y, fmap f a')

instance Applicative (AutoOn r) where
    pure y    = AConsOn $ \_ -> (Just y, pure y)
    af <*> ay = AConsOn $ \x ->
                  let (f, af') = runAutoOn af x
                      (y, ay') = runAutoOn ay x
                  in  (f <*> y, af' <*> ay')

instance Arrow AutoOn where
    arr f     = AConsOn $ \x -> (Just (f x), arr f)
    first a   = AConsOn $ \(x, z) ->
                  let (y, a') = runAutoOn a x
                  in  (fmap (,z) y , first a')
    second a  = AConsOn $ \(z, x) ->
                  let (y, a') = runAutoOn a x
                  in  (fmap (z,) y, second a')
    -- be careful!  a1 *** a2 =/= first a1 . second a2
    a1 *** a2 = AConsOn $ \(x1, x2) ->
                  let (y1, a1') = runAutoOn a1 x1
                      (y2, a2') = runAutoOn a2 x2
                  in  (liftA2 (,) y1 y2, a1' *** a2')
    a1 &&& a2 = AConsOn $ \x ->
                  let (y1, a1') = runAutoOn a1 x
                      (y2, a2') = runAutoOn a2 x
                  in  (liftA2 (,) y1 y2, a1' &&& a2')

instance ArrowChoice AutoOn where
    left a = AConsOn $ \x ->
                 case x of
                   Left l  ->
                     let (l', a') = runAutoOn a l
                     in  (fmap Left l', left a')
                   Right r ->
                     (Just (Right r), left a)

instance ArrowLoop AutoOn where
    loop a = AConsOn $ \x ->
               let res = do
                     rec let (myd, a') = runAutoOn a (x, d)
                         (y, d) <- myd
                     return (y, a')
               in  case res of
                     Just (_y, _a') -> (Just _y, loop _a')
                     Nothing        -> (Nothing, loop a)

instance Alternative (AutoOn a) where
    empty     = AConsOn $ \_ -> (Nothing, empty)
    -- (<|>) :: AutoOn a b -> AutoOn a b -> AutoOn a b
    a1 <|> a2 = AConsOn $ \x ->
                  let (y1, a1') = runAutoOn a1 x
                      (y2, a2') = runAutoOn a2 x
                  in  (y1 <|> y2, a1' <|> a2')

-- | Auto converters
--
-- autoOn: Converts an `Auto` into an "always on" `AutoOn`.
autoOn :: Auto a b -> AutoOn a b
autoOn a = AConsOn $ \x ->
             let (y, a') = runAuto a x
             in  (Just y, autoOn a')

-- arrOn: Turns an `a -> Maybe b` into an `AutoOn` that just runs that
--      function every step.  Like `arr`, but can chose to be on or off on
--      its own.
arrOn :: (a -> Maybe b) -> AutoOn a b
arrOn f = AConsOn $ \x -> (f x, arrOn f)

-- fromAutoOn: Turns an `AutoOn` back into the `Auto` with a `Maybe` output
--      that it really is.  Used for testing.
fromAutoOn :: AutoOn a b -> Auto a (Maybe b)
fromAutoOn a = ACons $ \x ->
                 let (y, a') = runAutoOn a x
                 in  (y, fromAutoOn a')

-- | Switching
--
-- (-->): Behave like the first `Auto` until it is off, and then behave
--      like the second `Auto`.  See blog post for more information.
--
--      Basically lets an Auto "chose" when to pass on control.
(-->) :: AutoOn a b -> AutoOn a b -> AutoOn a b
a1 --> a2 = AConsOn $ \x ->
              let (y1, a1') = runAutoOn a1 x
              in   case y1 of
                     Just _  -> (y1, a1' --> a2)
                     Nothing -> runAutoOn a2 x
infixr 1 -->


-- | AutoOn Test Autos
--
-- onFor: is "on" for the given amount of ticks, and lets input flow out
--      freely, and then is off forever.
--
--      Note that state is only "ticked" when this auto is not
--      "short-circuited" away.
onFor :: Int -> AutoOn a a
onFor n = proc x -> do
    i <- autoOn summer -< 1
    if i <= n
      then id    -< x       -- succeed
      else empty -< x       -- fail
-- alternatively, using explit recursion:
-- onFor 0 = empty
-- onFor n = AConsOn $ \x -> (Just x, onFor' (n-1))

-- filterA: Only allow inputs satisfying the predicate to pass through
filterA :: (a -> Bool) -> AutoOn a a
filterA p = arrOn (\x -> x <$ guard (p x))

-- untilA: Only allow things to pass through until the first item that
--      satisfies the predicate.
untilA :: (a -> Bool) -> AutoOn a a
untilA p = proc x -> do
    stopped <- autoOn (autoFold (||) False) -< p x
    if stopped
      then empty -< x       -- fail
      else id    -< x       -- succeed

untilA' :: (a -> Bool) -> AutoOn a a
untilA' p = AConsOn $ \x ->
              if p x
                then (Just x , untilA p)
                else (Nothing, empty   )

-- | AutoOn experiments
--
-- shortCircuit1, shortCircuit2: Tests in short circuiting behavior.
shortCircuit1 :: AutoOn Int Int
shortCircuit1 = proc x -> do
    filterA even -< x
    onFor 3      -< ()
    id           -< x * 10

shortCircuit2 :: AutoOn Int Int
shortCircuit2 = proc x -> do
    onFor 3      -< ()
    filterA even -< x
    id           -< x * 10

-- stages: demonstrates switching.  Each one "moves on" after it turns off.
--      Note the recursive demonstration --- the actual behavior loops
--      around forever.
stages :: AutoOn Int Int
stages = stage1 --> stage2 --> stage3 --> stages
  where
    stage1 = onFor 2 . arr negate
    stage2 = untilA (> 15) . autoOn summer
    stage3 = onFor 3 . (pure 100 . filterA even <|> pure 200)


