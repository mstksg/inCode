{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
-- http://blog.jle.im/entry/id/28

module Auto3 where

import Auto
import Auto2
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Function              (fix)
import Data.Maybe
import Prelude hiding             ((.), id)
import System.IO

-- | The AutoM type: Auto with effectful stepping.
newtype AutoM m a b = AConsM { runAutoM :: a -> m (b, AutoM m a b) }

-- | Auto testers
--
testAutoM :: Monad m => AutoM m a b -> [a] -> m ([b], AutoM m a b)
testAutoM a []      = return ([], a)
testAutoM a (x:xs)  = do
    (y , a' ) <- runAutoM a x
    (ys, a'') <- testAutoM a' xs
    return (y:ys, a'')

testAutoM_ :: Monad m => AutoM m a b -> [a] -> m [b]
testAutoM_ a = liftM fst . testAutoM a


-- | Instances
instance Monad m => Category (AutoM m) where
    id    = AConsM $ \x -> return (x, id)
    g . f = AConsM $ \x -> do
              (y, f') <- runAutoM f x
              (z, g') <- runAutoM g y
              return (z, g' . f')

instance Monad m => Functor (AutoM m r) where
    fmap f a = AConsM $ \x -> do
                 (y, a') <- runAutoM a x
                 return (f y, fmap f a')

instance Monad m => Applicative (AutoM m r) where
    pure y    = AConsM $ \_ -> return (y, pure y)
    af <*> ay = AConsM $ \x -> do
                  (f, af') <- runAutoM af x
                  (y, ay') <- runAutoM ay x
                  return (f y, af' <*> ay')

instance Monad m => Arrow (AutoM m) where
    arr f     = AConsM $ \x -> return (f x, arr f)
    first a   = AConsM $ \(x, z) -> do
                  (y, a') <- runAutoM a x
                  return ((y, z), first a')
    second a  = AConsM $ \(z, x) -> do
                  (y, a') <- runAutoM a x
                  return ((z, y), second a')
    a1 *** a2 = AConsM $ \(x1, x2) -> do
                  (y1, a1') <- runAutoM a1 x1
                  (y2, a2') <- runAutoM a2 x2
                  return ((y1, y2), a1' *** a2')
    a1 &&& a2 = AConsM $ \x -> do
                  (y1, a1') <- runAutoM a1 x
                  (y2, a2') <- runAutoM a2 x
                  return ((y1, y2), a1' &&& a2')

instance Monad m => ArrowChoice (AutoM m) where
    left a = AConsM $ \x ->
                 case x of
                   Left l  -> do
                     (l', a') <-runAutoM a l
                     return (Left l', left a')
                   Right r ->
                     return (Right r, left a)

instance MonadFix m => ArrowLoop (AutoM m) where
    loop a = AConsM $ \x -> do
               rec ((y, d), a') <- runAutoM a (x, d)
               return (y, loop a')

-- | Auto converters
--
-- autoM: Turn a effectless `Auto a b` into an `AutoM m a b` that performs
--      no effects.
autoM :: Monad m => Auto a b -> AutoM m a b
autoM a = AConsM $ \x -> let (y, a') = runAuto a x
                         in  return (y, autoM a')

-- arrM: Turns an "effectful" function `a -> m b` into an `AutoM` that just
--      repeatedly sequences the function with the given input and returns
--      the output.
arrM :: Monad m => (a -> m b) -> AutoM m a b
arrM f = AConsM $ \x -> do
                    y <- f x
                    return (y, arrM f)

-- aCons: A "smart constructor" for `AutoM`: Write something as if you were
--      writing an `Auto`, but just pop `aCons` where you would put `ACons`
--      to upgrade it into an effectless `AutoM`.
aCons :: Monad m => (a -> (b, AutoM m a b)) -> AutoM m a b
aCons f = AConsM $ \x -> return (f x)

-- | IO Test Autos
--
-- replicateGets: For every step, accumulates a new input string from IO,
--     repeating it the number of times as its input for that step.
replicateGets :: AutoM IO Int String
replicateGets = proc n -> do
    ioString <- arrM (\_ -> getLine) -< ()
    let inpStr = concat (replicate n ioString)
    autoM monoidAccum -< inpStr

-- logging: Turn an `Auto` into an `AutoM` that does the same thing, but
--      logs the result to a file "log.txt" at every step.
logging :: Show b => Auto a b -> AutoM IO a b
logging a = proc x -> do
    y <- autoM a -< x
    arrM (appendFile "log.txt") -< show y ++ "\n"
    id -< y

-- | State Test Autos
--
-- limit: takes an Int, and turns an `Auto` into an `AutoM` that "takes
--      fuel".  If the current fuel (the state) is too low, `Nothing` is
--      returned. If the fuel is sufficient, the `Auto` is run and its
--      result returned in a `Just`.
limit :: Int -> Auto a b -> AutoM (State Int) a (Maybe b)
limit cost a = proc x -> do
    fuel <- arrM (\_ -> get) -< ()
    if fuel >= cost
      then do
        arrM (\_ -> modify (subtract cost)) -< ()
        y <- autoM a -< x
        id -< Just y
      else
        id -< Nothing

-- sumSqDiff: Computes a running total of the squares of inputs and
--      a running total of inputs, and then returns their difference.
--      Warning, some things require fuel.  If fuel runs out half-way, then
--      things get pretty crazy.
sumSqDiff :: AutoM (State Int) Int Int
sumSqDiff = proc x -> do
  sums   <- fromMaybe 0 <$> limit 3 summer -< x
  sumSqs <- fromMaybe 0 <$> limit 1 summer -< x^2
  id -< sumSqs - sums

-- stuff: I know, it's a pretty bad name.  It just does some fun stuff with
--      things that involve fuel, including sequencing things that
--      themselves involve fuel.
--
--      Note that due to the way `proc` notation is desugared, the
--      "effects" of each `Auto` are sequenced line-by-line, in order.
--
--      See the blog post for more information.
stuff :: AutoM (State Int) Int (Maybe Int, Maybe Int, Int)
stuff = proc x -> do
    doubled <- limit 1 id -< x * 2
    tripled <- if even x
                 then limit 2 id -< x * 3
                 else id         -< Just (x * 3)
    sumSqD  <- sumSqDiff -< x
    id -< (doubled, tripled, sumSqD)

-- | State natural transformations
--
-- sealStateAuto: Turn an `AutoM` with global state into just an `Auto`
--      without any apparent visible state.  Given an initial state, and
--      just re-runs the output state as the input for every step.
sealStateAuto :: AutoM (State s) a b -> s -> Auto a b
sealStateAuto a s0 = ACons $ \x ->
                       let ((y, a'), s1) = runState (runAutoM a x) s0
                       in  (y, sealStateAuto a' s1)

-- runStateAuto: Reifies a `State s` AutoM into a normal `Auto` that takes
--      an input state and returns an output state.
runStateAuto :: AutoM (State s) a b -> Auto (a, s) (b, s)
runStateAuto a = ACons $ \(x, s) ->
                   let ((y, a'), s') = runState (runAutoM a x) s
                   in  ((y, s'), runStateAuto a')

-- | Reader Test Autos
--
-- integral: Accumulates the time integral of the input using Euler
--      integration, assuming that the environment parameter of Reader is
--      the timestep between the last step.  Given an initial value.
integral :: Double -> AutoM (Reader Double) Double Double
integral x0 = AConsM $ \dx -> do
                dt <- ask
                let x1 = x0 + dx * dt
                return (x1, integral x1)

-- derivative: Calculates the approximate time derivative of the input,
--      assuming that the environment parameter of Reader is the timestep
--      between the last step.
--
--      Outputs `Nothing` on the first tick, because there is really
--      nothing to output.  Outputs `Just (dx/dt)` on all of the rest.
derivative :: AutoM (Reader Double) Double (Maybe Double)
derivative = AConsM $ \x -> return (Nothing, derivative' x)
  where
                 -- x0 is the "previous input"
    derivative' x0 = AConsM $ \x1 -> do
                       let dx = x1 - x0
                       dt <- ask
                       return (Just (dx/dt), derivative' x1)


-- fancyCalculus: Just performs some fancy calculus on the input stream,
--      treating it as time series data.  The Reader environment is taken
--      to be the timestep between each step.
--
--      Note that the integral of the derivative is expected to be the
--      original stream.
--
--      See blog post for more details.
fancyCalculus :: AutoM (Reader Double) Double (Double, Double)
fancyCalculus = proc x -> do
    deriv  <- fromMaybe 0 <$> derivative -< x
    deriv2 <- fromMaybe 0 <$> derivative -< deriv
    intdev <-                 integral 0 -< deriv
    id -< (deriv2, intdev)

-- | Reader natural transformations
--
-- sealReaderAuto: Takes an `Auto` that needs an environment input every
--      step, and an environment input, and turns it into an Auto that no
--      longer needs one (by just feeding the given one in every step).
sealReaderAuto :: AutoM (Reader r) a b -> r -> Auto a b
sealReaderAuto a e = ACons $ \x ->
                       let (y, a') = runReader (runAutoM a x) e
                       in  (y, sealReaderAuto a e)

-- runReaderAuto: Reifies a Reader r AutoM into a normal Auto that just
--      takes an extra parameter.
runReaderAuto :: AutoM (Reader r) a b -> Auto (a, r) b
runReaderAuto a = ACons $ \(x, e) ->
                    let (y, a') = runReader (runAutoM a x) e
                    in  (y, runReaderAuto a')

-- | Recursive Auto Test Autos
--
-- laggingSummer: Like `summer`, but returns the value of the accumulator
--      *before* adding the input.  The accumulator is still updated for
--      the next step, though.  Sort of like `x++`, instead of `++x`.
laggingSummer :: Num a => Auto a a
laggingSummer = sumFrom 0
  where
    sumFrom :: Num a => a -> Auto a a
    sumFrom x0 = ACons $ \x -> (x0, sumFrom (x0 + x))

-- piTargeter: An implementation of a PI control system using recursive
--      Autos.  See blog post for more information.
piTargeter :: Auto Double Double
piTargeter = proc control -> do
    rec let err = control - response
        errSums  <- summer         -< err

        input    <- laggingSummer  -< 0.2 * err + 0.01 * errSums
        response <- blackBoxSystem -< input

    id -< response
  where
    blackBoxSystem = id     -- to simplify things :)
