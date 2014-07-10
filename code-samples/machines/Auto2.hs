module Auto2 where

import Auto
import Control.Applicative
import Control.Arrow
import Data.Function (fix)
import Control.Category
import Prelude hiding      ((.), id)


instance Category Auto where
    id    = ACons $ \x -> (x, id)
    g . f = ACons $ \x -> let (y, f') = runAuto f x
                              (z, g') = runAuto g y
                          in  (z, g' . f')

instance Functor (Auto r) where
    fmap f a = ACons $ \x -> let (y  , a') = runAuto a x
                             in  (f y, fmap f a')

instance Applicative (Auto r) where
    pure y    = ACons $ \_ -> (y, pure y)
    af <*> ay = ACons $ \x -> let (f, af') = runAuto af x
                                  (y, ay') = runAuto ay x
                              in  (f y, af' <*> ay')

instance Arrow Auto where
    arr f     = ACons $ \x -> (f x, arr f)
    first a   = ACons $ \(x, z) ->
                  let (y, a') = runAuto a x
                  in  ((y, z), first a')
    second a  = ACons $ \(z, x) ->
                  let (y, a') = runAuto a x
                  in  ((z, y), second a')
    a1 *** a2 = ACons $ \(x1, x2) ->
                  let (y1, a1') = runAuto a1 x1
                      (y2, a2') = runAuto a2 x2
                  in  ((y1, y2), a1' *** a2')
    a1 &&& a2 = ACons $ \x ->
                  let (y1, a1') = runAuto a1 x
                      (y2, a2') = runAuto a2 x
                  in  ((y1, y2), a1' &&& a2')

instance ArrowChoice Auto where
    left a = ACons $ \x ->
                 case x of
                   Left l  -> let (l', a') = runAuto a l
                              in  (Left l', left a')
                   Right r -> (Right r, left a)

instance ArrowLoop Auto where
    loop a = ACons $ \x -> (fst *** loop) (fix (\ ~((_,d), _) -> runAuto a (x, d)))
    -- loop a = mkAutoM (loop <$> loadAuto a)
    --                  (saveAuto a)
    --                  $ \x -> liftM (onOutput fst loop)
    --                          . mfix
    --                          $ \ ~(Output (_, d) _) -> stepAuto a (x, d)
