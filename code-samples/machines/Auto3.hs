-- {-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auto3 where

import Auto hiding         (onFor)
import Auto2
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Foldable
import Data.Function       (fix)
import Data.Maybe
import Data.Traversable
import Prelude hiding      ((.), id)

newtype AutoM m a b = AConsM { runAutoM :: a -> m (b, AutoM m a b) }


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

-- instance ArrowLoop AutoM where
--     loop a = ACons $ \x ->
--         (fst *** loop) (fix (\ ~((_,d), _) -> runAutoM a (x, d)))


-- -- data StreamM m a = StreamM { runStreamM :: m (a, StreamM m a) }

-- -- ioList :: StreamM IO String
-- -- ioList = StreamM $ fmap (, ioList) getLine

-- -- instance Monad m => Functor (StreamM m) where
-- --     fmap f s = StreamM $ do
-- --         (x, s') <- runStreamM s
-- --         return (f x, fmap f s')

-- -- unrollStreamM :: Monad m => StreamM m (m a) -> m ()
-- -- unrollStreamM s = do
-- --     (x, s') <- runStreamM s
-- --     x
-- --     unrollStreamM s'


-- -- onFor :: Int -> Auto a (Maybe a)
-- -- onFor n = proc x -> do
-- --     count <- summer -< 1
-- --     id -< if count <= n
-- --             then Just x
-- --             else Nothing

-- -- onUntil :: (a -> Bool) -> Auto a (Maybe a)
-- -- onUntil p = proc x -> do
-- --     off <- autoFold (||) False -< p x
-- --     id -< if off
-- --             then Nothing
-- --             else Just x

-- -- (.?) :: Auto b (Maybe c) -> Auto a (Maybe b) -> Auto a (Maybe c)
-- -- g .? f = proc x -> do
-- --     resF <- f -< x
-- --     case resF of
-- --         Nothing -> id -< Nothing
-- --         Just x  -> g  -< x

-- -- fmapAutoMaybe :: Auto a b -> Auto (Maybe a) (Maybe b)
-- -- fmapAutoMaybe a = ACons $ \x -> let mya' = fmap (runAuto a) x
-- --                                     my   = fmap fst mya'
-- --                                     ma'  = fmap snd mya'
-- --                                 in  (my, fmapAutoMaybe (fromMaybe a ma'))

-- -- -- fmapAuto :: forall f a b. Functor f => Auto a b -> Auto (f a) (f b)
-- -- -- fmapAuto a = ACons $ \x -> let fya' :: f (b, Auto a b)
-- -- --                                fya' = fmap (runAuto a) x
-- -- --                                fy   :: f b
-- -- --                                fy   = fmap fst fya'
-- -- --                                fa'  :: f (Auto a b)
-- -- --                                fa'  = fmap snd fya'
-- -- --                            in  (fy, _a')

-- -- newtype AutoOn a b = AConsOn { runAutoOn :: a -> (Maybe b, AutoOn a b) }

-- -- instance Functor (AutoOn r) where
-- --     fmap f a = AConsOn $ \x ->
-- --                  let (y, a') = runAutoOn a x
-- --                  in  (fmap f y, fmap f a')

-- -- instance Category AutoOn where
-- --     id    = AConsOn $ \x -> (Just x, id)
-- --     g . f = AConsOn $ \x ->
-- --               let (y, f') = runAutoOn f x
-- --                   (z, g') = case y of
-- --                               Nothing -> (Nothing, g)
-- --                               Just y' -> runAutoOn g y'
-- --               in  (z, g' . f')

-- -- instance Arrow AutoOn where
-- --     arr f     = AConsOn $ \x -> (Just (f x), arr f)
-- --     first a   = AConsOn $ \(x, z) ->
-- --                   let (y, a') = runAutoOn a x
-- --                       res     = fmap (, z) y
-- --                   in  (res, first a')

-- -- instance ArrowChoice AutoOn where
-- --     left a = AConsOn $ \x ->
-- --                  case x of
-- --                    Left l  -> let (l', a') = runAutoOn a l
-- --                               in  (fmap Left l', left a')
-- --                    Right r -> (Just (Right r), left a)

-- -- joinAutoOn :: AutoOn (Maybe a) a
-- -- joinAutoOn = AConsOn $ \x -> (x, joinAutoOn)
