{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module AutoX where


import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Fix
import Prelude hiding      ((.), id)

newtype AutoX m a b = AConsX { runAutoX :: a -> m (Maybe b, AutoX m a b) }

-- | Instances
instance Monad m => Category (AutoX m) where
    id    = AConsX $ \x -> return (Just x, id)
    g . f = AConsX $ \x -> do
              (y, f') <- runAutoX f x
              (z, g') <- case y of
                           Just _y -> runAutoX g _y
                           Nothing -> return (Nothing, g)
              return (z, g' . f')

instance Monad m => Functor (AutoX m r) where
    fmap f a = AConsX $ \x -> do
                 (y, a') <- runAutoX a x
                 return (fmap f y, fmap f a')

instance Monad m => Applicative (AutoX m r) where
    pure y    = AConsX $ \_ -> return (Just y, pure y)
    af <*> ay = AConsX $ \x -> do
                  (f, af') <- runAutoX af x
                  (y, ay') <- runAutoX ay x
                  return  (f <*> y, af' <*> ay')

instance Monad m => Arrow (AutoX m) where
    arr f     = AConsX $ \x -> return (Just (f x), arr f)
    first a   = AConsX $ \(x, z) -> do
                  (y, a') <- runAutoX a x
                  return (fmap (,z) y , first a')
    second a  = AConsX $ \(z, x) -> do
                  (y, a') <- runAutoX a x
                  return (fmap (z,) y, second a')
    a1 *** a2 = AConsX $ \(x1, x2) -> do
                  (y1, a1') <- runAutoX a1 x1
                  (y2, a2') <- runAutoX a2 x2
                  return  (liftA2 (,) y1 y2, a1' *** a2')
    a1 &&& a2 = AConsX $ \x -> do
                  (y1, a1') <- runAutoX a1 x
                  (y2, a2') <- runAutoX a2 x
                  return (liftA2 (,) y1 y2, a1' &&& a2')

instance Monad m => ArrowChoice (AutoX m) where
    left a = AConsX $ \x ->
                 case x of
                   Left l  -> do
                     (l', a') <- runAutoX a l
                     return (fmap Left l', left a')
                   Right r ->
                     return (Just (Right r), left a)


aCons :: Monad m => (a -> (b, AutoX m a b)) -> AutoX m a b
aCons a = AConsX $ \x ->
            let (y, aX) = a x
            in  return (Just y, aX)

aConsM :: Monad m => (a -> m (b, AutoX m a b)) -> AutoX m a b
aConsM a = AConsX $ \x -> do
             (y, aX) <- a x
             return (Just y, aX)

aConsOn :: Monad m => (a -> (Maybe b, AutoX m a b)) -> AutoX m a b
aConsOn a = AConsX $ \x ->
              let (y, aX) = a x
              in  return (y, aX)

summer :: (Monad m, Num a) => AutoX m a a
summer = sumFrom 0
  where
    sumFrom n = aCons $ \input ->
      let s = n + input
      in  ( s , sumFrom s )

arrM :: Monad m => (a -> m b) -> AutoX m a b
arrM f = aConsM $ \x -> do
                    y <- f x
                    return (y, arrM f)

untilA :: Monad m => (a -> Bool) -> AutoX m a a
untilA p = aConsOn $ \x ->
             if p x
               then (Just x , untilA p)
               else (Nothing, empty   )


-- instance MonadFix m => ArrowLoop (AutoX m) where
--     loop a = AConsX $ \x -> do
--               res <- do
--                        rec ()
                        
--                -- res <- do
--                --       rec let (myd, a') = runAutoX a (x, d)
--                --           (y, d) <- myd
--                --       return (y, a')
--                return $ case res of
--                      Just (_y, _a') -> (Just _y, loop _a')
--                      Nothing        -> (Nothing, loop a)

-- -- instance MonadFix m => ArrowLoop (AutoM m) where
-- --     loop a = AConsM $ \x -> do
-- --                rec ((y, d), a') <- runAutoM a (x, d)
-- --                return (y, loop a')


instance Monad m => Alternative (AutoX m a) where
    empty     = AConsX $ \_ -> return (Nothing, empty)
    a1 <|> a2 = AConsX $ \x -> do
                  (y1, a1') <- runAutoX a1 x
                  (y2, a2') <- runAutoX a2 x
                  return (y1 <|> y2, a1' <|> a2')


-- aCons :: Monad m => (a -> (b, AutoX m a b)) -> AutoX m a b
-- aCons a = AConsX $ \x -> case x of
--                            Just _x -> let (y, a') = a _x
--                                       in  return (Just y, a')
--                            Nothing -> return (Nothing, aCons a)

-- aConsM :: Monad m => (a -> m (b, AutoX m a b)) -> AutoX m a b
-- aConsM a = AConsX $ \x -> case x of
--                             Just _x -> do
--                               (y, a') <- a _x
--                               return (Just y, a')
--                             Nothing ->
--                               return (Nothing, aConsM a)

-- aConsOn :: Monad m => (a -> (Maybe b, AutoX m a b)) -> AutoX m a b
-- aConsOn a = AConsX $ \x -> case x of
--                              Just _x -> let (y, a') = a _x
--                                         in  return (y, a')
--                              Nothing -> return (Nothing, aConsOn a)

-- aConsOnM :: Monad m => (a -> m (Maybe b, AutoX m a b)) -> AutoX m a b
-- aConsOnM a = AConsX $ \x -> case x of
--                               Just _x -> do
--                                 (y, a') <- a _x
--                                 return (y, a')
--                               Nothing ->
--                                 return (Nothing, aConsOnM a)

-- autoXI :: Monad m => (Maybe a -> (Maybe b, AutoX m a b)) -> AutoX m a b
-- autoXI a = AConsX $ return . a

-- autoFold :: Monad m => (b -> a -> b) -> b -> AutoX m a b
-- autoFold op = aCons . foldFrom
--   where
--     foldFrom x y = (z, aCons (foldFrom z))
--       where
--         z = x `op` y

