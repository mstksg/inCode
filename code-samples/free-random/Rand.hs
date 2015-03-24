{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Free
import Data.Digest.Pure.MD5
import Data.List
import Data.Maybe
import Data.Tuple
import System.Random
import qualified Data.Binary          as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS

data RandF a where
    FromRandom   :: Random r =>           ( r  -> a) -> RandF a
    FromRandomR  :: Random r => r -> r -> ( r  -> a) -> RandF a
    FromRandoms  :: Random r =>           ([r] -> a) -> RandF a
    FromRandomRs :: Random r => r -> r -> ([r] -> a) -> RandF a

instance Functor RandF where
    fmap h rnd = case rnd of
        FromRandom         f -> FromRandom         (h . f)
        FromRandomR r0 r1  f -> FromRandomR r0 r1  (h . f)
        FromRandoms        f -> FromRandoms        (h . f)
        FromRandomRs r0 r1 f -> FromRandomRs r0 r1 (h . f)

runRandomF :: RandomGen g => RandF a -> g -> a
runRandomF (FromRandom f)         = f . fst . random
runRandomF (FromRandomR r0 r1 f)  = f . fst . randomR (r0, r1)
runRandomF (FromRandoms f)        = f . randoms
runRandomF (FromRandomRs r0 r1 f) = f . randomRs (r0, r1)

type Rand = Free RandF

runRandom :: RandomGen g => Rand a -> g -> (a, g)
runRandom (Pure x  ) g0 = (x, g0)
runRandom (Free rnd) g0 = case rnd of
    FromRandom f         -> let (x, g1) = random g0
                            in  runRandom (f x) g1
    FromRandomR r0 r1 f  -> let (x, g1) = randomR (r0, r1) g0
                            in  runRandom (f x) g1
    FromRandoms f        -> let (g1, g2) = split g0
                                xs       = randoms g1
                            in  runRandom (f xs) g2
    FromRandomRs r0 r1 f -> let (g1, g2) = split g0
                                xs = randomRs (r0, r1) g1
                            in runRandom (f xs) g2

runRandomIO :: Rand a -> IO a
runRandomIO (Pure x) = return x
runRandomIO (Free rnd) = case rnd of
    FromRandom f -> do
        r <- randomIO
        runRandomIO (f r)
    FromRandomR r0 r1 f -> do
        r <- randomRIO (r0, r1)
        runRandomIO (f r)
    FromRandoms f -> do
        rs <- randoms <$> newStdGen
        runRandomIO (f rs)
    FromRandomRs r0 r1 f -> do
        rs <- randomRs (r0, r1) <$> newStdGen
        runRandomIO (f rs)

runRandomStream :: RandomGen g => [g] -> Rand a -> (Maybe a, [g])
runRandomStream gs (Pure x) = (Just x, gs)
runRandomStream [] _        = (Nothing, [])
runRandomStream (g:gs) (Free rnd) = case rnd of
    FromRandom f         -> runRandomStream gs . f . fst $ random g
    FromRandomR r0 r1 f  -> runRandomStream gs . f . fst $ randomR (r0, r1) g
    FromRandoms f        -> runRandomStream gs . f $ randoms g
    FromRandomRs r0 r1 f -> runRandomStream gs . f $ randomRs (r0, r1) g

runRandomInteractive :: Rand a -> IO a
runRandomInteractive (Pure x) = return x
runRandomInteractive (Free rnd) = case rnd of
    FromRandom f -> do
        r <- fst . random <$> getFromStdin
        runRandomInteractive (f r)
    FromRandomR r0 r1 f -> do
        r <- fst . randomR (r0, r1) <$> getFromStdin
        runRandomInteractive (f r)
    FromRandoms f -> do
        rs <- randoms <$> getFromStdin
        runRandomInteractive (f rs)
    FromRandomRs r0 r1 f -> do
        rs <- randomRs (r0, r1) <$> getFromStdin
        runRandomInteractive (f rs)
  where
    getFromStdin :: IO StdGen
    getFromStdin = mkStdGen . B.decode . B.encode
                 . md5 . BS.fromStrict
               <$> BS.getLine

getRandom :: Random a => Rand a
getRandom = liftF $ FromRandom id

getRandomR :: Random a => a -> a -> Rand a
getRandomR r0 r1 = liftF $ FromRandomR r0 r1 id

getRandoms :: Random a => Rand [a]
getRandoms = liftF $ FromRandoms id

getRandomRs :: Random a => a -> a -> Rand [a]
getRandomRs r0 r1 = liftF $ FromRandomRs r0 r1 id

foo :: Rand Double
foo = do
    xs <- take 10 <$> getRandoms
    ys <- take 10 <$> getRandomRs 0 1
    let zs = zipWith (*) xs ys
    x <- getRandom
    y <- getRandomR 10 20
    return $ (sum zs + x) * y

gens :: IO [StdGen]
gens = unfoldr (Just . swap . next . mkStdGen) <$> randomIO

main :: IO ()
main = do
    print . fst . runRandom foo =<< newStdGen
    print =<< runRandomIO foo
    print . fromJust . fst . flip runRandomStream foo =<< gens
    print =<< runRandomInteractive foo
