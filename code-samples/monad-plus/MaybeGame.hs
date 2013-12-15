-- A simple game using Maybe to keep track of dying.
--
-- From the article:
--      Let's say we are making a game where you can lose health by being
--      hit or gain health by picking up powerups.  We want to calculate
--      the final health at the end of the game.  It seems a bit easy: just
--      add up all the losses and gains! Unfortunately, it's not so simple
--      --- it needs to be implemented such that if your health ever dips
--      below 0, you are dead.  Forever.  No powerups will ever help you.
--
-- To load:
-- $ ghci
-- λ: :l MaybeGame.hs
--
-- Some things to play around with:
-- λ: setHealth 2 >>= hit >>= powerup >>= hit >>= powerup >>= powerup
-- λ: setHealth 2 >>= hit >>= powerup >>= hit >>= hit >>= powerup
-- λ: setHealth 10 >>= powerup >> die >>= powerup >>= powerup 
--
-- http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus#a-practical-use

import Control.Monad (guard)

-- die or fail immediately
die :: Maybe Int
die = Nothing                       -- or die = mzero

-- if not dead, sets the health to the given level
setHealth :: Int -> Maybe Int
setHealth n = Just n                -- or setHealth n = return n

-- damage the player (from its previous health) and check for death
hit :: Int -> Maybe Int
hit currHealth = do
    let newHealth = currHealth - 1
    guard $ newHealth > 0           -- fail/die immediately unless newHealth
                                    --     is positive
    return newHealth                -- succeed with newHealth if not already
                                    --     dead

-- an alternative but identical definition of `hit`, using >>= and >>
hit' :: Int -> Maybe Int
hit' currHealth = guard (newHealth > 0) >> return newHealth
    where
        newHealth = currHealth - 1

-- increase the player's health from its previous health
powerup :: Int -> Maybe Int
powerup currHealth = Just $ currHealth + 1
