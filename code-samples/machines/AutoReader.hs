{-# LANGUAGE Arrows #-}

module AutoReader where

import Auto
import Auto3
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.State
import Data.Maybe
import Prelude hiding            ((.), id)

