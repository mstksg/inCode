{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}

module Main (main) where

import "base" Prelude
import Web.Blog.Database

main :: IO ()
main = runDB blogMigrate
