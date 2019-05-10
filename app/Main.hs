{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Seal.Prelude.App
import Types
import Repl
import RIO.Process
import System.Environment
-- import qualified Paths_pda

main :: IO ()
main = repl
