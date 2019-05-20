{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Seal.Prelude.App
import Types
import Repl
import Run (runFile)
import RIO.Process
import System.Environment
-- import qualified Paths_pda

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path]  -> runFile path
    []      -> repl
    _       -> putStrLn $ "unknown argumens: " <> show args
