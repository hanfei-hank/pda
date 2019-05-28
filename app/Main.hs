{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Seal.Prelude.App
import Types
import Repl
import Run (runFiles)
import RIO.Process
import System.Environment
-- import qualified Paths_pda

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, cmd]  -> runFiles [path] cmd
    [path1, path2, cmd]  -> runFiles [path1, path2] cmd
    []      -> repl
    _       -> putStrLn $ "unknown argumens: " <> show args
