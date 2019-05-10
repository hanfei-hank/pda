module Repl(repl) where

import Seal.Prelude
import System.Console.Repline

import Run

type Repl a = HaskelineT IO a

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

quit :: [String] -> Repl ()
quit args = do
  _ <- liftIO $ print $ "cowsay" ++ " " ++ unwords args
  abort

options :: [(String, [String] -> Repl ())]
options = [
    ("help", help)  -- :help
  , ("q", quit)    -- :quit
  ]

ini :: Repl ()
ini = liftIO $ putTextLn "Welcome!"

repl :: IO ()
repl = do
  runner <- new
  let cmd = liftIO . runner
  evalRepl (pure "pda> ") cmd options (Just ':') (Word completer) ini