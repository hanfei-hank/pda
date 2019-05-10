{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Run (new) where

import Seal.Prelude
import Seal.Lang.Clj.Simple 
import Text.Mustache
import Text.ProjectTemplate

echo :: Text -> Repl Text
echo s = putStrLn s >> return s

-- render :: Text -> Text -> Repl Text
-- render templateFile targetFile = liftIO $ do
--   compiled <- localAutomaticCompile $ toString templateFile
--   case compiled of
--     Left err -> return $ toText err
--     Right template -> do
--       writeFileUtf8 (toString targetFile) $ substituteValue template value
--       return "success!"

makeRepl ['echo]


