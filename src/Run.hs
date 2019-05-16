{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Run (new) where

import Seal.Prelude
import Seal.Lang.Clj.Simple 
import Text.Mustache
import Text.ProjectTemplate

import Service.TCPProxy
import qualified Service.FCoin.Impl as FCoin

echo :: Text -> Repl Text
echo s = putStrLn s >> return s

proxy :: Text -> Integer -> Integer -> Repl Text
proxy host port proxyPort = do
    liftIO $ tcpProxy (encodeUtf8 host) (fromInteger port) (fromInteger proxyPort)
    return "proxy started!"

fcoin :: Repl Text
fcoin = do
    liftIO $ FCoin.start
    return "ok"

-- render :: Text -> Text -> Repl Text
-- render templateFile targetFile = liftIO $ do
--   compiled <- localAutomaticCompile $ toString templateFile
--   case compiled of
--     Left err -> return $ toText err
--     Right template -> do
--       writeFileUtf8 (toString targetFile) $ substituteValue template value
--       return "success!"

makeRepl ['echo, 'proxy, 'fcoin]


