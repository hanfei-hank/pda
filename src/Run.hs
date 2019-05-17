{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Run (new) where

import Seal.Prelude
import Seal.Lang.Clj.Simple 
import Text.Mustache
import Text.ProjectTemplate

import Service.TCPProxy
import Service.FCoin.API
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

serverTime :: Repl Integer
serverTime = do
    liftIO $ FCoin.serverTime
    -- return "ok"

apiConfig = APIConfig "64b30b6b26df481ebc0b904b6900cea3" "c3873618caca4c3a9299026ef6a627c2"
getOrder :: Text -> Repl Text
getOrder oid = do
    ts <- serverTime
    order <- liftIO $ FCoin.orderRequest apiConfig ts $ GetOrder $ toString oid
    return $ toText $ show order

orders :: Text -> Repl Text
orders sym = do
    ts <- serverTime
    orders <- liftIO $ FCoin.orderRequest apiConfig ts $ GetOrders (toString sym) ["submitted"]
    return $ toText $ show orders
-- render :: Text -> Text -> Repl Text
-- render templateFile targetFile = liftIO $ do
--   compiled <- localAutomaticCompile $ toString templateFile
--   case compiled of
--     Left err -> return $ toText err
--     Right template -> do
--       writeFileUtf8 (toString targetFile) $ substituteValue template value
--       return "success!"

makeRepl ['echo, 'proxy, 'fcoin, 'serverTime, 'getOrder, 'orders]


