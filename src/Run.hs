{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Run (newRepl, runFile) where

-- import Data.Decimal
import Seal.Prelude
import Seal.Lang.Clj.Repl 
import Seal.Lang.Clj.TH 
import Seal.Lang.Clj.Types.Runtime
import Text.Mustache
import Text.ProjectTemplate
-- import UnliftIO.Concurrent (threadDelay)

import Service.TCPProxy
import Service.FCoin.API
import qualified Service.FCoin.Impl as FCoin


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

    
sleep :: Integer -> Repl Text
sleep ms = do
    threadDelay $ fromInteger ms * 1000
    return "ok"
-- render :: Text -> Text -> Repl Text
-- render templateFile targetFile = liftIO $ do
--   compiled <- localAutomaticCompile $ toString templateFile
--   case compiled of
--     Left err -> return $ toText err
--     Right template -> do
--       writeFileUtf8 (toString targetFile) $ substituteValue template value
--       return "success!"

makeNativeModule "user" ['proxy, 'fcoin, 'serverTime, 'sleep]

initRepl :: Repl ()
initRepl = do
    loadNativeModule userModule
    forM_ [1..20] $ \(n :: Int) -> do
        let tn = toText $ show n
        defNativeVar ("*price-buy" <> tn) tTyDecimal
        defNativeVar ("*price-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal

    depthRef <- newIORef def

    installNativeVarReducer $ \n -> 
        let 
            buyPrice i = do
                depth <- readIORef depthRef
                let Just p = depth ^? dBids . ix (i - 1) . price
                return $ toTermLiteral (p)

            sellPrice i = do
                depth <- readIORef depthRef
                let Just p = depth ^? dAsks . ix (i - 1) . price
                return $ toTermLiteral (p)
        -- putTextLn $ "reduce native var " <> n
        in case n of
            "*price-buy1" -> buyPrice 1
            "*price-buy2" -> buyPrice 2
            "*price-buy3" -> buyPrice 3
            "*price-buy4" -> buyPrice 4
            "*price-buy5" -> buyPrice 5
            "*price-buy6" -> buyPrice 6
            "*price-buy7" -> buyPrice 7
            "*price-buy8" -> buyPrice 8
            "*price-buy9" -> buyPrice 9
            "*price-buy10" -> buyPrice 10
            "*price-buy11" -> buyPrice 11
            "*price-buy12" -> buyPrice 12
            "*price-buy13" -> buyPrice 13
            "*price-buy14" -> buyPrice 14
            "*price-buy15" -> buyPrice 15

            "*price-sell1" -> sellPrice 1
            "*price-sell2" -> sellPrice 2
            "*price-sell3" -> sellPrice 3
            "*price-sell4" -> sellPrice 4
            "*price-sell5" -> sellPrice 5
            "*price-sell6" -> sellPrice 6
            "*price-sell7" -> sellPrice 7
            "*price-sell8" -> sellPrice 8
            "*price-sell9" -> sellPrice 9
            "*price-sell10" -> sellPrice 10
            "*price-sell11" -> sellPrice 11
            "*price-sell12" -> sellPrice 12
            "*price-sell13" -> sellPrice 13
            "*price-sell14" -> sellPrice 14
            "*price-sell15" -> sellPrice 15
            -- _ -> return $ toTermLiteral (10.0 :: Decimal)
            _ -> throwString $ "unknown native var: " <> toString n

    tdepth <- liftIO FCoin.start
    cfgRef <- newIORef $ APIConfig "" ""

    let
        orders :: Text -> Repl Text
        orders sym = do
            ts <- serverTime
            cfg <- readIORef cfgRef
            orders <- liftIO $ FCoin.orderRequest cfg ts $ GetOrders (toString sym) ["submitted"]
            return $ toText $ show orders

        getOrder :: Text -> Repl Text
        getOrder oid = do
            ts <- serverTime
            cfg <- readIORef cfgRef
            order <- liftIO $ FCoin.orderRequest cfg ts $ GetOrder $ toString oid
            return $ toText $ show order

        setApi :: Text -> Text -> Repl Text
        setApi key secret = do
            writeIORef cfgRef $ APIConfig (encodeUtf8 key) (encodeUtf8 secret)
            return "ok"

        getMarket :: Text -> Repl Text
        getMarket sym = do
            readTVarIO tdepth >>= writeIORef depthRef
            return "ok"

    loadNativeModule ("fcoin",
                       [ $(defRNativeQ "orders" [t| Text -> Text |] [| orders |])
                       , $(defRNativeQ "get-order" [t| Text -> Text |] [| getOrder |])
                       , $(defRNativeQ "set-api" [t| Text -> Text -> Text |] [| setApi |])
                       , $(defRNativeQ "get-market" [t| Text -> Text |] [| getMarket |])
                       ])


    -- debug 
    store <- getRefStore
    putStrLn $ show store

    

newRepl = new $ do
    initRepl

runFile :: FilePath -> IO ()
runFile path = do
  env <- newReplEnv

  runRIO env $ do
    initRepl
    r <- evalFile path
    putStrLn $ show r