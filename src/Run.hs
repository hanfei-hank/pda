{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Run (newRepl, runFile) where

import GHC.Conc (retry)
import Prelude (read)
import Seal.Prelude
import Seal.Lang.Clj.Repl 
import Seal.Lang.Clj.TH 
import Seal.Lang.Clj.Types.Runtime
import Text.Mustache
import Text.ProjectTemplate

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
    defNativeVar "*buy-order" tTyBool
    defNativeVar "*price-buy-order" tTyDecimal
    defNativeVar "*sell-order" tTyBool
    defNativeVar "*price-sell-order" tTyDecimal

    forM_ [1..20] $ \(n :: Int) -> do
        let tn = toText $ show n
        defNativeVar ("*price-buy" <> tn) tTyDecimal
        defNativeVar ("*price-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal

    depthRef <- newIORef def

    refBuyOrder :: IORef (Maybe Order) <- newIORef Nothing
    refSellOrder :: IORef (Maybe Order) <- newIORef Nothing

    installNativeVarReducer $ \n -> 
        let 
            depthPrice da i = do
                depth <- readIORef depthRef
                let Just p = depth ^? da . ix (i - 1) . price
                return $ toTermLiteral p

            buyPrice = depthPrice dBids
            sellPrice = depthPrice dAsks

            orderPrice ref = do
                mod <- readIORef ref
                case mod of
                    Nothing -> return $ toTermLiteral (0.0 :: Double)
                    Just od -> return $ toTermLiteral $ read @Double $ od ^. odPrice
            
            isJustRef ref = do
                mv <- readIORef ref
                case mv of
                    Nothing -> return $ toTermLiteral False
                    Just _  -> return $ toTermLiteral True
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

            "*buy-order"    -> isJustRef refBuyOrder
            "*price-buy-order" -> orderPrice refBuyOrder
            "*sell-order"   -> isJustRef refSellOrder
            "*price-sell-order" -> orderPrice refSellOrder

            -- _ -> return $ toTermLiteral (10.0 :: Decimal)
            _ -> throwString $ "unknown native var: " <> toString n

    tdepth <- liftIO FCoin.start
    cfgRef <- newIORef $ APIConfig "" ""

    let
        orders :: Text -> Repl Text
        orders sym = do
            ts <- serverTime
            cfg <- readIORef cfgRef
            orders <- liftIO $ FCoin.orderRequest cfg ts $ GetOrders (toString sym) ["submitted", "partial_filled"]
            let sellorder = listToMaybe [order | order <- orders, _odSide order == "sell"]
                buyorder  = listToMaybe [order | order <- orders, _odSide order == "buy"] 
            writeIORef refSellOrder sellorder
            writeIORef refBuyOrder buyorder
            putStrLn $ "sell order: " <> show sellorder
            putStrLn $ "buy order: " <> show buyorder

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
            lastTs <- view dts <$> readIORef depthRef
            d <- atomically $ do
                d <- readTVar tdepth
                if d ^. dts <= lastTs
                    then retry
                    else return d
            writeIORef depthRef d
            return "ok"

    loadNativeModule ("fcoin",
                       [ $(defRNativeQ "get-orders" [t| Text -> Text |] [| orders |])
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