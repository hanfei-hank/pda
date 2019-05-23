{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Run (newRepl, runFile) where

import GHC.Conc (retry)
import Prelude (read)
import Data.Decimal
import qualified RIO.HashMap as HM
import Data.Aeson -- (Value(..))
import Data.Aeson.Lens
import Data.List.Lens
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
                    Just od -> 
                        return $ toTermLiteral $ read @Double $ od ^. odPrice
            
            isJustRef ref = do
                mv <- readIORef ref
                case mv of
                    Nothing -> return $ toTermLiteral False
                    Just v  -> return $ toTerm $ toJSON v
        -- putTextLn $ "reduce native var " <> n
        in if | n == "*price-buy-order" -> orderPrice refBuyOrder
              | n == "*price-sell-order" -> orderPrice refSellOrder
              | n == "*buy-order"    -> isJustRef refBuyOrder
              | n == "*sell-order"   -> isJustRef refSellOrder
              | Just i <- toString n ^? prefixed "*price-buy" -> buyPrice $ read i
              | Just i <- toString n ^? prefixed "*price-sell" -> sellPrice $ read i
              | otherwise -> throwString $ "unknown native var: " <> toString n

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

        getBalance :: FunApp -> [Term Name] -> Repl (Term Name)
        getBalance _ _ = do
            putTextLn "get-balance"
            ts <- serverTime
            cfg <- readIORef cfgRef
            balances <- liftIO $ FCoin.orderRequest cfg ts GetBalance
            let cs = balances ^.. values . key "currency" . _String
                mb =  Object $ HM.fromList (zip cs $ balances ^.. values)
            -- putStrLn $ show mb
            return $ toTerm mb

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

        newOrder :: _ -> Text -> Decimal -> Decimal -> Repl Text
        newOrder dir sym p a = do
            ts <- serverTime
            cfg <- readIORef cfgRef
            oid <- liftIO $ FCoin.orderRequest cfg ts $ dir (toString sym) (toDouble p) (toDouble a)
            putStrLn oid
            return $ toText oid
          where
            toDouble :: Decimal -> Double
            toDouble = fromRational . toRational

        cancelOrder :: Text -> Repl Text
        cancelOrder oid = do
            ts <- serverTime
            cfg <- readIORef cfgRef
            ret <- liftIO $ FCoin.orderRequest cfg ts $ CancelOrder $ toString oid
            putStrLn $ show ret
            return $ toText $ show ret

    loadNativeModule ("fcoin",
                       [ $(defRNativeQ "get-orders" [t| Text -> Text |] [| orders |])
                       , $(defRNativeQ "get-order" [t| Text -> Text |] [| getOrder |])
                       , $(defRNativeQ "set-api" [t| Text -> Text -> Text |] [| setApi |])
                       , $(defRNativeQ "get-market" [t| Text -> Text |] [| getMarket |])
                       , $(defRNativeQ "sell" [t| Text -> Decimal -> Decimal -> Text |] [| newOrder Sell |])
                       , $(defRNativeQ "buy" [t| Text -> Decimal -> Decimal -> Text |] [| newOrder Buy |])
                       , $(defRNativeQ "cancel-order" [t| Text -> Text |] [| cancelOrder |])
                       , defRNative "get-balance" getBalance (funType (tTyObject TyAny) []) "get balance"
                       ])


    -- debug 
    -- store <- getRefStore
    -- putStrLn $ show store

    

newRepl = new $ do
    initRepl

runFile :: FilePath -> IO ()
runFile path = do
  env <- newReplEnv

  runRIO env $ do
    initRepl
    r <- evalFile path
    putStrLn $ show r