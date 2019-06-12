{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Run (newRepl, runFiles) where

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

import Service.FCoin.API
import qualified Service.FCoin.Impl as FCoin


serverTime :: Repl Integer
serverTime = do
    liftIO $ FCoin.serverTime
    -- return "ok"

sleep :: Integer -> Repl Text
sleep ms = do
    threadDelay $ fromInteger ms * 1000
    return "ok"

makeNativeModule "user" ['serverTime, 'sleep]

initRepl :: Repl ()
initRepl = do
    loadNativeModule userModule
    defNativeVar "*buy-orders" tTyBool
    defNativeVar "*price-buy-order" tTyDecimal
    defNativeVar "*sell-orders" tTyBool
    defNativeVar "*price-sell-order" tTyDecimal

    forM_ [1..20] $ \(n :: Int) -> do
        let tn = toText $ show n
        defNativeVar ("*price-buy" <> tn) tTyDecimal
        defNativeVar ("*price-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal
        defNativeVar ("*amount-sell" <> tn) tTyDecimal

    depthRef <- newIORef def

    refBuyOrder :: IORef [Order] <- newIORef []
    refSellOrder :: IORef [Order] <- newIORef []

    installNativeVarReducer $ \n -> 
        let 
            depthPrice da i = do
                depth <- readIORef depthRef
                let Just (Price p) = depth ^? da . ix (i - 1) . price
                return $ toTermLiteral p

            buyPrice = depthPrice dBids
            sellPrice = depthPrice dAsks

            orderPrice ref = do
                mod <- readIORef ref
                case mod of
                    [] -> return $ toTermLiteral (0.0 :: Double)
                    od:_ -> 
                        return $ toTermLiteral $ read @Decimal $ od ^. odPrice

            isJustRef ref = do
                mv <- readIORef ref
                case mv of
                    [] -> return $ toTermLiteral False
                    v -> return $ toTerm $ toJSON v
        -- putTextLn $ "reduce native var " <> n
        in if | n == "*price-buy-order" -> orderPrice refBuyOrder
              | n == "*price-sell-order" -> orderPrice refSellOrder
              | n == "*buy-orders"    -> isJustRef refBuyOrder
              | n == "*sell-orders"   -> isJustRef refSellOrder
              | Just i <- toString n ^? prefixed "*price-buy" -> buyPrice $ read i
              | Just i <- toString n ^? prefixed "*price-sell" -> sellPrice $ read i
              | otherwise -> throwString $ "unknown native var: " <> toString n

    tdepth <- newTVarIO def
    tts <- newTVarIO def
    cfgRef <- newIORef $ APIConfig "" ""

    let
        setApi :: Text -> Text -> Repl Text
        setApi key secret = do
            writeIORef cfgRef $ APIConfig (encodeUtf8 key) (encodeUtf8 secret)
            -- liftIO $ FCoin.start tdepth symbol
            return "ok"
        
        subTopic :: Text -> Repl Text
        subTopic symbol = do
            liftIO $ FCoin.start tdepth symbol
            return "ok"


        lastServerTime :: Repl Integer
        lastServerTime = readTVarIO tts

        orders :: Text -> Repl Text
        orders sym = do
            ts <- serverTime
            atomically $ writeTVar tts ts
            cfg <- readIORef cfgRef
            orders <- FCoin.orderRequest' cfg ts $ GetOrders (mkSymbol $ toString sym) ["submitted", "partial_filled"]
            let sellorder = [order | order <- orders, _odSide order == "sell", _odSource order /= "web"]
                buyorder  = [order | order <- orders, _odSide order == "buy", _odSource order /= "web"] 
            writeIORef refSellOrder sellorder
            writeIORef refBuyOrder buyorder
            -- putStrLn $ "sell order: " <> show sellorder
            -- putStrLn $ "buy order: " <> show buyorder

            return $ toText $ show orders

        getOrder :: Text -> Repl Text
        getOrder oid = do
            ts <- lastServerTime
            cfg <- readIORef cfgRef
            order <- FCoin.orderRequest' cfg ts $ GetOrder $ toString oid
            return $ toText $ show order

        getBalance :: FunApp -> [Term Name] -> Repl (Term Name)
        getBalance _ _ = do
            -- putTextLn "get-balance"
            ts <- lastServerTime
            cfg <- readIORef cfgRef
            balances <- FCoin.orderRequest' cfg ts GetBalance
            let cs = balances ^.. values . key "currency" . _String
                mb =  Object $ HM.fromList (zip cs $ balances ^.. values)
            -- putStrLn $ show mb
            return $ toTerm mb

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

        getMarket150 :: Text -> Repl Text
        getMarket150 sym = do
            liftIO (FCoin.getDepth sym) >>= writeIORef depthRef
            return "ok"

        newOrder :: _ -> Text -> Decimal -> Decimal -> Repl Text
        newOrder dir sym p a = do
            ts <- lastServerTime
            cfg <- readIORef cfgRef
            oid <- FCoin.orderRequest cfg ts $ dir (mkSymbol $ toString sym) (Price $ toDouble p) (Amount $ toDouble a)
            putStrLn $ show oid
            return $ "ok"
          where
            toDouble :: Decimal -> Double
            toDouble = fromRational . toRational

        cancelOrder :: Text -> Repl Text
        cancelOrder oid = do
            ts <- lastServerTime
            cfg <- readIORef cfgRef
            ret <- FCoin.orderRequest' cfg ts $ CancelOrder $ toString oid
            putStrLn $ show ret
            return $ toText $ show ret
        
        getDepthAmount :: _ -> Integer -> Repl Double
        getDepthAmount da i = do
            depth <- readIORef depthRef
            let Just (Amount p) = depth ^? da . ix (fromInteger i - 1) . amount
            return p

        getDepthPrice :: _ -> Integer -> Repl Double
        getDepthPrice da i = do
            -- putStrLn $ "get price " <> show i
            depth <- readIORef depthRef
            let Just (Price p) = depth ^? da . ix (fromInteger i - 1) . price
            return p

        buyDepthLength :: Repl Integer
        buyDepthLength = do
            depth <- readIORef depthRef
            return $ toInteger @Int $ length $ depth ^. dBids 

        sellDepthLength :: Repl Integer
        sellDepthLength = do
            depth <- readIORef depthRef
            return $ toInteger @Int $ length $ depth ^. dAsks 

    loadNativeModule ("fcoin",
                       [ $(defRNativeQ "get-orders" [t| Text -> Text |] [| orders |])
                       , $(defRNativeQ "get-order" [t| Text -> Text |] [| getOrder |])
                       , $(defRNativeQ "set-api" [t| Text -> Text -> Text |] [| setApi |])
                       , $(defRNativeQ "sub-topic" [t| Text -> Text |] [| subTopic |])
                       , $(defRNativeQ "get-market" [t| Text -> Text |] [| getMarket |])
                       , $(defRNativeQ "get-market-150" [t| Text -> Text |] [| getMarket150 |])
                       , $(defRNativeQ "sell" [t| Text -> Decimal -> Decimal -> Text |] [| newOrder Sell |])
                       , $(defRNativeQ "buy" [t| Text -> Decimal -> Decimal -> Text |] [| newOrder Buy |])
                       , $(defRNativeQ "cancel-order" [t| Text -> Text |] [| cancelOrder |])
                       , defRNative "get-balance" getBalance (funType (tTyObject TyAny) []) "get balance"
                       , $(defRNativeQ "sell-amount" [t| Integer -> Decimal |] [| getDepthAmount dAsks |])
                       , $(defRNativeQ "buy-amount" [t| Integer -> Decimal |] [| getDepthAmount dBids |])
                       , $(defRNativeQ "sell-price" [t| Integer -> Decimal |] [| getDepthPrice dAsks |])
                       , $(defRNativeQ "buy-price" [t| Integer -> Decimal |] [| getDepthPrice dBids |])
                       , $(defRNativeQ "buy-depth-len" [t| Integer |] [| buyDepthLength |])
                       , $(defRNativeQ "sell-depth-len" [t| Integer |] [| sellDepthLength |])
                       ])


    -- debug 
    -- store <- getRefStore
    -- putStrLn $ show store

    

newRepl = new $ do
    initRepl

runFiles :: [FilePath] -> String -> IO ()
runFiles paths cmd = do
  env <- newReplEnv

  runRIO env $ do
    initRepl
    mapM_ evalFile paths
    r <- evalString cmd
    putStrLn $ show r