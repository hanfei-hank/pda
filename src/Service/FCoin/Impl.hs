
-- fcoin client
module Service.FCoin.Impl where

import           Seal.Prelude hiding ((.=))
import           Seal.Prelude.Unsafe ((!!))
import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Network.WebSockets  as WS
import Wuss
import Network.HTTP.Simple as HTTP

import Data.ByteArray as ByteArray
import Crypto.Hash
import Crypto.MAC.HMAC
import Service.FCoin.API as API

toDepth :: Text -> Maybe Depth
toDepth = decode . encodeUtf8

--------------------------------------------------------------------------------

sign :: ByteString -> ByteString -> ByteString
sign secret msg = 
        let digest :: Digest SHA1 = hmacGetDigest $ hmac secret $ B64.encode msg
        in B64.encode $ ByteArray.convert digest

app :: TVar Depth -> TVar Integer -> Text -> WS.ClientApp ()
app tdepth tts symbol conn = do
    putTextLn "Connected!"

    -- Read from stdin and write to WS
--     let loop = do
--             threadDelay 30000000
--              WS.sendTextData conn "xxx" >> loop
    let subCmd :: Text = "{\"cmd\":\"sub\",\"args\":[\"depth.L20." <> symbol <> "\"]}"
    WS.sendTextData conn subCmd
--     loop
    -- writes WS data to stdout
    let go n ts
            | n > 30 = do
                let pingCmd :: Text = "{\"cmd\":\"ping\",\"args\":[" <> toText (show ts) <> "],\"id\":\"1\"}"
                WS.sendTextData conn pingCmd 
                go 0 ts
            | otherwise = do
                msg <- WS.receiveData conn
                case toDepth msg of
                    Nothing -> do
                        -- putTextLn $ "ignore : " <> msg
                        go (n+1) ts
                    Just depth  -> do
                        -- putTextLn $ toText $ show s
                        atomically $ do
                            writeTVar tdepth depth
                            writeTVar tts $ depth ^. dts
                        go (n+1) (_dts depth)
    
    go 0 0
    
  
--------------------------------------------------------------------------------
start :: TVar Depth -> TVar Integer -> Text -> IO ()
start tdepth tts symbol = do
  let go = do
          putTextLn "starting web socket"
          runSecureClient "api.fcoin.com" 443 "/v2/ws" (app tdepth tts symbol)
          `catchAny` \e -> do
            putTextLn $ "web socket stopped!" <> toText (show e)
            threadDelay 1000000
  async $ forever go
  return ()
    

serverTime :: IO Integer
serverTime = do
    let request = "GET https://api.fcoin.com/v2/public/server-time"
    Just (response :: Integer) <- rpData . getResponseBody <$> httpJSON request
    return response

getDepth :: DepthLevel -> Text -> IO Depth
getDepth dl symbol= do
    let request = fromString $ "GET https://api.fcoin.com/v2/market/depth/" <> show dl <> "/" <> toString symbol
    Just (response :: Depth) <- rpData . getResponseBody <$> httpJSON request
    -- putStrLn $ show response
    return response    

buy, sell, limit :: Text
buy = "buy";    sell = "sell";  limit = "limit"

orderRequest' :: (MonadIO m, FromJSON a) => APIConfig -> Integer -> OrderRequest a -> m a
orderRequest' cfg ts req = do
    eret <- orderRequest cfg ts req
    case eret of
        Left err -> throwString $ toString err
        Right ret -> return ret

orderRequest :: (MonadIO m, FromJSON a) => APIConfig -> Integer -> OrderRequest a -> m (Either Text a)
orderRequest APIConfig{..} ts req = liftIO $ case req of
    GetOrders sym sts -> do
        call $ get $ "https://api.fcoin.com/v2/orders?limit=60&states=" <> mconcat (intersperse "," sts) <> "&symbol=" <> show sym
    GetOrder odID -> do
        call $ get $ "https://api.fcoin.com/v2/orders/" <> odID
    CancelOrder odID -> do
        httpNoBody =<< post ("https://api.fcoin.com/v2/orders/" <> odID <> "/submit-cancel") "" (object [])
        return $ Right ()
    GetBalance -> do
        call $ get $ "https://api.fcoin.com/v2/accounts/balance"
    Sell s (Price p) (Amount a) -> 
        call =<< post "https://api.fcoin.com/v2/orders" 
                ("amount=" <> show a <> "&price=" <> show p <> "&side=sell&symbol=" <> show s <> "&type=limit")
                (object ["amount" .= show a, "price" .= show p, "side" .= sell, "symbol" .= show s, "type" .= limit])
    Buy s (Price p) (Amount a) -> 
        call =<< post "https://api.fcoin.com/v2/orders" 
                ("amount=" <> show a <> "&price=" <> show p <> "&side=buy&symbol=" <> show s <> "&type=limit")
                (object ["amount" .= show a, "price" .= show p, "side" .= buy, "symbol" .= show s, "type" .= limit])

  where
    call req = do
        body <- getResponseBody <$> httpJSON req
        case (rpData body, rpMsg body) of
            (Just ret, _) -> return $ Right ret
            (Nothing, Just err) -> return $ Left err
            
    sig = sign apiSecret . fromString
    fcHeaders sig = 
                addRequestHeader "FC-ACCESS-KEY" apiKey
              . addRequestHeader "FC-ACCESS-SIGNATURE" sig
              . addRequestHeader "FC-ACCESS-TIMESTAMP" (fromString $ show ts)
    get :: String -> HTTP.Request
    get s = fcHeaders (sig $ "GET" <> s <> show ts) . fromString $ "GET " <> s

    post :: String -> String -> Value -> IO HTTP.Request
    post s ps v = do
        let presig = "POST" <> s <> show ts <> ps
            postsig = sig presig
        -- putStrLn $ presig <> " -> " 
        -- putStrLn $ postsig
        return $ setRequestBodyJSON v . fcHeaders postsig . fromString $ "POST " <> s