
-- fcoin client
module Service.FCoin.Impl where

import           Seal.Prelude
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
-- toSuggestion :: (Int, Int) -> Depth -> Suggestion
-- toSuggestion (ifrom, ito) Depth{..} = 
--     Suggestion 
--         (depth ^. dBids . ix ito . price) (price $ dBids !! ifrom)
--         (price $ dAsks !! ifrom) (price $ dAsks !! ito)


sign :: ByteString -> ByteString -> ByteString
sign secret msg = 
        let digest :: Digest SHA1 = hmacGetDigest $ hmac secret $ B64.encode msg
        in B64.encode $ ByteArray.convert digest

app :: TVar Depth -> WS.ClientApp ()
app tdepth conn = do
    putTextLn "Connected!"

    -- Read from stdin and write to WS
--     let loop = do
--             threadDelay 30000000
--              WS.sendTextData conn "xxx" >> loop
    let subCmd :: Text = "{\"cmd\":\"sub\",\"args\":[\"depth.L20.eosusdt\"]}"
    WS.sendTextData conn subCmd
--     loop
    -- writes WS data to stdout
    let go n ts
            | n > 100 = do
                let pingCmd :: Text = "{\"cmd\":\"ping\",\"args\":[" <> toText (show ts) <> "],\"id\":\"1\"}"
                WS.sendTextData conn pingCmd 
                go 0 ts
            | otherwise = do
                msg <- WS.receiveData conn
                case toDepth msg of
                    Nothing -> do
                        putTextLn $ "ignore : " <> msg
                        go (n+1) ts
                    Just depth  -> do
                        -- let s = toSuggestion (8, 16) depth
                        -- putTextLn $ toText $ show s
                        atomically $ writeTVar tdepth depth
                        go (n+1) (_dts depth)
    
    go 0 0
    
  
--------------------------------------------------------------------------------
start :: IO (TVar Depth)
start = do
  -- runSecureClient "echo.websocket.org" 443 "/" app
  tdepth <- newTVarIO def
  async $ runSecureClient "api.fcoin.com" 443 "/v2/ws" $ app tdepth
  return tdepth
    

serverTime :: IO Integer
serverTime = do
    let request = "GET https://api.fcoin.com/v2/public/server-time"
    response :: Integer <- rpData . getResponseBody <$> httpJSON request
    return response

orderRequest :: FromJSON a => APIConfig -> Integer -> OrderRequest a -> IO a
orderRequest APIConfig{..} ts = \case
    GetOrders sym sts -> do
        call $ get $ "https://api.fcoin.com/v2/orders?limit=20&states=" <> mconcat (intersperse "," sts) <> "&symbol=" <> sym
    GetOrder odID -> do
        call $ get $ "https://api.fcoin.com/v2/orders/" <> odID
    CancelOrder odID -> do
        call $ get $ "https://api.fcoin.com/v2/orders/" <> odID <> "/submit-cancel"

  where
    call req = rpData . getResponseBody <$> httpJSON req
    sig = sign apiSecret . fromString
    fcHeaders sig = 
                addRequestHeader "FC-ACCESS-KEY" apiKey
              . addRequestHeader "FC-ACCESS-SIGNATURE" sig
              . addRequestHeader "FC-ACCESS-TIMESTAMP" (fromString $ show ts)
    get :: String -> HTTP.Request
    get s = fcHeaders (sig $ "GET" <> s <> show ts) . fromString $ "GET " <> s