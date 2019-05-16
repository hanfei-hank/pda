
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
toSuggestion :: (Int, Int) -> Depth -> Suggestion
toSuggestion (ifrom, ito) Depth{..} = 
    Suggestion 
        (price $ dBids !! ito) (price $ dBids !! ifrom)
        (price $ dAsks !! ifrom) (price $ dAsks !! ito)


sign :: ByteString -> ByteString -> ByteString
sign secret msg = 
        let digest :: Digest SHA1 = hmacGetDigest $ hmac secret $ B64.encode msg
        in B64.encode $ ByteArray.convert digest

app :: TVar Suggestion -> WS.ClientApp ()
app tsuggestion conn = do
    putTextLn "Connected!"

    -- Read from stdin and write to WS
--     let loop = do
--             threadDelay 30000000
--              WS.sendTextData conn "xxx" >> loop
    let subCmd :: Text = "{\"cmd\":\"sub\",\"args\":[\"depth.L20.btcusdt\"]}"
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
                        let s = toSuggestion (8, 16) depth
                        putTextLn $ toText $ show s
                        atomically $ writeTVar tsuggestion s
                        go (n+1) (dts depth)
    
    go 0 0
    
  
--------------------------------------------------------------------------------
start :: IO ()
start = do
  -- runSecureClient "echo.websocket.org" 443 "/" app
  tsuggestion <- newTVarIO $ Suggestion 0 0 0 0
  runSecureClient "api.fcoin.com" 443 "/v2/ws" $ app tsuggestion
    

serverTime :: IO ()
serverTime = do
    let request = "GET https://api.fcoin.com/v2/public/server-time"
    response :: Integer <- rpData . getResponseBody <$> httpJSON request
    putTextLn $ toText $ show response
