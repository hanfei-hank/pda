
-- fcoin client
module Service.FCoin.Impl where

import           Seal.Prelude
import           Seal.Prelude.Unsafe ((!!))
import           Data.Aeson
import qualified Network.WebSockets  as WS
import Wuss

data Item = Item {
    price :: Double
  , amount :: Double
} deriving (Show)

data Depth = Depth {
    dType :: Text
  , dBids   :: [Item]
  , dAsks   :: [Item]
  , dts     :: Integer
} deriving (Show)

instance FromJSON Depth where
    parseJSON = withObject "Depth" $ \v -> Depth
        <$> v .: "type"
        <*> fmap toItem (v .: "bids")
        <*> fmap toItem (v .: "asks")
        <*> v .: "ts"
      where
        toItem [] = []
        toItem (p:a:rest) = Item p a : toItem rest

toDepth :: Text -> Maybe Depth
toDepth = decode . encodeUtf8

--------------------------------------------------------------------------------
data Suggestion = Suggestion {
    minBidPrice :: Double
  , maxBidPrice :: Double
  , minAskPrice :: Double
  , maxAskPrice :: Double
} deriving (Show)

toSuggestion :: (Int, Int) -> Depth -> Suggestion
toSuggestion (ifrom, ito) Depth{..} = 
    Suggestion 
        (price $ dBids !! ito) (price $ dBids !! ifrom)
        (price $ dAsks !! ifrom) (price $ dAsks !! ito)

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
    



