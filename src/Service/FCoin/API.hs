
module Service.FCoin.API where

import Seal.Prelude
import Data.Aeson
import Text.Casing

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

data Suggestion = Suggestion {
    minBidPrice :: Double
  , maxBidPrice :: Double
  , minAskPrice :: Double
  , maxAskPrice :: Double
} deriving (Show)

data Order = Order {
  odId	:: OrderID	-- 订单 ID
, odSymbol	:: String	-- 交易对
, odSide	:: String	-- 交易方向（buy, sell）
, odType	:: String	-- 订单类型（limit，market）
, odPrice	:: String	-- 下单价格
, odAmount	:: String	-- 下单数量
, odState	:: String	-- 订单状态
, odExecutedValue	:: String	-- 已成交
, odFilledAmount	:: String	-- 成交量
, odFillFees	:: String	-- 手续费
, odCreatedAt	:: Integer	-- 创建时间
, odSource	:: String	-- 来源
} deriving (Generic, Show)

jsonLabel :: String -> String
jsonLabel = toQuietSnake . dropPrefix . fromHumps

customOptions = defaultOptions
                { fieldLabelModifier = jsonLabel
                }

instance FromJSON Order where
  parseJSON = genericParseJSON customOptions

-- type OrderMap = Map String Order

data Response a = Response {
    rpStatus  :: Int
  , rpData    :: a
} deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON customOptions

type OrderID = String
type Symbol = String
type Price = Double
type Amount = Double

data OrderRequest a where
  Buy :: Symbol -> Price -> Amount -> OrderRequest OrderID
  Sell :: Symbol -> Price -> Amount -> OrderRequest OrderID
  GetOrders :: Symbol -> [String] -> OrderRequest [Order]
  GetOrder :: OrderID -> OrderRequest Order
  CancelOrder :: OrderID -> OrderRequest Bool
  
data APIConfig = APIConfig {
    apiKey :: ByteString
  , apiSecret :: ByteString
} deriving (Show)