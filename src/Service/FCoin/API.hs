{-# LANGUAGE TemplateHaskell #-}

module Service.FCoin.API where

import Seal.Prelude
import Data.Aeson
import Text.Casing

data Item = Item {
    _price :: Double
  , _amount :: Double
} deriving (Show)

data Depth = Depth {
    _dType :: Text
  , _dBids   :: [Item]
  , _dAsks   :: [Item]
  , _dts     :: Integer
} deriving (Show)

instance Default Depth where def = Depth "" def def def

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
  _odId	:: OrderID	-- 订单 ID
, _odSymbol	:: String	-- 交易对
, _odSide	:: String	-- 交易方向（buy, sell）
, _odType	:: String	-- 订单类型（limit，market）
, _odPrice	:: String	-- 下单价格
, _odAmount	:: String	-- 下单数量
, _odState	:: String	-- 订单状态
, _odExecutedValue	:: String	-- 已成交
, _odFilledAmount	:: String	-- 成交量
, _odFillFees	:: String	-- 手续费
, _odCreatedAt	:: Integer	-- 创建时间
, _odSource	:: String	-- 来源
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

makeLenses ''Item
makeLenses ''Depth
makeLenses ''Order