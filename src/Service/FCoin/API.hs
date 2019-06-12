{-# LANGUAGE TemplateHaskell #-}

module Service.FCoin.API where

import Seal.Prelude
import Data.Aeson
import Data.List.Lens
import Text.Casing

newtype Price = Price Double
    deriving (FromJSON, Show)

newtype Amount = Amount Double
    deriving (FromJSON, Show)

data Token 
  = ADA
  | BTC
  | DASH
  | ETH
  | EOS
  | USDT
  | BSV
  | ZEC

instance Show Token where
    show ADA = "ada"
    show BTC = "btc"
    show DASH = "dash"
    show ETH = "eth"
    show EOS = "eos"
    show USDT = "usdt"
    show BSV = "bsv"
    show ZEC = "zec"

mkToken :: String -> Token
mkToken "ada" = ADA
mkToken "btc" = BTC
mkToken "dash" = DASH
mkToken "eth" = ETH
mkToken "eos" = EOS
mkToken "usdt" = USDT
mkToken "bsv" = BSV
mkToken "zec" = ZEC
mkToken s = error $ toText $ "unknown token: " <> s

data Symbol = Symbol !Token !Token

instance Show Symbol where
    show (Symbol t1 t2) = show t1 <> show t2

mkSymbol :: String -> Symbol
mkSymbol s 
    | Just a <- s ^? suffixed "btc" = Symbol (mkToken a) BTC
    | Just a <- s ^? suffixed "eth" = Symbol (mkToken a) ETH
    | Just a <- s ^? suffixed "usdt" = Symbol (mkToken a) USDT
    | otherwise = error $ toText $ "unknown symbol: " <> s

-- 价格精度
pricePrec :: Symbol -> Int
pricePrec = \case
    Symbol EOS BTC  -> 7
    Symbol EOS ETH  -> 5
    Symbol EOS USDT -> 3
    Symbol EOS BTC  -> 7
    symbol          -> error $ toText $ "unknown symbol prec:" <> show symbol

data DepthLevel = L20 | L150
  deriving (Show)

data Item = Item {
    _price :: Price
  , _amount :: Amount
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
        toItem (p:a:rest) = Item (Price p) (Amount a) : toItem rest

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

instance ToJSON Order where
  toJSON = genericToJSON customOptions

-- type OrderMap = Map String Order

data Response a = Response {
    rpStatus  :: Int
  , rpData    :: Maybe a
  , rpMsg     :: Maybe Text
} deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON customOptions

type OrderID = String
-- type Symbol = String
-- type Price = Double
-- type Amount = Double

data OrderRequest a where
  Buy :: Symbol -> Price -> Amount -> OrderRequest OrderID
  Sell :: Symbol -> Price -> Amount -> OrderRequest OrderID
  GetOrders :: Symbol -> [String] -> OrderRequest [Order]
  GetOrder :: OrderID -> OrderRequest Order
  CancelOrder :: OrderID -> OrderRequest ()
  GetBalance  :: OrderRequest Value
  
data APIConfig = APIConfig {
    apiKey :: ByteString
  , apiSecret :: ByteString
} deriving (Show)

makeLenses ''Item
makeLenses ''Depth
makeLenses ''Order
