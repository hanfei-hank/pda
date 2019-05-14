module Service.TCPProxy where

import Seal.Prelude
import Data.Conduit
import Data.Conduit.Network

-- 将指定服务映射到本地端口，用户通过访问本地端口来访问远程服务
tcpProxy :: ByteString -> Int -> Int -> IO ()
tcpProxy host port proxyPort = void $ forkTCPServer (serverSettings proxyPort "!4") $ \clientAd -> 
  runTCPClient (clientSettings port host) $ \serverAd ->
    race_ (runConduit $ appSource serverAd .| appSink clientAd)
          (runConduit $ appSource clientAd .| appSink serverAd)

