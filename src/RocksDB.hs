module RocksDB where

import Seal.Prelude
import qualified Database.RocksDB as Rocks


data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }


openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB fp = DB Rocks.defaultReadOptions Rocks.defaultWriteOptions options
                   <$> Rocks.open options
  where options = (Rocks.defaultOptions fp)
          { Rocks.optionsCreateIfMissing = True
          , Rocks.optionsCompression     = Rocks.NoCompression
          }

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Delete element from RocksDB for given key.
rocksDelete :: (MonadIO m) => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k
