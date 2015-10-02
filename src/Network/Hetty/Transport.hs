module Network.Hetty.Transport where

import Data.ByteString

data Transport = Transport {
    send :: ByteString -> IO ()
    -- XXX: receive
  , close :: IO ()
  }

newtype Service a = Service a

data Locator a = Locator {
    connect :: Service a -> IO Transport
  }
