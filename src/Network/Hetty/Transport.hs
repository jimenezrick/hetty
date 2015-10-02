module Network.Hetty.Transport where

import Data.ByteString.Lazy

data Transport = Transport {
    send :: ByteString -> IO ()
    -- XXX: receive
  , close :: IO ()
  }

newtype Service a = Service a

data Locator a = Locator {
    connect :: Service a -> IO Transport
  }
