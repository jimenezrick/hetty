module Network.Hetty.Transport where

data Transport a = Transport {
    send :: [a] -> IO ()
    -- XXX: receive
  , close :: IO ()
  }

newtype Service a = Service a

data Locator a b = Locator {
    connect :: Service a -> IO (Transport b)
  }
