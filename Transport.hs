module Transport where

import Data.ByteString
import Data.Word

class Transport a where
    send :: a -> [ByteString] -> IO ()
    -- XXX: receive
    close :: a -> IO ()

newtype Service = LocalService Word16

class Locator a where
    connect :: Transport a => Service -> IO a
