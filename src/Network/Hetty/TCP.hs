module TCP where

import Control.Monad

import qualified Data.ByteString as B
import qualified System.Socket as S
import qualified System.Socket.Family.Inet as SI

import Network.Hetty.Transport

createTransport :: S.Socket S.Inet S.Stream S.TCP -> Transport a
createTransport sock = {
    send t bs = void $ S.send (unTCP t) (B.concat bs) mempty
  , close = S.close . unTCP
  }

--instance Locator TCP where
foo = {
    connect (LocalService port) = do
        sock <- S.socket
        S.connect sock addr
        return $ TCP sock
      where addr = SI.SocketAddressInet SI.loopback $ SI.Port port
  }
