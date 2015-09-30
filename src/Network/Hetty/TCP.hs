module Network.Hetty.TCP where

import Control.Monad
import Data.Word

import qualified System.Socket as S
import qualified System.Socket.Family.Inet as SI

import Network.Hetty.Transport

type Socket = S.Socket S.Inet S.Stream S.TCP

type Port = Word16

createTCPTransport :: Socket -> Transport
createTCPTransport sock = Transport {
    send  = \buf -> void $ S.send sock buf mempty
  , close = S.close sock
  }

localLocator :: Locator Port
localLocator = Locator {
    connect = \(Service port) -> liftM createTCPTransport $ connectLoopbackPort port
  }

connectLoopbackPort :: Port -> IO Socket
connectLoopbackPort port = do
    s <- S.socket
    S.connect s addr
    return s
  where addr = SI.SocketAddressInet SI.loopback $ SI.Port port
