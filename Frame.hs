{-# LANGUAGE GADTs #-}

module Frame where

import Transport

import Data.Binary
import Data.ByteString





data Frame a where
    Frame :: Binary a => a -> Frame a






-- TODO: Use builder directly, no a list
buildFrame :: [ByteString]
