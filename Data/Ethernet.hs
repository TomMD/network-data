{-# LANGUAGE DisambiguateRecordFields, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{- |The Data.IP library exports IPv4 address and header structures.

   FIXME:
   There is currently no support for options fields of the IP header.
 -}
module Data.Ethernet
        ( Ethernet(..)
        , EthernetHeader(..)
        ) where

import Control.Monad (sequence, when, liftM)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.CSum
import Data.Data
import Data.List
import Data.Bits
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.Word
import Numeric

-- | An Ethernet hardware address or MAC address.
data Ethernet = Ethernet !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize Ethernet where
  put (Ethernet o0 o1 o2 o3 o4 o5) = do pW8 o0
                                        pW8 o1
                                        pW8 o2
                                        pW8 o3
                                        pW8 o4
                                        pW8 o5
  get = do o0 <- gW8
           o1 <- gW8
           o2 <- gW8
           o3 <- gW8
           o4 <- gW8
           o5 <- gW8
           return $ Ethernet o0 o1 o2 o3 o4 o5

data EthernetHeader =
        EthernetHdr { destination  :: !Ethernet,
                      source       :: !Ethernet,
                      etherType    :: !Word16
                    } deriving (Eq, Ord, Show, Read, Data, Typeable)
                    
instance Serialize EthernetHeader where
  put (EthernetHdr dst src ty) = do put dst
                                    put src
                                    pW16 ty
  get = do dst <- get
           src <- get
           ty  <- gW16
           return $ EthernetHdr dst src ty

gW8 = getWord8 >>= return . fromIntegral
gW16 = getWord16be >>= return . fromIntegral
pW8 = putWord8 . fromIntegral
pW16 = putWord16be . fromIntegral
pW32 = putWord32be . fromIntegral

-- Pretty Printing and parsing instances
instance Pretty Ethernet where
        pPrint (Ethernet o0 o1 o2 o3 o4 o5) = text $ concat $ intersperse "::" $ map (flip showHex "") [o0, o1, o2, o3, o4, o5]
