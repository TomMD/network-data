{-# LANGUAGE DisambiguateRecordFields, FlexibleInstances, MultiParamTypeClasses
           , DeriveDataTypeable #-}
{- |The Data.Ethernet module exports Ethernet header structures.
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
data Ethernet = Ethernet !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize Ethernet where
  put (Ethernet o0 o1 o2 o3 o4 o5) = do putWord8 o0
                                        putWord8 o1
                                        putWord8 o2
                                        putWord8 o3
                                        putWord8 o4
                                        putWord8 o5
  get = do o0 <- getWord8
           o1 <- getWord8
           o2 <- getWord8
           o3 <- getWord8
           o4 <- getWord8
           o5 <- getWord8
           return $ Ethernet o0 o1 o2 o3 o4 o5

data EthernetHeader =
    EthernetHdr { destination  :: !Ethernet,
                  source       :: !Ethernet,
                  etherType    :: !Word16
                } deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize EthernetHeader where
  put (EthernetHdr dst src ty) = do put dst
                                    put src
                                    putWord16be ty
  get = do dst <- get
           src <- get
           ty  <- getWord16be
           return $ EthernetHdr dst src ty

-- Pretty Printing and parsing instances
instance Pretty Ethernet where
    pPrint (Ethernet o0 o1 o2 o3 o4 o5)
        = text
        . concat 
        . intersperse "::" 
        . map (flip showHex "") 
        $ [o0, o1, o2, o3, o4, o5]
