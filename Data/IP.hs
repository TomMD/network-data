{-# LANGUAGE DisambiguateRecordFields, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{- |The Data.IP library exports IPv4 address and header structures.

   FIXME:
   There is currently no support for options fields of the IP header.
 -}
module Data.IP
        ( IPv4(..)
        , IPv4Header(..)
        , IPv4Flag(..)
        , IP
        , IPHeader
        , dummyIPv4Header
        ) where

import Control.Monad (sequence, when, liftM)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.CSum
import Data.Data
import Data.List
import Data.IPv6
import Data.Header
import Data.Bits
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.Word

type IP = Either IPv4 IPv6

type IPHeader = Either IPv4Header IPv6Header

-- |For IPv4 addresses.
data IPv4 = IPv4 Word32 deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize IPv4 where
        put (IPv4 b) = putWord32be b
        get = liftM IPv4 getWord32be

-- |Don't fragment, more fragment and reserved flags
data IPv4Flag = DF | MF | Res deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Enum [IPv4Flag] where
        fromEnum xs = foldl' (.|.) 0 $ map fromEnum1 xs
        toEnum f = map snd $ filter fst [(testBit f 0, Res), (testBit f 1, MF), (testBit f 2, DF)]

fromEnum1 MF   = 1
fromEnum1 DF   = 2
fromEnum1 Res  = 4

-- |This IPv4 header structure lacks support for options.  Ints are used
-- for most integral data types and the binary instance hands the bit packing.
--
-- No warning is provided if a value is trunkated when packed!
data IPv4Header =
        IPv4Hdr { hdrLength             :: Int
                , version               :: Int
                , tos                   :: Int
                , totalLength           :: Int
                , ipID                  :: Int
                , flags                 :: [IPv4Flag]
                , fragmentOffset        :: Int
                , ttl                   :: Int
                , protocol              :: Int
                , checksum              :: CSum
                , source                :: IPv4
                , destination           :: IPv4
        } deriving (Eq, Ord, Show, Read, Data, Typeable)

-- |A dummy header with zeroed fields except version, header length and TTL (255).
dummyIPv4Header = IPv4Hdr 5 4 0 0 0 [] 0 255 0 0 ipv4zero ipv4zero

ipv4zero = IPv4 0

instance Serialize IPv4Header where
  put (IPv4Hdr ihl ver tos len id flags off ttl prot csum src dst) = do
        pW8 $ (ihl .&. 0xF) .|. (ver `shiftL` 4 .&. 0xF0)
        pW8 tos
        pW16 len
        pW16 id
        let offFlags = (off .&. 0x1FFF) .|. fromIntegral (fromEnum flags `shiftL` 13)
        pW16 offFlags
        pW8 ttl
        pW8 prot
        put csum
        put src
        put dst

  get = do
        ihlVer <- gW8
        let ihl = (ihlVer .&. 0xF)
            ver = (ihlVer `shiftR` 4) .&. 0xF
        tos <- gW8
        len <- gW16
        id  <- gW16
        offFlags <- gW16
        let off = offFlags .&. 0x1FFF
            flags = toEnum $ offFlags `shiftR` 13
        ttl <- gW8
        prot <- gW8
        csum <- get
        src <- get
        dst <- get
        return $ IPv4Hdr ihl ver tos len id flags off ttl prot csum src dst

gW8 = getWord8 >>= return . fromIntegral
gW16 = getWord16be >>= return . fromIntegral
pW8 = putWord8 . fromIntegral
pW16 = putWord16be . fromIntegral
pW32 = putWord32be . fromIntegral

-- L3Header and L3Address instances (see Data.Header)
instance L3Header IPv4Header IPv4 CSum where
        getChecksum = checksum
        setChecksum h c = h { checksum = c }
        src = Data.IP.source
        dst = Data.IP.destination
        pseudoHeader h = runPut (do
                put (src h)
                put (dst h)
                putWord8 0
                pW8 $ fromIntegral (protocol h)
                pW16 (totalLength h))
        computeChecksum h = csum16 (encode (zeroChecksum h))

instance L3Address IPv4 IPv4Header where
        localBroadcast (IPv4 a) = IPv4 (0xFFFFFF00 .|. (0x000000FF .&. a))
        globalBroadcast = IPv4 0xFFFFFFFF

-- Pretty Printing and parsing instances
instance Pretty IPv4 where
        pPrint (IPv4 i) = text . concat . intersperse "." . map show $ unfoldr (\(i,v) -> if i /= 0 then Just (v .&. 0xFF, (i-1, v `shiftR` 8)) else Nothing) (4,i)
