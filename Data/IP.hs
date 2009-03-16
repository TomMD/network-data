{-# LANGUAGE DisambiguateRecordFields, FlexibleInstances, MultiParamTypeClasses #-}
{- |The Data.IP library exports IPv4 and IPv6 address and header structures.

   Patches to add more parsing and pretty printing are welcome.

   FIXME:
   There is currently no support for options fields of the IP header.
 -}
module Data.IP
	( IPv4 (..)
	, IPv4Header (..)
	, IPv4Flag (..)
	, dummyIPv4Header
	, module Data.IPv6
	, ipv4
	) where

import Control.Monad (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.CSum
import Data.List
import Data.IPv6
import Data.Header
import Data.Bits
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Prim

-- |For IPv4 addresses.  The internal representation is a bytestring so
-- use the pretty print 'ipv4' function as needed (instead of 'show').
data IPv4 = IPv4 B.ByteString deriving (Eq, Ord, Show, Read)

instance Binary IPv4 where
	put (IPv4 b) = putLazyByteString b
	get = getLazyByteString 4 >>= return . IPv4

-- |Don't fragment, more fragment and reserved flags
data IPv4Flag = DF | MF | Res deriving (Eq, Ord, Show, Read)

instance Enum [IPv4Flag] where
	fromEnum xs = foldl' (.|.) 0 $ map fromEnum1 xs
	toEnum f = map snd $ filter fst [(testBit f 0, Res), (testBit f 1, MF), (testBit f 2, DF)]

fromEnum1 DF   = 4
fromEnum1 MF   = 2
fromEnum1 Res  = 1

-- |This IPv4 header structure lacks support for options.  Ints are used
-- for most integral data types and the binary instance hands the bit packing.
--
-- No warning is provided if a field is overflowed!
data IPv4Header =
	IPv4Hdr { hdrLength		:: Int
		, version		:: Int
		, tos			:: Int
		, payloadLength		:: Int
		, ipID			:: Int
		, flags			:: [IPv4Flag]
		, fragmentOffset	:: Int
		, ttl			:: Int
		, protocol		:: Int
		, checksum		:: CSum
		, source		:: IPv4
		, destination		:: IPv4
	} deriving (Eq, Ord, Show)

-- |A dummy header with zeroed fields except version, header length and TTL (255).
dummyIPv4Header = IPv4Hdr 5 4 0 0 0 [] 0 255 0 0 ipv4zero ipv4zero

ipv4zero = IPv4 (B.pack [0,0,0,0])

instance Binary IPv4Header where
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
	src = source
	dst = destination
	pseudoHeader h = runPut (do
		put (src h)
		put (dst h)
		putWord8 0
		pW8 $ fromIntegral (protocol h)
		pW16 (payloadLength h))
	computeChecksum h = csum16 (encode (zeroChecksum h))

instance L3Address IPv4 IPv4Header where
	localBroadcast (IPv4 a) = IPv4 $ B.concat [B.pack [0xFF], B.drop 1 a]
	globalBroadcast = IPv4 $ B.replicate 4 0xFF

-- Pretty Printing and parsing instances
instance Pretty IPv4 where
	pPrint (IPv4 i) = cat . intersperse (char '.') . map (int . fromIntegral) $ (B.unpack i)

ipv4 = do
	a <- octet
	P.char '.'
	b <- octet
	P.char '.'
	c <- octet
	P.char '.'
	d <- octet
	return $ IPv4 $ B.pack [a, b, c, d]
	<?>
	"Expected IPv4 Address"

octet = do
	d <- P.many1 P.digit
	let s = toEnum $ read d
	return s
