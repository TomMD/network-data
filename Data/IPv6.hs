{-# LANGUAGE DisambiguateRecordFields #-}

module Data.IPv6
	( IPv6 (..)
	) where

import Control.Monad (sequence)
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Numeric (showHex)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Text.ParserCombinators.Parsec as P
import Data.Maybe (maybeToList)

gW8 = getWord8 >>= return . fromIntegral
gW16 = getWord16be >>= return . fromIntegral
gW32 = getWord32be >>= return . fromIntegral
pW8 = putWord8 . fromIntegral
pW16 = putWord16be . fromIntegral
pW32 = putWord32be . fromIntegral

data IPv6 = IPv6 B.ByteString deriving (Eq, Ord, Show, Read)

instance Binary IPv6 where
	put (IPv6 b) = putLazyByteString b
	get = getLazyByteString 16 >>= return . IPv6

data IPv6Header =
	IPv6Hdr { version		:: Int
		, trafficClass		:: Int
		, flowLabel		:: Int
		, payloadLength		:: Int
		, nextHeader		:: IPv6Ext
		, hopLimit		:: Int
		, source		:: IPv6
		, destination		:: IPv6
	} deriving (Eq, Ord, Show)

instance Binary IPv6Header where
  put (IPv6Hdr ver tc fl len nh hop src dst) = do
	let verTCFlow = (ver .&. 0xF `shiftL` 28) .|. (tc .&. 0xFF `shiftL` 20) .|. (fl .&. 0xFFFFF)
	pW32 verTCFlow
	pW16 len
	put nh
	pW8 hop
	put src
	put dst

  get = do
	verTCFlow <- gW32
	let ver = (verTCFlow `shiftR` 28) .&. 0xF
	    tc  = (verTCFlow `shiftR` 20) .&. 0xFF
	    fl  = verTCFlow .&. 0xFFFFF
	len <- gW16
	nh  <- get
	hop <- gW8
	src <- get
	dst <- get
	return $ IPv6Hdr ver tc fl len nh hop src dst

data IPv6Ext = E deriving (Eq, Ord, Show)

instance Binary IPv6Ext where
	get = return E
	put _ = return ()

-- TODO: Header and Address instanes

instance Pretty IPv6 where
	pPrint (IPv6 i) = cat . alternate colon . map (pHex 2) $ (B.unpack i)

-- Until 'hex' is part of the pretty printer
pHex nr n = text $ pad nr $ showHex n ""
  where
  pad n s = let l = length s in if l < n then replicate (n - l) '0' ++ s else drop (l - n) s

alternate :: a -> [a] -> [a]
alternate f xs = go xs
  where
  go (a:b:c:xs) = a : b : f : go (c:xs)
  go x = x

second :: GenParser Char st [String]
second = do
		P.char ':'
		x <- many1 hexDigit
		xs <- second
		return (x : xs)
	 <|>
		return []

first :: GenParser Char st [String]
first = do
	x <- many1 hexDigit
	(do	P.char ':'
		xs <- first
		return (x:xs)
	 <|> return [x])
	<|> return []

ipv6 :: GenParser Char st IPv6
ipv6 = do error "blah"
  where
  replace :: (Eq a) => a -> [a] -> [a] -> [a]
  replace _ _ [] = []
  replace n r (x:xs) = if n == x then r ++ xs else x : replace n r xs
