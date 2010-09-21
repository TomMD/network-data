{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveDataTypeable #-}
module Data.UDP
	( UDPPort(..)
	, UDPHeader (..)
	) where

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.CSum
import Data.Header
import Text.PrettyPrint.HughesPJClass
import Data.Data
import Data.Word

newtype UDPPort = UDPPort Word16 deriving (Eq, Ord, Show, Read, Num, Bounded, Data, Typeable)

instance Pretty UDPPort where
	pPrint (UDPPort p) = text (show p)

instance Serialize UDPPort where
	put (UDPPort p) = putWord16be p
	get = getWord16be >>= return . UDPPort

data UDPHeader =
	UDPHdr  { srcPort	:: UDPPort
		, dstPort	:: UDPPort
		, payloadLength :: Int
		, checksum	:: CSum
	} deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize UDPHeader where
	put (UDPHdr s d l c) = do
		put s
		put d
		putWord16be $ fromIntegral l
		put c
	get = do
		s <- get
		d <- get
		l <- getWord16be >>= return . fromIntegral
		c <- get
		return $ UDPHdr s d l c
