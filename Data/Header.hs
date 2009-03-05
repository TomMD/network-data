{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Header
	( L3Header (..)
	, L3Address (..)
	) where

import Data.Binary (encode, Binary)
import qualified Data.ByteString.Lazy as B
import Data.CSum as C

-- |A class of network headers that assumes a checksum is present.
class (Binary h) => L3Header h a | h -> a, a -> h where
	-- |Returns the checksum from the header
	getChecksum :: h -> CSum

	-- |Sets the checksum in the header
	setChecksum :: h -> CSum -> h

	-- |Returns a 'source' for the header.
	src :: h -> a

	-- |Returns a 'destination' for the header.
	dst :: h -> a

	-- |Computes the checksum
	-- |Returns a header with all the same fields except the checksum is zeroed
	zeroChecksum :: h -> h
	zeroChecksum h = setChecksum h 0

	computeChecksum :: h -> CSum
	computeChecksum h = C.csum16 (encode (zeroChecksum h))

	fillChecksum :: h -> h
	fillChecksum h = setChecksum h (computeChecksum h)

	-- |Returns True iff the checksum is valid
	valid :: h -> Bool
	valid h = computeChecksum h == getChecksum h

-- |A class of network addresses that assumes there is a 'broadcast' concept.
class (Binary a) => L3Address a h | a -> h, h -> a where
	localBroadcast :: a -> a
	globalBroadcast :: a
