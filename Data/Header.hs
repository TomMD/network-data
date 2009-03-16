{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Header
	( L3Header (..)
	, L3Address (..)
	) where

import Data.Binary (encode, Binary)
import qualified Data.ByteString.Lazy as B
import Data.CSum as C

-- |A class of network headers that assumes a checksum is present.
class (Binary h) => L3Header h a c | h -> a, a -> h, h -> c where
	-- |Returns the checksum from the header
	getChecksum :: h -> c

	-- |Sets the checksum in the header
	setChecksum :: h -> c -> h

	-- |Returns a 'source' for the header.
	src :: h -> a

	-- |Returns a 'destination' for the header.
	dst :: h -> a

	-- |Returns a header with all the same fields except the checksum is zeroed
	zeroChecksum :: h -> h
	zeroChecksum h = setChecksum h 0

	-- |Computes the checksum
	computeChecksum :: h -> c
	computeChecksum h = C.csum16 (encode (zeroChecksum h))

	-- |Computes the checksum, returns a header with the proper checksum
	fillChecksum :: h -> h
	fillChecksum h = setChecksum h (computeChecksum h)

	-- |Used by various layer 4 protocols (UDP, TCP),
	-- a pseudo header is needed to compute the checksum
	pseudoHeader :: h -> B.ByteString

	-- |Returns True iff the checksum is valid
	valid :: h -> Bool
	valid h = zeroCSum == getChecksum h || computeChecksum h == getChecksum h

-- |A class of network addresses that assumes there is a 'broadcast' concept.
class (Binary a) => L3Address a h | a -> h, h -> a where
	localBroadcast :: a -> a
	globalBroadcast :: a

class (Binary a) => L4Header h p where
	fixChecksum :: L3Header => h -> l3 -> h
	srcPort	:: h -> p
	dstPort :: h -> p
