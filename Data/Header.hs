{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Header where

import Data.Binary (encode, Binary)
import qualified Data.ByteString.Lazy as B
import Data.CSum as C

class (Binary h) => Header h a | h -> a where
	zeroChecksum :: h -> h
	zeroChecksum = id
	getChecksum :: h -> CSum
	computeChecksum :: h -> B.ByteString -> CSum
	computeChecksum h _ = C.csum16 (encode (zeroChecksum h))
	valid :: h -> Bool
	valid h = computeChecksum h B.empty == getChecksum h
	src :: h -> a
	dst :: h -> a
	
