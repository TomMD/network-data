{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Data.TCP
        ( TCPPort (..)
        , TCPHeader (..)
        ) where

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.CSum
import Data.Bits
import Data.List
import Data.Data
import Data.Word

newtype TCPPort = TCPPort Word16 deriving (Eq, Ord, Show, Read, Num, Bounded, Data, Typeable)

instance Serialize TCPPort where
        put (TCPPort p) = putWord16be p
        get = getWord16be >>= return . TCPPort

newtype SeqNumber = SN Word32 deriving (Eq, Ord, Show, Read, Num, Bounded, Data, Typeable)
newtype AckNumber = AN Word32 deriving (Eq, Ord, Show, Read, Num, Bounded, Data, Typeable)

instance Serialize SeqNumber where
        put (SN n) = putWord32be n
        get = getWord32be >>= return . SN

instance Serialize AckNumber where
        put (AN n) = putWord32be n
        get = getWord32be >>= return . AN

data TCPFlag = FIN | SYN | RST | PSH | ACK | URG | ECE | CWR deriving (Eq, Ord, Show, Read, Enum, Data, Typeable)

instance Enum [TCPFlag] where
        fromEnum fs = foldl' (+) 0 $ map (bit . fromEnum) fs
        toEnum i = map (toEnum . snd) . filter fst . flip zip [0..7] . map (testBit i) $ [0..7]

data TCPHeader =
        TCPHdr  { srcPort       :: TCPPort
                , dstPort       :: TCPPort
                , seqNumber     :: SeqNumber
                , ackNumber     :: AckNumber
                , dataOffset    :: Int
                , res           :: Int
                , flags         :: [TCPFlag]
                , windowSize    :: Int
                , checksum      :: CSum
                , urgentPtr     :: Int
        } deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize TCPHeader where
        put (TCPHdr s d seq ack dat res fs w c u) = do
                put s
                put d
                put seq
                put ack
                let datRes = ((dat .&. 0xF) `shiftL` 4) .|. (res .&. 0xF)
                putWord8 (fromIntegral datRes)
                putWord8 (fromIntegral $ fromEnum fs)
                putWord16be (fromIntegral w .&. 0xFFFF)
                put c
                putWord16be (fromIntegral u .&. 0xFFFF)
        get = do
                s <- get
                d <- get
                seq <- get
                ack <- get
                datRes <- fmap fromIntegral getWord8
                let dat = (datRes `shiftR` 4) .&. 0xF
                    res = datRes .&. 0xF
                fs <- fmap (toEnum . fromIntegral) getWord8
                w <- getWord16be >>= return . fromIntegral
                c <- get
                u <- getWord16be >>= return . fromIntegral
                return $ TCPHdr s d seq ack dat res fs w c u

-- FIXME need parser for TCPPort, L4Header instance
