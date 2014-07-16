module GunZip where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word
import Data.Char
import Text.Printf (printf)
import System.IO 
import Data.Bits
import Data.IORef


gzfile = "keats.txt.gz"

type GZipFlags = Word8

data BitStream = BitStream {
    stream :: Handle,
    bv :: IORef [Int] 
    -- not sure how bit vectors work in haskell so just using [Int] as a BV
    -- IORef allows mutability
}

-- total of 10 bytes
data GZipHeader = GZipHeader {
    magicWords :: [Word8], -- length 2
    compressionMethod :: Word8,
    flags :: GZipFlags,
    mtime :: [Word8], -- file modification time in Unix format, length 4
    extraFlags :: Word8,
    os :: Word8
} deriving (Show)

data GZipMetadata = GZipMetadata {
    header :: GZipHeader,
    xlen :: Word16, -- two bytes
    extra :: [Char],
    fname :: [Char],
    fcomment :: [Char],
    crc16 :: Word16 -- two bytes
}

data BlockFormat = BlockFormat {
    last :: Bool,
    blockType :: IORef [Int] -- length 2
}

data HuffmanHeader = HuffmanHeader {
    
}

-- translate bytes to binary
toBool (x:xs)
    | x == 1    = True : toBool xs
    | x == 0    = False : toBool xs
toBool [] = []

toBinary 0 = [0, 0, 0, 0, 0, 0, 0, 0]
toBinary n = ensureEight $ reverse (helper n)

helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

ensureEight n = (replicate (8 - length n) 0) ++ n

toHex :: B.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

-- Magic numbers are: 0x1f and 0x8b, or 31 and 139

getGZipHeader :: IO GZipHeader
getGZipHeader = do
    handle <- openBinaryFile gzfile ReadMode
    _magic <- B.hGet handle 2
    _compression <- B.hGet handle 1
    _flags <- B.hGet handle 1
    _mtime <- B.hGet handle 4
    _eFlags <- B.hGet handle 1
    _os <- B.hGet handle 1
    let header = GZipHeader {
            magicWords = B.unpack _magic,
            compressionMethod = head $ B.unpack _compression,
            flags = head $ B.unpack _flags,
            mtime = B.unpack _mtime,
            extraFlags = head $ B.unpack _eFlags,
            os = head $ B.unpack _os
        }
        magicID = magicWords header
        comp = compressionMethod header
    -- check for correct magic word and compression type    
    case magicID of
        [31, 139]   -> case comp of
                            8 -> return header
                            _ -> error $ "Invalid file compresion."
        _           -> error $ "Invalid magic words."

getGZipMetadata :: IO GZipMetadata
getGZipMetadata = do
    _header <- getGZipHeader
    contents <- B.readFile gzfile
    handle <- openBinaryFile gzfile ReadMode
    unnecessary <- B.hGet handle 10
    let uint8 = drop 10 $ B.unpack contents
    (_xlen, _extra) <- case (hasExtra $ flags _header) of
                        True -> do
                                __xlen <- B.hGet handle 2
                                let len = B.unpack __xlen
                                    numBytes = fromIntegral $ encodeWord8 len
                                __extra <- B.hGet handle numBytes
                                let unpacked = B.unpack __extra
                                    str = map chr $ map (fromIntegral) $ unpacked
                                return (encodeWord8 len, str)
                        False -> do return (0 :: Word16, "")
    _fname <- case (hasName $ flags _header) of
                True -> do
                        let _uint8 = drop (fromIntegral _xlen) uint8
                            __fname = getUntil _uint8 (0 :: Word8)
                        updateHandle <- B.hGet handle (length __fname)
                        return (C.unpack $ B.pack $ __fname)
                False -> do return ""
    _fcomment <- case (hasComment $ flags _header) of
                    True -> do
                        let _uint8 = drop (length _fname) $ drop (fromIntegral _xlen) uint8
                            __fcomment = getUntil _uint8 (0 :: Word8)
                        updateHandle <- B.hGet handle (length __fcomment + 1)
                        return (C.unpack $ B.pack $ __fcomment)
                    False -> do return ""
    _crc16 <- case (hasCrc $ flags _header) of 
                True -> do
                    crc <- B.hGet handle 2
                    return $ encodeWord8 (B.unpack crc)
                False -> do return (0 :: Word16)

    return $ GZipMetadata {
                header = _header,
                xlen = _xlen,
                extra = _extra,
                fname = _fname,
                fcomment = _fcomment,
                crc16 = _crc16
            }

getUntil :: Eq a => [a] -> a -> [a]
getUntil (x:xs) char 
    | x == char         = []
    | otherwise         = x : getUntil xs char

encodeWord8 :: [Word8] -> Word16
encodeWord8 xs = ((fromIntegral $ Prelude.last xs) * 256 + (fromIntegral $ head xs)) :: Word16

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]

-- Checks for various flags
hasAscii :: Integral a => a -> Bool
hasAscii flag = ((toBool $ toBinary $ fromIntegral flag) !! 7) && True

hasCrc :: Integral a => a -> Bool
hasCrc flag =  ((toBool $ toBinary $ fromIntegral flag) !! 6) && True

hasExtra :: Integral a => a -> Bool
hasExtra flag = ((toBool $ toBinary $ fromIntegral flag) !! 5) && True

hasName :: Integral a => a -> Bool
hasName flag =  ((toBool $ toBinary $ fromIntegral flag) !! 4) && True

hasComment :: Integral a => a -> Bool
hasComment flag = ((toBool $ toBinary $ fromIntegral flag) !! 3) && True

-- outputs in big endian
makeBitVector :: (Integral a1, Num a, Bits a) => a1 -> [a]
makeBitVector x = helper (fromIntegral x) []
    where 
        helper n l
            | length l == 8     = l
            | otherwise         =  helper (n `shiftR` 1) (l ++ [n .&. 0x1])

-- interprets binary param as in little endian format
makeInt :: [Int] -> Int
makeInt l = helper 0x00 l
    where 
        helper n arr@(x:xs) = helper ((n `shiftL` 1) + x) xs
        helper n [] = n

test = do 
    handle <- openBinaryFile gzfile ReadMode
    bvector <- newIORef []
    let a = BitStream {
                stream = handle,
                bv = bvector
            }

    ok <- readBits a 8
    print ok

-- need to make sure this works properly
readBits :: BitStream -> Int -> IO [Int]
readBits bs n = do 
    let handle = stream bs
    cached_bits <- readIORef (bv bs)
    let bytesToRead = if n > length cached_bits then n - length cached_bits else 0
    bytes <- B.hGet handle bytesToRead
    let updated_cache_bits = readBitsHelper n cached_bits (B.unpack bytes)
        streamBV = drop n updated_cache_bits
    -- remove this block later
    before <- readIORef $ bv bs
    print before
    writeIORef (bv bs) streamBV
    after <- readIORef $ bv bs
    print (after) 
    -- remove this block later
    return $ take n updated_cache_bits

readBitsHelper :: (Integral t, Num a, Bits a) => Int -> [a] -> [t] -> [a]
readBitsHelper n cachedBits (x:xs)
    | n > length cachedBits         = readBitsHelper n (cachedBits ++ newBits) xs
    | otherwise                     = cachedBits
    where
        newBits = makeBitVector x

-- read bits from stream and output decimal value
readBitsInv :: BitStream -> Int -> IO Int
readBitsInv bs n = do
    bits <- readBits bs n
    let reversed = reverse bits
    return $ makeInt reversed

