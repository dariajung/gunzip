import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.BitArray
import Data.Word
import Data.Char
import Text.Printf (printf)
import System.IO 
import Data.Bits

gzfile = "keats.txt.gz"

type GZipFlags = Word8

data BitStream = BitStream {
    stream :: IO (),
    bv :: BitArray
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
    crc16 :: Word16 -- four bytes
} deriving (Show)

data BlockFormat = BlockFormat {
    last :: Bool,
    blockType :: BitArray -- length 2
} deriving (Show)

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

--getGZipHeader :: IO GZipHeader
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

-- getGZipMetadata :: IO GZipMetadata
getGZipMetadata = do
    header <- getGZipHeader
    contents <- B.readFile gzfile
    handle <- openBinaryFile gzfile ReadMode
    unnecessary <- B.hGet handle 10
    let uint8 = drop 10 $ B.unpack contents
        --_xlen = map (\x -> x :: Word8) [0, 0]
        --_extra = ""
        --_fname = ""
        --_fcomment = ""
        --crc16 = map (\x -> x :: Word8) [0, 0]
    (_xlen, _extra) <- case (hasExtra $ flags header) of
                        True -> do
                                __xlen <- B.hGet handle 2
                                let len = B.unpack __xlen
                                    numBytes = fromIntegral $ encodeWord8 len
                                __extra <- B.hGet handle numBytes
                                let unpacked = B.unpack __extra
                                    str = map chr $ map (fromIntegral) $ unpacked
                                return (encodeWord8 len, str)
                        False -> do return (0 :: Word16, "")

    print unnecessary

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

hasCont :: Integral a => a -> Bool
hasCont flag =  ((toBool $ toBinary $ fromIntegral flag) !! 6) && True

hasExtra :: Integral a => a -> Bool
hasExtra flag = ((toBool $ toBinary $ fromIntegral flag) !! 5) && True

hasName :: Integral a => a -> Bool
hasName flag =  ((toBool $ toBinary $ fromIntegral flag) !! 4) && True

hasComment :: Integral a => a -> Bool
hasComment flag = ((toBool $ toBinary $ fromIntegral flag) !! 3) && True
