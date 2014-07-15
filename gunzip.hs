import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.BitArray
import Data.Word
import Data.Char
import Text.Printf (printf)

type GZipFlags = Word8

data BitStream = BitStream {
    stream :: IO (),
    bv :: BitArray
}

{-      
1 byte  flags
        bit 0 set: file probably ascii text
        bit 1 set: continuation of multi-part gzip file
        bit 2 set: extra field present
        bit 3 set: original file name present
        bit 4 set: file comment present
        bit 5 set: file is encrypted
        bit 6,7:   reserved
-}

-- total of 10 bytes
data GZipHeader = GZipHeader {
    magicWords :: [Word8], -- length 2
    compressionMethod :: Word8,
    flags :: GZipFlags,
    mtime :: [Word8], -- length 4
    extraFlags :: Word8,
    os :: Word8
} deriving (Show)

data GZipMetadata = GZipMetadata {
    header :: GZipHeader,
    xlen :: Word16,
    extra :: [Char],
    fname :: [Char],
    fcomment :: [Char],
    crc16 :: Word16
} deriving (Show)

data BlockFormat = BlockFormat {
    last :: Bool,
    blockType :: BitArray -- length 2
} deriving (Show)

data HuffmanHeader = HuffmanHeader {
    
}

-- translate bytes to binary
toBinary 0 = [0, 0, 0, 0, 0, 0, 0, 0]
toBinary n = ensureEight $ reverse (helper n)

helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

ensureEight n = (replicate (8 - length n) 0) ++ n

toHex :: B.ByteString -> String
toHex bytes = C.unpack bytes >>= printf "%02x"

-- Magic numbers are: 0x1f, 0x8b, or 31 and 139

main = do
    contents <- B.readFile "keats.txt.gz"
    let uint8 = B.unpack contents
    --print $ take 2 uint8
        header = GZipHeader {
                magicWords = take 2 uint8,
                compressionMethod = uint8 !! 3,
                flags = uint8 !! 4,
                mtime = take 4 $ drop 4 uint8,
                extraFlags = uint8 !! 8,
                os = uint8 !! 9
            }
    return header
