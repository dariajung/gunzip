import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.BitArray
import Data.Word
import Data.Char

data BitStream = BitStream {
    stream :: IO (),
    bv :: BitArray
}

data GZipHeader = GZipHeader {
    id :: [Word8], -- length 2
    compressionMethod :: Word8,
    flags :: GZipFlags,
    mtime :: [Word8], -- length 4
    extraFlags :: Word8,
    os :: Word8
}

data GZipMetadata = GZipMetadata {
    header :: GZipHeader,
    xlen :: Word16,
    extra :: [Char],
    fname :: [Char],
    fcomment :: [Char],
    crc16 :: Word16
}

data BlockFormat = BlockFormat {
    last :: Bool,
    blockType :: BitArray
}

type GZipFlags = Word8

main = do
    contents <- B.readFile "keats.txt.gz"
    print contents