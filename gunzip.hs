import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Header = Header {
    id :: B.ByteString,
    compressionMethod :: B.ByteString
    flags :: GZipFlags,
    mtime :: B.ByteString,
    extraFlags :: B.ByteString,
    os :: B.ByteString
}

type GZipFlags = B.ByteString

main = do
    contents <- B.readFile "keats.txt.gz"
    print contents