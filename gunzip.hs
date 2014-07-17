module GunZip where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word
import Data.Char
import Text.Printf (printf)
import System.IO 
import Data.Bits
import Data.IORef
import qualified Data.List as L


gzfile = "keats.txt.gz"

type GZipFlags = Word8

data BitStream = BitStream {
    stream :: Handle,
    bv :: IORef [Int] 
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
    blockType :: IORef [Bool] -- length 2
}

data HuffmanHeader = HuffmanHeader {
    hlit :: Word8,
    hdist :: Word8,
    hclen :: Word8
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

getGZipMetadata :: IO (GZipMetadata, Handle)
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
                        updateHandle <- B.hGet handle (length __fname + 1)
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

    return $ (GZipMetadata {
                header = _header,
                xlen = _xlen,
                extra = _extra,
                fname = _fname,
                fcomment = _fcomment,
                crc16 = _crc16
            }, handle)

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

make_BitVector x len = helper (fromIntegral x) []
    where 
        helper n l
            | length l == len       = l
            | otherwise             =  helper (n `shiftR` 1) ((n .&. 0x1) : l)

-- interprets binary param as in little endian format
makeInt :: [Int] -> Int
makeInt l = helper 0x00 l
    where 
        helper n arr@(x:xs) = helper ((n `shiftL` 1) + x) xs
        helper n [] = n

-- need to make sure this works properly
readBits :: BitStream -> Int -> IO [Int]
readBits bs n = do 
    let handle = stream bs
    cached_bits <- readIORef (bv bs)

    let updated_cache_bits = readBitsHelper n cached_bits handle   
    _updated_cache_bits <- updated_cache_bits

    let streamBV = drop n _updated_cache_bits

    writeIORef (bv bs) streamBV

    return $ take n _updated_cache_bits

readBitsHelper n cached_bits handle = 
    case greater of
        True    -> do 
                    byte <- B.hGet handle 1
                    let new_bits = makeBitVector (head $ B.unpack byte)
                        _cached_bits = cached_bits ++ new_bits
                    readBitsHelper n _cached_bits handle
        False   -> return cached_bits

    where greater = n > length cached_bits

-- read bits from stream and output decimal value
readBitsInv :: BitStream -> Int -> IO Int
readBitsInv bs n = do
    bits <- readBits bs n
    let reversed = reverse bits
    return $ makeInt reversed

getBlockFormat :: BitStream -> IO BlockFormat
getBlockFormat bs = do
    bits <- readBits bs 3
    btype <- newIORef $ map (\x -> if x == 1 then True else False) (drop 1 bits)
    return $ BlockFormat {
                GunZip.last = if head bits == 1 then True else False,
                blockType = btype
            } 


getHuffmanHeader :: BitStream -> IO HuffmanHeader
getHuffmanHeader bstream = do
    lit <- readBitsInv bstream 5
    dist <- readBitsInv bstream 5
    len <- readBitsInv bstream 4

    print lit
    print dist
    print len

    return $ HuffmanHeader {
                hlit = fromIntegral lit,
                hdist = fromIntegral dist,
                hclen = fromIntegral len
            }

-- define Huffman tree & nodes

data InternalNode a = InternalNode {
    zero :: Node a, -- left
    one :: Node a -- right
} deriving (Show)

data Node a = EmptyNode
            | LeafNode { label :: a }
    deriving (Show)

type HuffmanTree = InternalNode

createCodeTable :: (Num a, Ord t, Bits a) => [Int] -> [t] -> [(t, [a])]
createCodeTable hclens labels = 
    gen_code_table ans_sorted

    where 
        not_zero_indicies = map (==0) hclens
        _hclens = filter (not . (==0)) hclens
        (_labels, bools) = unzip $ filter (\(x,y) -> y == False) $ zip labels not_zero_indicies
        _sorted_pair = L.sort $ zip _hclens _labels
        answers = take (length _hclens) $ helper _sorted_pair 0 [0]
        ans_sorted = zip answers _sorted_pair

        helper [] _ arr = arr
        helper (x@(code_len, label):xs) prev_code_len arr
            | code_len == prev_code_len         = helper xs code_len (arr ++ [prev_code_len + 1])
            | otherwise                         = helper xs code_len (arr ++ [(prev_code_len + 1) `shiftL` (code_len - prev_code_len)])

        gen_code_table ((ans, (code_len, label)):xs) = (label, make_BitVector ans code_len) : gen_code_table xs
        gen_code_table [] = []

inflate = do
    (metadata, handle) <- getGZipMetadata
    arr <- newIORef []
    let bs = BitStream {
                stream = handle,
                bv = arr
            }

    getHuffmanHeader bs


