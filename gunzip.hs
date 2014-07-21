{-# LANGUAGE FlexibleContexts #-}

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
-- UNSAFE!!! FOR DEBUGGING PURPOSES ONLY. 
-- WILL BE REMOVED LATER.
import System.IO.Unsafe
import Control.Monad


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

createCodeTable :: (Num Int, Ord t, Bits Int) => [Int] -> [t] -> [(t, [Int])]
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

-- define Huffman tree & nodes

data InternalNode a = EmptyNode
                    | LeafNode { label :: a }
                    | InternalNode {
                            zero :: IORef (InternalNode a),  -- left
                            one :: IORef (InternalNode a)  -- right
                        }
    deriving (Show)


type HuffmanTree a = InternalNode a

initInternalNode :: IO (InternalNode a) -- InternalNode
initInternalNode = do
    _zero <- newIORef $ EmptyNode
    _one <- newIORef $ EmptyNode

    return $ InternalNode {
                zero = _zero,
                one = _one
            }   
                        -- Internal Node
setIndex :: (Eq a, Num a) => InternalNode a1 -> InternalNode a1 -> a -> IO ()
setIndex node value direction =
    case direction of
        0 -> do writeIORef (zero node) value
        1 -> do writeIORef (one node) value

                                -- Internal Node
getIndex :: (Eq a, Monad m, Num a) => InternalNode a1 -> a -> m (IORef (InternalNode a1))
getIndex node dir = 
    case dirBool of
        True    -> do 
                    return $ one node
        False   -> do
                    return $ zero node

    where dirBool = if dir == 1 then True else False

-- root should be an internal node
addItem root _label [x] = do
                val <- getIndex root (x)
                writeIORef val (LeafNode _label)

addItem root _label code@(x:xs) = do
    node_val <- getIndex root (x)
    child <- initInternalNode
    _node_val <- readIORef node_val

    case _node_val of 
        (LeafNode x)            -> addItem _node_val _label xs
        (InternalNode x y)      -> addItem _node_val _label xs
        _                       -> do 
                                    writeIORef node_val child
                                    addItem child _label xs            

createHuffmanTree :: (Eq a1, Num a1, Show a) => [(a, [a1])] -> IO (InternalNode a)
createHuffmanTree code_table = do
    root <- initInternalNode
    let addedItems = map (\(label, codes) -> addItem root label codes) code_table

    return root

-- returns IO InternalNode
read_first_tree bs hclen = do
    -- Whoa, list of labels from GZip specs. Thanks, Julia.
    let labels = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
    _hclens <- sequence $ map (\x -> readBitsInv bs 3) [1..(hclen + 4)]
    let code_table = createCodeTable _hclens labels
        first_tree = createHuffmanTree code_table
    first_tree

read_huffman_bits :: BitStream -> HuffmanTree a1 -> IO a1
read_huffman_bits bs tree =
    let n = tree
    in helper n
    where 
        helper node =
            case node of
                (LeafNode x)    -> return $ label node
                _           -> do 
                                _bit <- readBits bs 1
                                let bit = head _bit
                                _val <- getIndex node bit
                                val <- readIORef _val
                                helper val

read_second_tree :: (Eq a, Num a) => BitStream -> HuffmanHeader -> InternalNode a -> IO [a]
read_second_tree bs header tree =
    let n_to_read = fromIntegral $ hlit header + hdist header + 258
    in helper 0 [] n_to_read

    where helper count vals to_read =
            case loop of
                True -> do 
                        code_len <- read_huffman_bits bs tree
                        case code_len of
                            16      -> do 
                                        _n_repeat <- readBitsInv bs 2
                                        let n_repeat = _n_repeat + 3
                                            arr = take n_repeat $ repeat (vals !! count)
                                        helper (count + n_repeat) (vals ++ arr) to_read
                            17      -> do
                                        _n_zeros <- readBitsInv bs 3
                                        let n_zeros = _n_zeros + 3 
                                            arr = take n_zeros $ repeat 0
                                        helper (count + n_zeros) (vals ++ arr) to_read
                            18      -> do
                                        _n_zeros <- readBitsInv bs 7 
                                        let n_zeros = _n_zeros + 11
                                            arr = take n_zeros $ repeat 0
                                        helper (count + n_zeros) (vals ++ arr) to_read
                            _       -> helper (count + 1) (vals ++ [code_len]) to_read
                False -> return vals
                where 
                    loop = (count < to_read)

-- rudimentary inflate func for now
inflate = do
    (metadata, handle) <- getGZipMetadata
    arr <- newIORef []
    let bs = BitStream {
                stream = handle,
                bv = arr
            }

    root <- initInternalNode

    addItem root 6 [0,0,0,0,1,0,1,0]

    print root

-- UNSAFE!!! For debugging purposes ONLY            
instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))

