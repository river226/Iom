-- qualified imports
import qualified Data.ByteString.Lazy as BS

-- normal imports
import Data.ByteString.Builder.Prim
import Data.Word
import Data.Bits
import Data.Array

-- Copyright (c) 2016-2020 Jedidiah River Clemons-Johnson @ jriver.xyz
-- MIT License @ opensource.org/licenses/MIT

-- Constants
hexF :: Word8
hexF = fromInteger 15

hexE :: Word8
hexE = fromInteger 14

hexD :: Word8
hexD = fromInteger 13

hexC :: Word8
hexC = fromInteger 12

hexB :: Word8
hexB = fromInteger 11

hexA :: Word8
hexA = fromInteger 10

nine :: Word8
nine = fromInteger 9

eight :: Word8
eight = fromInteger 8

seven :: Word8
seven = fromInteger 7

six :: Word8
six = fromInteger 6

five :: Word8
five = fromInteger 5

four :: Word8
four = fromInteger 4

three :: Word8
three = fromInteger 3

two :: Word8
two = fromInteger 2

one :: Word8
one = fromInteger 1

zero :: Word8
zero = fromInteger 0

-- constants for calculating bitrate
-- kbits/s bitrates.
--correspond to bitrate value -1 array spot
-- MPEG 1/2 layer 1
mpeg1_2_L1 :: [Int]
mpeg1_2_L1 = [32,64,96,128,160,192,224,256,288,320,352,384,416,448]

-- MPEG 1/2 layer 2
mpeg1_2_L2 :: [Int]
mpeg1_2_L2 = [32,48,56,64,80,96,112,128,160,192,224,256,320,384]

-- MPEG 1 layer 3
mpeg1_L3 :: [Int]
mpeg1_L3 = [32,40,48,56,64,80,96,112,128,160,192,224,256,320]

-- MPEG 2 layer 3
mpeg2_L3 :: [Int]
mpeg2_L3 = [8,16,24,32,64,80,56,64,128,160,112,128,256,320]

-- Frequency of the file, 1-1 correspondence.
mpeg1_freq :: [Int]
mpeg1_freq = [44100,48000,32000]

mpeg2_freq :: [Int]
mpeg2_freq = [22050,24000,16000]

-- AND bits to get desired values
-- desiredBits should always be one of the constants zero -> seven
getSetBits :: Word8 -> Word8 -> Word8
getSetBits inputBits desiredBits = (.&.) inputBits desiredBits

goThroughList :: [Int] -> Word8 -> Int
goThroughList [] _ = 0
goThroughList (x:xs) y
  | y == zero = x
  | otherwise = goThroughList xs (y-1)

getFrameSize :: Word8 -> Word8 -> Int
getFrameSize layer freq = ((144 * (bitRate layer freq) `div` (sampleRate layer freq)) + (padding freq)) - 4

bitRate :: Word8 -> Word8 -> Int
bitRate layer freq = getBitRate (shiftR freq 4) (getSetBits layer seven) (getSetBits layer six)

sampleRate :: Word8 -> Word8 -> Int
sampleRate layer freq = getSampleRate (shiftR (getSetBits freq hexB) 3) (getSetBits layer seven)

padding :: Word8 -> Int
padding freq = getPadding (getSetBits freq two)


getBitRate :: Word8 -> Word8 -> Word8 -> Int
getBitRate bitr y x
  | y == seven && x == two = goThroughList mpeg1_L3 (bitr - 1)
  | x == six = goThroughList mpeg1_2_L1 (bitr - 1)
  | x == four = goThroughList mpeg1_2_L2 (bitr - 1)
  | x == two = goThroughList mpeg2_L3 (bitr - 1)

getSampleRate :: Word8 -> Word8 -> Int
getSampleRate freq x
  | x == seven = goThroughList mpeg1_freq freq -- get freq location in mpeg1_freq Array
  | otherwise = goThroughList mpeg2_freq freq -- get freq location in mpeg2_freq Array

getPadding :: Word8 -> Int
getPadding x
  | x == two = 1
  | otherwise = 0

-- Reference = http://id3.org/mp3Frame

-- Start

main = do
  x <- BS.readFile "test.mp3" -- Generates bytestream
  BS.writeFile "fliptest.mp3" $ processMp3 x -- Breaks apart and writes adjusted file
  print "complete" -- Lets you know the program is complete

-- Start processing the file
-- Break the file apart
-- process the file
-- reassemble the file
processMp3 :: BS.ByteString -> BS.ByteString
processMp3 file = BS.pack $ processFrame $ BS.unpack file

-- Start processing the Frame, breaking into header/CRC/Side information, and Data
processFrame :: [Word8] -> [Word8]
processFrame [] = []
processFrame (h1:h2:h3:h4:f) = h1 : h2 : h3 : h4 : (processSideInformation (getSetBits h2 one) (shiftR h4 6) ((getFrameSize h2 h3) - 4) f)

processSideInformation :: Word8 -> Word8 -> Int -> [Word8] -> [Word8]
processSideInformation x y frms f
  | x == one && y == three = takeOutSI (16+17) frms f
  | x == one = takeOutSI (16+32) frms f
  | y == three = takeOutSI 17 frms f
  | otherwise = takeOutSI 32 frms f

takeOutSI :: Int -> Int -> [Word8] -> [Word8]
takeOutSI 0 frms f = processMain frms f
takeOutSI bytes frms (f:fs) = f : takeOutSI (bytes - 1) (frms - 1) fs

-- processMain :: Int -> [Word8] -> [Word8]
-- processMain _ [] = []
-- processMain frms (f:fs) = [complement f] ++ fs

processMain :: Int -> [Word8] -> [Word8]
processMain _ [] = []
processMain 0 f = processFrame f
processMain frms (f:fs) = [complement f] ++ processMain (frms - 1) fs
