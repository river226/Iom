-- qualified imports
import qualified Data.ByteString.Lazy as BS

-- normal imports
import Data.ByteString.Builder.Prim
import Data.Word
import Data.Bits

-- Copyright (c) 2016 Jedidiah River Clemons-Johnson @ www.jedidiahriver.com
-- MIT License @ opensource.org/licenses/MIT

-- Constants
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

-- AND bits to get desired values
-- desiredBits should always be one of the constants zero -> seven
getSetBits :: Word8 -> Word8 -> Word8
getSetBits inputBits desiredBits = (.&.) inputBits desiredBits

-- Start

main = do
  x <- BS.readFile "test.mp3" -- Generates bytestream
  BS.writeFile "fliptest.mp3" $ processMp3 x -- Breaks apart and writes adjusted file
  print "complete" -- Lets you know the program is complete

-- Start processing the file
processMp3 :: BS.ByteString -> BS.ByteString
processMp3 file = BS.pack $ processFrame $ BS.unpack file

-- Start processing the Frame, breaking into header/CRC/Side information, and Data
processFrame :: [Word8] -> [Word8]
processFrame [] = []
processFrame (h1:h2:h3:h4:f) = h1 : h2 : h3 : h4 : (processCRC (getSetBits h2 one) f)

--processCRC
processCRC :: Word8 -> [Word8] -> [Word8]
processCRC one (p1: p2: f) = p1 : p2 : processSideInformation f
processCRC zero f = processSideInformation f

--processSideInformation
processSideInformation file = file

--flipData
