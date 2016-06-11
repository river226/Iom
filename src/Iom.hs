-- qualified imports
import qualified Data.ByteString.Lazy as BS

-- normal imports
import Data.ByteString.Builder.Prim
import Data.Word
import Data.Bits

-- Copyright (c) 2016 Jedidiah River Clemons-Johnson @ www.jedidiahriver.com
-- MIT License @ opensource.org/licenses/MIT

one :: Word8
one = fromInteger 1

zero :: Word8
zero = fromInteger 0

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
processFrame (h1:h2:h3:h4:f) = h1 : h2 : h3 : h4 : (processCRC (getPrivateBit h2) f)

getPrivateBit :: Word8 -> Word8
getPrivateBit pb = (.&.) pb one

--processCRC
processCRC :: Word8 -> [Word8] -> [Word8]
processCRC one (p1: p2: f) = p1 : p2 : processSideInformation f
processCRC zero f = processSideInformation f

--processSideInformation
processSideInformation file = file

--flipData
