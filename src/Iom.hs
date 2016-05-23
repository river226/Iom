import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits

-- Copyright (c) 2016 Jedidiah River Clemons-Johnson @ www.jedidiahriver.com
-- MIT License @ opensource.org/licenses/MIT

main = do
  x <- BS.readFile "test.mp3" -- Generates bytestream
  BS.writeFile "fliptest.mp3" $ processMp3 x -- Breaks apart and writes adjusted file
  print "complete" -- Lets you know the program is complete

-- Start processing the file
processMp3 :: BS.ByteString -> BS.ByteString
processMp3 file = BS.pack $ processFrame $ BS.unpack file

-- Start processing the Frame, breaking into header/CRC/Side information, and Data
processFrame :: [word8] -> [word8]
processFrame file = file
