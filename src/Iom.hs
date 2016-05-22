import qualified Data.ByteString.Lazy.Char8 as C

-- Copyright (c) 2016 Jedidiah River Clemons-Johnson @ www.jedidiahriver.com
-- MIT License @ opensource.org/licenses/MIT

main = do
  x <- C.readFile "test.mp3" -- Generates bytestream
  C.writeFile "fliptest.mp3" $ processMp3 x
  print "complete"

processMp3 file = file
