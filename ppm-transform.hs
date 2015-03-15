-- P6
-- 1275 1650
-- 255
-- <FF><FF><FF><FF><FF><FF><FF><FF><FF><FF><FF><FF>

import System.Environment
import Data.List
-- import HSH
import Data.Word
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  let infile_pattern  = args!!0
      outfile_pattern = args!!1
      name_convert fname = if (isPrefixOf infile_pattern fname) then outfile_pattern ++ drop (length infile_pattern) fname else outfile_pattern ++ fname
  mapM_ (convert_file name_convert) (drop 2 args)
  
convert_file :: (String -> String) -> String -> IO ()  
convert_file nconv filename = do
  c <- BS.readFile filename
  BS.writeFile (nconv filename) (convert c)
  
convert :: BS.ByteString -> BS.ByteString
convert c =
  let offset = find_nth 0 3 (toEnum 10) c
  in (BS.take offset c) `BS.append` (convert3 (BS.drop offset c))
     
-- loosely find the nth occurence of some character in the ByteString

find_nth :: Int -> Int -> Word8 -> BS.ByteString -> Int
find_nth offset 0 _ _ = offset
find_nth offset n c str | BS.index str offset == c = find_nth (offset + 1) (n - 1) c str
                        | otherwise                = find_nth (offset + 1) n c str

-- we convert the image by simply inverting it:

convert3 :: BS.ByteString -> BS.ByteString
convert3 str = BS.map (\w8 -> 255 - w8) str
