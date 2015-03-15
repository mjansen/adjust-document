module PPM.P6 where

-- P6
-- 1275 1650
-- 255
-- <FF><FF><FF><FF><FF><FF><FF><FF><FF><FF><FF><FF>

-- import Data.List
-- import Data.Word

-- import Data.Sequence

import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec.ByteString.Char8 as P

import System.FilePath
import System.Process.Exts

data P6 = P6
  { ppm_width  :: Int
  , ppm_height :: Int
  , ppm_depth  :: Int  -- shades of gray
  , ppm_data   :: BC.ByteString
  }

instance Show P6 where
  show (P6 w h d _) = "{PPM P6 width=" ++ show w ++ ", height=" ++ show h ++ ", depth=" ++ show d ++ "}"

parseP6 :: P.Parser P6
parseP6 = do
  P.string "P6\n"
  w <- P.decimal
  P.char ' '
  h <- P.decimal
  P.char '\n'
  d <- P.decimal
  P.char '\n'
  c <- P.take (3*w*h)
  P.endOfInput
  return $ P6 w h d c

readP6 :: FilePath -> IO (Maybe P6)
readP6 fileName = do
  content <- BC.readFile fileName
  case P.parseOnly parseP6 content of
    Left msg -> print msg >> return Nothing
    Right x  -> return . Just $ x

unparseP6 :: P6 -> BC.ByteString
unparseP6 (P6 w h d c) =
  let header = "P6\n" ++ show w ++ " " ++ show h ++ "\n" ++ show d ++ "\n"
  in BC.append (BC.pack header) c

writeP6 :: FilePath -> P6 -> IO ()
writeP6 fileName = BC.writeFile fileName . unparseP6

writeP6toPNG :: FilePath -> P6 -> IO ()
writeP6toPNG fileName x = do
  _ <- runCommandCleanly "convert" [ "ppm:-", "png:" ++ fileName ] . unparseP6 $ x
  return ()
-- convert ppm:- < out.ppm png:out2.png

toLines :: P6 -> [P6]
toLines (P6 w h d c) = map takeLine [0..h - 1]
  where
    takeLine k = P6 w 1 d (BC.take (3*w) . BC.drop (3*w*k) $ c)

fromLines :: [P6] -> P6
fromLines rs@((P6 w 1 d c):_) = P6 w (length rs) d (BC.concat . map ppm_data $ rs)

isWhite :: P6 -> Bool
isWhite (P6 w h d c) = BC.all (== toEnum 255) c

whiteOnLeft :: P6 -> Int
whiteOnLeft (P6 w h d c) =
  let tmp = BC.takeWhile (== toEnum 255) c
  in BC.length tmp `quot` 3

whiteOnRight :: P6 -> Int
whiteOnRight (P6 w h d c) =
  let tmp = BC.takeWhile (== toEnum 255) . BC.reverse $ c
  in BC.length tmp `quot` 3

cropWhiteSpace :: P6 -> P6
cropWhiteSpace x =
  let xs = dropWhile isWhite . foldr combine [] . toLines $ x
        where
          combine l [] | isWhite l = []
                       | otherwise = l:[]
          combine l sofar = l:sofar
      lm = minimum . map whiteOnLeft  $ xs
      rm = minimum . map whiteOnRight $ xs
  in fromLines . map (cropTo lm rm) $ xs

cropTo :: Int -> Int -> P6 -> P6
cropTo lm rm (P6 w 1 255 c) = P6 (w - lm - rm) 1 255 (BC.take (3*(w - lm - rm)) . BC.drop (3*lm) $ c)

