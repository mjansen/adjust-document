import System.Envirnoment
import System.Directory
import Data.List
-- import HSH
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  args <- getArgs
  print args
  mapM_ (\x -> pdf_info x >>= (print . pdf_pages)) args
  mapM_ to_dark_view args
  
pdf_info :: String -> IO BS.ByteString
pdf_info filename = run ("pdfinfo", [filename]) :: IO BS.ByteString
      
pdf_pages :: BS.ByteString -> Int
pdf_pages = (read :: String -> Int) . drop (length "Pages:") . head . filter ("Pages:" `isPrefixOf`) . lines . BS.unpack
      
to_dark_view :: String -> IO ()
to_dark_view filename = do
  info <- pdf_info filename
  let npages = pdf_pages info
  let directory_name = if ".pdf" `isSuffixOf` filename then take (length filename - length ".pdf") filename else filename ++ ".dir"
  runIO ("mkdir", ["-p", directory_name])
  setCurrentDirectory directory_name
  mapM_ (convert_page filename) [1..npages]
  -- runIO $ ("pdftoppm", ["-r", "220", "../" ++ filename, "out"])
  -- mapM_ convert_to_png new_files
  print "END"

convert_to_png :: String -> IO ()
convert_to_png filename = do
  let new_name = (if ".ppm" `isSuffixOf` filename then take (length filename - length ".ppm") filename else filename) ++ ".png"
  runIO ("convert", [filename, new_name])

convert_page :: String -> Int -> IO ()
convert_page filename page =
  let page_str = show page
      page_str' = let len = length page_str
                  in take (6 - len) (repeat '0') ++ page_str
      fname = "out-" ++ page_str' ++ ".ppm"
      nname = "new-" ++ page_str' ++ ".ppm"
      pname = "file-" ++ page_str' ++ ".png"
  in  do print ("doing " ++ page_str')
         runIO ("pdftoppm", ["-f", page_str, "-l", page_str, "../" ++ filename, "out"])
         runIO ("../ppm-transform", ["out", "new", fname])
         runIO ("rm", [fname])
         runIO ("convert", [nname, pname])
         runIO ("rm", [nname])
