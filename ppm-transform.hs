import Data.List
import Data.Word
import qualified Data.ByteString as B

import System.Environment
import System.FilePath
        
import PPM.P6    

main :: IO ()
main = do
  files <- getArgs
  mapM_ (\ fn -> convertFile fn (dropExtension fn <.> "png")) files
  
convertFile :: FilePath -> FilePath -> IO ()  
convertFile filename outfile = do
  p' <- readP6 filename
  case p' of
    Nothing -> return ()
    Just p  -> writeP6toPNG outfile (cropWhiteSpace p)
