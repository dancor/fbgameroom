import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Ord
import Graphics.GD
import System.Directory
import System.FilePath

fixdate d = year ++ ":" ++ monthDay ++ rest where
  (monthDay, d2) = splitAt 5 d
  (year, rest) = splitAt 4 $ tail d2

getC f = do
  i <- loadPngFile f
  unRgba <$> getPixel (0, 0) i

numify f = do
  (r, g, b, _) <- getC $ "pixsm" </> f
  appendFile "nums" $ (fixdate . init $ takeWhile (/= 'P') f) ++ " " ++
    show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
  removeFile $ "pixsm" </> f

main = do
  fs <- getDirectoryContents "pixsm"
  mapM_ numify $ sortBy (comparing fixdate) $ filter ((/= ".") . take 1) fs
  threadDelay 2000000
  main

