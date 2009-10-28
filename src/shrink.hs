import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Ord
import HSH
import System.Directory
import System.FilePath

shrink f = do
  runIO ("convert", ["-resize", "1x1", "pix" </> f,
    "pixsm" </> f ++ ".png"])
  removeFile $ "pix" </> f

fixdate d = year ++ ":" ++ monthDay ++ rest where
  (monthDay, d2) = splitAt 5 d
  (year, rest) = splitAt 4 $ tail d2

main = do
  createDirectoryIfMissing False "pixsm"
  fs <- filter ((/= '.') . head) <$> getDirectoryContents "pix"
  mapM_ shrink $ sortBy (comparing fixdate) fs
  threadDelay 2000000
  main

