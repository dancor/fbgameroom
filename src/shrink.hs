import Control.Concurrent
import Data.List
import HSH
import System.Directory
import System.FilePath

shrink f = if ".jpg" `isSuffixOf` f
  then do
    runIO ("convert", ["-resize", "1x1", "pix" </> f,
      "pixsm" </> take (length f - 3) f ++ "png"])
    removeFile $ "pix" </> f
  else return ()

main = do
  createDirectoryIfMissing False "pixsm"
  fs <- getDirectoryContents "pix"
  mapM_ shrink fs
  threadDelay 2000000
  main
