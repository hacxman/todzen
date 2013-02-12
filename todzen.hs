
import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Util.Run  -- spawnPipe and hPutStrLn
import System.IO                   -- hPutStrLn scope

import qualified Data.Map as M

import Control.Concurrent
import Numeric
import Control.Monad
import Text.Regex.TDFA

import Text.Printf
import System.Process

main = do
  dzen <- spawnPipe dzenCmd
  updateStatus dzen 0

updateStatus hnd i = do
  date <- readProcess "date" [] []
  (memT, memF) <- memUsage
  (loadavg, cpus) <- loadAvg
  let load_red = floor $ min 255 $ 155.0 + loadavg * (100.0 / fromIntegral cpus)

  hPutStrLn hnd $ Prelude.concatMap (wrap " " " ") $
    [dzenColor ("#"++showHex load_red "9999") "" $ printf "%.2f" loadavg
    ,dzenColor "#88dd22" "" $ hBar 50 10 memT (memT - memF)
    ,dzenColor "#88dd22" ""
    $ printf "%.2f" ((fromIntegral (memT - memF) / (1024*1024.0)) :: Float) ++ "GiB"
    ,dzenColor "#ffffff" "" $ dzenEscape $ trim date]

  threadDelay (seconds 1)
  putStrLn $ "### WOLOLO " ++ show i
  updateStatus hnd $ i+1

hBar w h max val =
      "^ib(1)^ro("++show w++"x10)^p(-"++show (w-2)++")^r("
      ++ show pixval ++"x6)"
      ++ "^p(+" ++ show (w-pixval) ++ ")^ib(0)"
  where
    pixval = floor (fromIntegral w * (fromIntegral val / fromIntegral max))

dzenCmd = "dzen2 -x '600' -w '800' -ta 'r'" ++ style
style   = " -h '16' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"

loadAvg :: IO (Float, Int)
loadAvg = do
  liftM2 (,) (extractor  "/proc/loadavg" extractUsage)
             (extractor "/proc/cpuinfo" extractCPUCount)
  where
    extractUsage con = read loadavg
      where [[_,loadavg]] = con =~ "^([0-9]*\\.[0-9]*).*$" :: [[String]]

    extractCPUCount con = length cpus
      where cpus = con =~ "^processor.*:\\ [0-9]*.*$" :: [[String]]

memUsage :: IO (Int, Int) -- max, current
memUsage = do
  extractor "/proc/meminfo" extractUsage
  where
    extractUsage con =
      let [[_,_,mem_total]] = con =~ "^(MemTotal):\\ *([0-9]*).*$" :: [[String]]
          [[_,_,mem_free ]] = con =~ "^(MemFree):\\ *([0-9]*).*$" :: [[String]] in
        (read mem_total, read mem_free)

extractor filename fun = do
  withFile filename ReadMode fn
  where
    fn hnd = do
      con <- hGetContents hnd
      ln <- return $ length con
      print ln
      return $ fun (con)
