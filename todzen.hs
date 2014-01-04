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


--  (bat, ac, batcap) <- batInfo
--  let ac_bat_string' = show bat ++ " " ++ show ac
--  let ac_bat_string'' | ac == Online = "AC " ++
--                        case bat of Unknown  -> "C"
--                                    Charging -> "C"
--                                    Full     -> "F"
--                                    _        -> "Err"
--                    | ac == Offline =
--                        case bat of Discharging -> "D"
--                                    Full        -> "F"
--                                    Empty       -> "E"
--                                    _        -> "Err"
--
--  let ac_bat_string | ac == Online = "AC " ++ (head $ show bat) : []
--                    | ac == Offline = (head $ show bat) : []

  vol <- volume
  print vol
  hPutStrLn hnd $ Prelude.concatMap (wrap " " " ") $
--  putStrLn $ Prelude.concatMap (wrap " " " ") $
    [dzenColor "#cccc22" "" (clicable (dzenEscape " - ") "5" "0x1008FF11")
    ++"/"++dzenColor "#cccc22" "" (clicable (dzenEscape " + ") "4" "0x1008FF13")
    ++dzenColor "#cccc22" "" (hBar 50 10 100 (floor $ 100*vol))
    ,dzenColor "#cccc22" "" $ show $ floor (vol*100)
--    ,dzenColor "#cc7722" "" $ hBar 50 10 100 (floor $ 100*batcap)
  --  ,dzenColor "#cc7722" "" $ ac_bat_string ++ " " ++ show (floor $ 100*batcap) ++ "%"
    ,dzenColor ("#"++showHex load_red "9999") "" $ printf "%.2fL" loadavg
    ,dzenColor "#77bb22" "" $ hBar 50 10 memT (memT - memF)
    ,dzenColor "#77bb22" ""
    $ printf "%.2f" ((fromIntegral (memT - memF) / (1024*1024.0)) :: Float) ++ "GiB"
    ,dzenColor "#ffffff" "" $ dzenEscape $ trim date]

  threadDelay (seconds 1)
  putStrLn $ "### WOLOLO " ++ show i
  updateStatus hnd $ i+1

clicable :: String -> String -> String -> String
clicable l b key = "^ca("++b++",xdotool key "++key++")"++l++"^ca()"

hBar w h max val =
      "^ib(1)^ro("++show w++"x10)^p(-"++show (w-2)++")^r("
      ++ show pixval ++"x6)"
      ++ "^p(+" ++ show (w-pixval-2) ++ ")^ib(0)"
  where
    pixval = floor (fromIntegral (w - 4) * (fromIntegral val / fromIntegral max))

dzenCmd = "dzen2 -x '600' -w '1250' -ta 'r'" ++ style
style   = " -h '16' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"

volume :: IO Float
volume = do
  con <- readProcess "amixer" ["sget", "Master"] []
  --print $ (con =~ "^.*Playback.*\\[([0-9]*)%\\].*$" :: [[String]])
  return $ extractVol con
  where
    extractVol con = ((/100).read) (vol++".0")
      where [[_,vol],_] = con =~ "^.*Playback.*\\[([0-9]*)%\\].*$" :: [[String]]

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

data BatState = Discharging | Charging | Full | Empty | Unknown
  deriving (Show, Read, Eq)
data ACState  = Offline | Online
  deriving (Show, Read, Enum, Eq)

batInfo :: IO (BatState, ACState, Float)
batInfo = liftM3 (,,)
  (extractor "/sys/class/power_supply/BAT0/status" (read))
  (extractor "/sys/class/power_supply/AC/online" (toEnum.read))
  (extractor "/sys/class/power_supply/BAT0/capacity" ((/100).fromIntegral.read))

extractor filename fun = do
  withFile filename ReadMode fn
  where
    fn hnd = do
      con <- hGetContents hnd
      ln <- return $ length con
      print ln
      return $ fun (con)
