{-# LANGUAGE TupleSections #-}
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
import System.Directory
import System.FilePath
import Data.Text (split, pack, unpack)
import Data.Char

import Data.Time.Clock.POSIX
import Control.Monad.State

import Data.List
import System.Time

import Control.Exception
import Data.Maybe
import Control.DeepSeq


-- default (Integer, Double)

data NetspeedState = NetspeedState { rx_timestamp :: POSIXTime
                                   , rx_bytes :: Integer
                                   , tx_timestamp ::POSIXTime
                                   , tx_bytes :: Integer } deriving (Show)

emptyNetspeedState = NetspeedState { rx_timestamp = 0, rx_bytes = 0, tx_timestamp = 0, tx_bytes = 0 }

main = do
  dzen <- spawnPipe dzenCmd
  updateStatus dzen (0 :: Integer)

getTemperature :: String -> FilePath -> FilePath -> IO (Float, Float)
getTemperature nametoextract hwmonDir namefile = do
  hwmons <- getDirectoryContents hwmonDir
  f <- filterM (\d -> getDirectoryContents (hwmonDir </> d)
                  >>= (return. (namefile `elem`))) hwmons
  fs <- filterM (\d -> fromMaybe' False $ extractor (hwmonDir </> d </> namefile) (==nametoextract) ) f
  liftM2 (,) (fromMaybe' 0 $ extractor (hwmonDir </> head fs </> "temp1_input") ((/1000).fromIntegral.read))
             (fromMaybe' 0 $ extractor (hwmonDir </> head fs </> "temp1_max") ((/1000).fromIntegral.read))

showHex2 :: Int -> String -> String
showHex2 a b = printf "%02x" a ++ b

drawBarGraph namevalues = "^ib(1)^fg(#dd3300)^ro(" ++ show barSize ++ "x" ++ (show $ 2+16) ++ ")^p(-"++show (barSize - 1)++";+4)^fg(#33cc33)" ++ concat bars ++ "^p()"
    where barsH = map (\v -> ceiling $ (16-16 * (snd v) / lookupN (fst v) :: Double)) namevalues
          bars = map (\v -> "^fg(#aaaa33)^r(2x16)^p(-2)^fg(#000000)^r(2x" ++ show (max 0 v) ++ ")^p(+0)") barsH
          barSize = 2+2*(length values)
          lookupN :: String -> Double
          lookupN name = snd $ head' $ filter ((`isInfixOf` name) . fst) nominals
          nominals = [("_60_", 20.0), ("_80_", 22.0), ("_8t_", 33)]
          values = map snd namevalues
          head' [] = ("", 50.0)
          head' x = head x

updateStatus hnd i = do
  date <- liftM show getClockTime
  eths <- readProcess "unpaid-eth" ["a"] []
  btcusd <- readProcess "btc-price" ["a"] []
  hashes <- readProcess "hashrates.sh" [] []
  ethmins <- readProcess "eth-remainingmins.py" [] []
  let parsed_hashrate = map (\line -> let [n,v] = words line in (n, (read v) :: Double)) $ lines hashes
--  print hashes
--  let lala = map (\line -> let [n,v] = words line in (n, (read v) :: Double)) $ lines hashes
--  print lala
--  print $ drawBarGraph lala
  (memT, memF) <- memUsage
  (loadavg, cpus) <- loadAvg
  let load_red = floor $ min 255 $ 155.0 + loadavg * (100.0 / fromIntegral cpus)

  (bat0, ac0, batcap0) <- batInfo "0"
  (bat1, ac1, batcap1) <- batInfo "1"
  let ac0_bat_string' = show bat0 ++ " " ++ show ac0
  let ac0_bat_string'' | ac0 == Online = "AC " ++
                        case bat0 of Unknown  -> "C"
                                     Charging -> "C"
                                     Full     -> "F"
                                     _        -> "Err"
                       | ac0 == Offline =
                        case bat0 of Discharging -> "D"
                                     Full        -> "F"
                                     Empty       -> "E"
                                     _           -> "Err"

  let ac0_bat_string | ac0 == Online = "AC " ++ (head $ show bat0) : []
                     | ac0 == Offline = (head $ show bat0) : []

  let ac1_bat_string' = show bat1 ++ " " ++ show ac1
  let ac1_bat_string'' | ac1 == Online = "AC " ++
                                case bat1 of Unknown  -> "C"
                                             Charging -> "C"
                                             Full     -> "F"
                                             _        -> "Err"
                       | ac1 == Offline =
                        case bat1 of Discharging -> "D"
                                     Full        -> "F"
                                     Empty       -> "E"
                                     _           -> "Err"

  let ac1_bat_string | ac1 == Online = "AC " ++ (head $ show bat1) : []
                     | ac1 == Offline = (head $ show bat1) : []

  vol <- volume
  (most_cpu_eating_proc_percent, most_cpu_eating_proc) <- do
                        maxproc <- liftM ((Data.Text.split isSpace).(Data.Text.pack).trim) $ readProcess "sh" ["-c", "ps h -Ao pcpu,comm --sort=-pcpu | head -n 1"] ""
                        return $ case maxproc of [val, name] -> (2.55*(read $ Data.Text.unpack val)::Float, Data.Text.unpack name);  _ -> (0.0::Float, "ERR")

  hPutStrLn hnd $ Prelude.concatMap (wrap " " " ") $
--  putStrLn $ Prelude.concatMap (wrap " " " ") $
    [dzenColor "#999999" "" $ btcusd ++ "$"
    ,dzenColor "#999999" "" $ eths ++ "ETH," ++ head (words ethmins) ++ "h"
    ,drawBarGraph $ parsed_hashrate
      ,dzenColor "#999999" "" $ show (round $ sum $ map snd parsed_hashrate) ++ "MH/s," ++ show (length parsed_hashrate) ++ "m"
    ,dzenColor "#cccc22" "" (clicable (dzenEscape " - ") "5" "0x1008FF11")
    ++"/"++dzenColor "#cccc22" "" (clicable (dzenEscape " + ") "4" "0x1008FF13")
    ++dzenColor "#cccc22" "" (hBar 50 10 (if vol > 1 then 200 else 100) (floor $ 100*vol))
    ,dzenColor "#cccc22" "" $ show $ floor (vol*100)
--    ,dzenColor "#cc7722" "" $ hBar 50 10 200 (floor $ 100*(batcap0+batcap1))
--    ,dzenColor "#cc7722" "" $ ac0_bat_string ++ " " ++ show (floor $ 100*batcap0) ++ "%"
--    ,dzenColor "#cc7722" "" $ ac1_bat_string ++ " " ++ show (floor $ 100*batcap1) ++ "%"
    ,dzenColor ("#"++showHex load_red "9999") "" $ printf "%.2fL" loadavg
    ,dzenColor "#77bb22" "" $ hBar 50 10 memT (memT - memF)
    ,dzenColor "#77bb22" ""
    $ printf "%.2f" ((fromIntegral (memT - memF) / (1024*1024.0)) :: Float) ++ "GiB"
    ,dzenColor ("#"++(showHex2 ((floor most_cpu_eating_proc_percent)::Int)
                     (showHex2 (255-(floor most_cpu_eating_proc_percent)::Int) "aa"))) "" $ trim most_cpu_eating_proc
    ,dzenColor "#ffffff" "" $ dzenEscape $ trim date]

--  when (batcap0 + batcap1 < 6.0) blink_display
--      where blink_display = do
--          current <- readProcess "xbacklight" [] []
--          spawn $ "xbacklight =" ++ show (2*current/3)
--          threadDelay (seconds 0.2)
--          spawn $ "xbacklight =" ++ show current
--          threadDelay (seconds 0.2)
--          spawn $ "xbacklight =" ++ show (2*current/3)
--          threadDelay (seconds 0.2)
--          spawn $ "xbacklight =" ++ show current
  threadDelay (seconds 1)
--  putStrLn $ "### WOLOLO " ++ show i
  updateStatus hnd $ i+1

clicable :: String -> String -> String -> String
clicable l b key = "^ca("++b++",xdotool key "++key++")"++l++"^ca()"

hBar w h max val =
      "^ib(1)^ro("++show w++"x10)^p(-"++show (w-2)++")^r("
      ++ show pixval ++"x6)"
      ++ "^p(+" ++ show (w-pixval-2) ++ ")^ib(0)"
  where
    pixval = floor (fromIntegral (w - 4) * (fromIntegral val / fromIntegral max))

--dzenCmd = "dzen2 -x '600' -w '720' -ta 'r'" ++ style
--style   = " -h '16' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=7'"

dzenCmd = "dzen2 -x '2500' -w '1280' -ta 'r'" ++ style
style   = " -h '24' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=16'"

volume :: IO Float
volume = do
  con <- readProcess "amixer" ["sget", "Master"] []
  return $ extractVol con
  where
    extractVol con = ((/100).read) (vol++".0")
      where [[_,vol], _] = con =~ "^.*Playback.*\\[([0-9]*)%\\].*$" :: [[String]]

fromMaybe' x = liftM (fromMaybe x)

loadAvg :: IO (Float, Int)
loadAvg = do
  liftM2 (,) (fromMaybe' 0 $ extractor  "/proc/loadavg" extractUsage)
             (fromMaybe' 0 $ extractor "/proc/cpuinfo" extractCPUCount)
  where
    extractUsage con = read loadavg
      where [[_,loadavg]] = con =~ "^([0-9]*\\.[0-9]*).*$" :: [[String]]

    extractCPUCount con = length cpus
      where cpus = con =~ "^processor.*:\\ [0-9]*.*$" :: [[String]]

memUsage :: IO (Int, Int) -- max, current
memUsage = do
  fromMaybe' (0, 0) $ extractor "/proc/meminfo" extractUsage
  where
    extractUsage con =
      let [[_,_,mem_total]] = con =~ "^(MemTotal):\\ *([0-9]*).*$" :: [[String]]
          [[_,_,mem_free ]] = con =~ "^(MemFree):\\ *([0-9]*).*$" :: [[String]] in
        (read mem_total, read mem_free)

data BatState = Discharging | Charging | Full | Empty | Unknown
  deriving (Show, Read, Eq)
data ACState  = Offline | Online
  deriving (Show, Read, Enum, Eq)

batInfo :: String -> IO (BatState, ACState, Float)
batInfo idx = do
          a <- extractor ("/sys/class/power_supply/BAT"++idx++"/status") (read)
          b <- extractor "/sys/class/power_supply/AC/online" (toEnum.read)
          c <- (extractor ("/sys/class/power_supply/BAT"++idx++"/capacity") ((/100).fromIntegral.read))
          return (fromMaybe Unknown a,fromMaybe Online b,fromMaybe (0.0) c)

extractor :: FilePath -> (String -> a) -> IO (Maybe a)
extractor filename fun = do
  tryextract `catch` \e -> do
    putStrLn ("Caught " ++ show (e :: IOException))
    return Nothing
  where
    tryextract = withFile filename ReadMode fn
    fn hnd = do
      con <- hGetContents hnd
      evaluate (rnf con)
      return $ Just $ fun (con)
