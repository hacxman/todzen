-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Util.Run(spawnPipe, safeSpawnProg)  -- spawnPipe and hPutStrLn
import XMonad.Hooks.UrgencyHook
import System.IO                   -- hPutStrLn scope
import qualified XMonad.StackSet as W
import XMonad.Layout.Maximize

import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import qualified Data.Map as M
import XMonad.Actions.NoBorders

import Control.Concurrent
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Loggers
import XMonad.Layout.Tabbed
import XMonad.Layout.TabBarDecoration

main = do
     status <- spawnPipe myDzenStatus    -- xmonad status on the left
     safeSpawnProg "/home/mzatko/.xmonad/todzen"
--     conky  <- spawnPipe myDzenConky     -- conky stats on the right
     session <- getEnv "DESKTOP_SESSION"
     safeSpawnProg "stalonetray"
     safeSpawnProg "nm-applet"
     safeSpawnProg "xscreensaver"
     safeSpawnProg "/usr/libexec/lxpolkit"
     xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ maybe desktopConfig (desktop status) session

desktop _ "gnome" = gnomeConfig
desktop _ "kde" = kde4Config
desktop _ "xfce" = xfceConfig
desktop _ "xmonad-gnome" = gnomeConfig
desktop status _ = let cnf = defaultConfig { modMask = mod4Mask
                          , manageHook = manageDocks <+> myManageHook
                                         <+> manageHook defaultConfig
                          , workspaces = myWorkspaces
                          , logHook    = myLogHook status
                          , layoutHook = avoidStruts $ {-smartBorders $-} myLayoutHook
--                          , eventHook = myEventHook
                          , terminal = "xfce4-terminal"} in
            cnf `additionalKeys` (myKeys cnf)

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
myLayoutHook =  tiled ||| mtiled ||| full -- Full
  where
    --full    = simpleTabbed ||| Full
    full    = Full
    mtiled  = Mirror tiled
    tiled   = Tall 1 (3/100) (1/2) --Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- myEventHook event = return (All True)

myKeys conf@(XConfig {XMonad.modMask = modm}) =
              [((modm, xK_Right)               , nextWS)
              ,((modm, xK_Left)                , prevWS)
              ,((modm, xK_Escape)              , toggleWS)
              ,((modm, xK_Return)              , spawn $ XMonad.terminal conf)
              ,((modm .|. shiftMask, xK_Return), windows W.swapMaster)
              ,((modm, xK_m)                   , withFocused (sendMessage . maximizeRestore))
              ,((modm,  xK_g ),   withFocused toggleBorder)
              ,((mod1Mask.|.controlMask, xK_l) , spawn "xscreensaver-command --lock")
              ,((0                     , 0x1008FF03), spawn "xbacklight -dec 10 -steps 1")
              ,((0                     , 0x1008FF02), spawn "xbacklight -inc 10 -steps 1")
              ,((0                     , 0x1008FF11), spawn "amixer -c 0 -- sset Master playback 2dB-")
              ,((0                     , 0x1008FF13), spawn "amixer -c 0 -- sset Master playback 2dB+")
              ,((0                     , 0x1008FF12), spawn "amixer --set Master toggle")
              ,((0                     , 0x1008ffb2), spawn "dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true")]
--              ,((0                     , 0x1008ff2f), spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.UPower\"  /org/freedesktop/UPower org.freedesktop.UPower.Suspend")]
              --,((mod1Mask.|.controlMask, xK_l) , spawn "gnome-screensaver-command --lock")]


-- Window management
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
--    , className =? "XCalc"          --> doFloat
--    , className =? "Chromium"       --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
--    , className =? "Nautilus"       --> doF (W.shift (myWorkspaces !! 2)) -- send to ws 3
--    , className =? "Gimp"           --> doF (W.shift (myWorkspaces !! 3)) -- send to ws 4
    , className =? "stalonetray"    --> doIgnore
    , className =? "eekboard"    --> doFloat
    ]

-- Statusbar
--
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }


myWorkspaces            :: [String]
myWorkspaces            = clickable . (map (dzenEscape.wrap " " " ")) $ ["1","2","3","4","5","6","7","8","9"]

  where clickable l     = [ "^ca(1,xdotool key super+" ++ trim (show (n)) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]


--myDzenStatus = "dzen2 -w '732' -ta 'l'" ++ myDzenStyle
--myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '732' -w '708' -ta 'r'" ++ myDzenStyle
myDzenStatus = "dzen2 -w '600' -ta 'l' " ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '600' -w '700' -ta 'c'" ++ myDzenStyle
myDzenStyle  = " -h '16' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=9'"


myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "#223322"
    , ppHidden  = dzenColor "#dddddd" ""
    , ppHiddenNoWindows = dzenColor "#777777" ""
    , ppUrgent  = dzenColor "#ff0000" ""
    , ppWsSep   = "|"
    , ppSep     = "     "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" ""
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 80 . dzenEscape
--    , ppExtras = [ padL loadAvg, logCmd "fortune -n 40 -s" ]
    }

