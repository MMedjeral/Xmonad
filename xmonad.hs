import XMonad

-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask
    , terminal = "alacritty"
    , startupHook = myStartupHook
    , layoutHook = spacingWithEdge 10 $ myLayout
    , manageHook = myManageHook
    }
  `additionalKeysP`
    [ ("M-S-f", spawn "Thunar")
    , ("M-p", spawn "dmenu_run -fn 'Noto sans-12'")
    , ("M-y", spawn "yandex-browser")
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "vlc" --> doFloat
    , className =? "Descargas" --> doFloat
    , className =? "lxappearance" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full ||| threeCol)
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill ~/backup/xmonad/bg.png"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "setxkbmap -layout es"
  spawnOnce "nm-applet &"
