import XMonad
import XMonad.Util.EZConfig (additionalKeys)

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders

-- Hooks
import Data.Ratio ((%))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

myModMask = mod1Mask

myKeys = [ ((myModMask, xK_p), spawn "dmenu_run -b")
	, ((myModMask, xK_q), spawn "killall dzen2; xmonad --recompile; xmonad --restart")]

mySpacing = 5

myLayout = spacing mySpacing $ smartBorders ( tiled ||| Mirror tiled ||| gridded ||| Full )
	where
		tiled = Tall nmaster delta ratio
		nmaster = 1
		delta = 1/2
		ratio = 1/2
		gridded = GridRatio (4/3)

main = xmonad $ defaultConfig
	{ modMask = myModMask
	, layoutHook = myLayout
	, terminal = "urxvt"
	, handleEventHook  = fullscreenEventHook
        , startupHook = setWMName "LG3D"
	} `additionalKeys` myKeys
