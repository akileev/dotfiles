import XMonad
import XMonad.Util.EZConfig (additionalKeys)

myModMask = mod1Mask

myKeys = [ ((myModMask, xK_p), spawn "dmenu_run -b")
	, ((myModMask, xK_q), spawn "killall dzen2; xmonad --recompile; xmonad --restart")]

main = xmonad $ defaultConfig
	{ modMask = myModMask
	, terminal = "urxvt"
	} `additionalKeys` myKeys
