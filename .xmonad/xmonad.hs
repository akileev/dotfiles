import XMonad

main = xmonad defaultConfig
	{ modMask = mod2Mask
	, terminal = 'urxvt'
	}
