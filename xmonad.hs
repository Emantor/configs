import XMonad
import XMonad.Config.Xfce

myManageHook :: ManageHook

myManageHook = composeAll [
    className =? "Xfce4-notifyd" --> doIgnore
    ]

myTerminal = "xfce4-terminal"


main = xmonad xfceConfig
		{ modMask = mod4Mask,
		  terminal = myTerminal,
		  manageHook = myManageHook <+> manageHook xfceConfig
		}
