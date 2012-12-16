import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


-- ManageHook für Docks und Apps 
myManageHook = manageHook defaultConfig <+> manageDocks <+> composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Steam"      --> doFloat <+> doFullFloat <+> doIgnore
    , className =? "Pidgin"    --> doShift "7:chat"
    , className =? "Thunderbird"    --> doShift "8:mail"
    ]

-- LogHooks
myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn xmproc }
                       
customPP :: PP
customPP = xmobarPP { ppTitle = xmobarColor "blue" "" . shorten 100 } 

fadeLogHook :: X ()
fadeLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

-- LayoutHook mit SmartBorders und Struts 
myLayoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig

-- Workspaces
myWorkspaces = ["1","2","3","4","5","6","7:chat","8:mail","9:movie"]

-- Xmonad starten
main = do
    -- Xmobar starten
    xmproc <- spawnPipe "/usr/bin/xmobar /home/phoenix/.xmobarrc"
    -- Xmonad starten
    xmonad $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook 
        , logHook = myLogHook xmproc >> (fadeLogHook)
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "urxvt"
        , borderWidth = 2
        , focusedBorderColor = "#FF0000"
        , workspaces = myWorkspaces
        } `additionalKeys`
        -- Headphone Volume
        [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Headphone playback 1-")
        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Headphone playback 1+")
        -- Master Volume
        , ((mod4Mask, xF86XK_AudioLowerVolume), spawn "amixer set Master playback 1-")
        , ((mod4Mask, xF86XK_AudioRaiseVolume), spawn "amixer set Master playback 1+")
        -- Brightness
        , ((mod1Mask, xF86XK_AudioLowerVolume), spawn "xbacklight -10")
        , ((mod1Mask, xF86XK_AudioRaiseVolume), spawn "xbacklight +10")
        -- Brightness Hotkeys: 1 25 50 75 100
        , ((mod1Mask .|. mod4Mask, xK_1), spawn "xbacklight =1")
        , ((mod1Mask .|. mod4Mask, xK_2), spawn "xbacklight =25")
        , ((mod1Mask .|. mod4Mask, xK_3), spawn "xbacklight =50")
        , ((mod1Mask .|. mod4Mask, xK_4), spawn "xbacklight =75")
        , ((mod1Mask .|. mod4Mask, xK_5), spawn "xbacklight =100")
        --Dock ein und ausblenden
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        -- Apps starten
        , ((mod4Mask, xK_f), spawn "firefox")
        -- MPD Control Bibliothekar
        , ((mod4Mask, xF86XK_AudioPlay), spawn "mpc -h bibliothekar toggle")
        , ((mod4Mask, xF86XK_AudioPrev), spawn "mpc -h bibliothekar prev")
        , ((mod4Mask, xF86XK_AudioNext), spawn "mpc -h bibliothekar next")
        -- Touchpad toggle
        -- , ((0, xF86XK_TouchpadToggle), spawn "/sbin/trackpad-toggle.sh")
        -- Cmus Remote (Currently not working)
        , ((0, xF86XK_AudioPlay), spawn "cmus-remote -u")
        , ((0, xF86XK_AudioPrev), spawn "cmus-remote -p")
        , ((0, xF86XK_AudioNext), spawn "cmus-remote -n")
        ]
