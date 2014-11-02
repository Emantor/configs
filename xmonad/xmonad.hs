import XMonad hiding ( (|||) )

import XMonad.Config.Gnome

-- Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.DwmStyle
import XMonad.Layout.Decoration
import XMonad.Layout.Drawer
import XMonad.Layout.LayoutCombinators


-- Actions
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FloatNext

-- Java Workaround
import XMonad.Hooks.SetWMName
import Data.Ratio ((%))

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.Prompt.Window
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

-- Misc
import XMonad.Util.Run(spawnPipe,unsafeSpawn,safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Variables
myWallpaper = "ww_gun.jpg"
myWallpapers = ["ww_3.jpg", "ww_gun.jpg", "samcha.png", "ME.png","bi.jpg","bg.jpg","tgm.png","samcha.png","ww_girlonroof.jpg","ww_girlhp.jpg","ww_drachen.jpg","ww_schloss.png","ww_schweden.png"]
myTerminal  = "urxvt"
myModMask   = mod4Mask

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig

-- Wallpaper Prompt
data Wallpaper = Wallpaper

instance XPrompt Wallpaper where
     showXPrompt Wallpaper = "WP: "

promptWallpaper :: XPConfig -> X()
promptWallpaper c = do
     mkXPrompt Wallpaper c (mkComplFunFromList myWallpapers) setWallpaper

-- StartupHook
myStartupHook = do
--                setWMName "LG3D"
                setWallpaper myWallpaper


-- ManageHook für Docks und Apps 
myManageHook = manageHook defaultConfig <+> manageDocks <+> composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "Pidgin"           --> doShift "im"
    , className =? "Steam"            --> doShift "steam"
    , className =? "Thunderbird"      --> doShift "mail"
    , className =? "Guild Wars"       --> doFloat
    , className =? "Conky"            --> doIgnore
    , appName   =? "floatingTerminal" --> doRectFloat (W.RationalRect 0.15 0.5 0.5 0.4)
    , isFullscreen                    --> doFullFloat
    ]

-- LogHooks
myLogHook :: Handle -> X ()
myLogHook xmproc1 = dynamicLogWithPP customPP { ppOutput = hPutStrLn xmproc1 }

                       
customPP :: PP
customPP = xmobarPP { ppLayout = xmobarColor "green" ""
                    , ppTitle = xmobarColor "cyan" "" . shorten 65
                    , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                    , ppCurrent = xmobarColor "#FF0088" ""
                    , ppSep = "/"
                    , ppWsSep = "/"
                    , ppVisible = xmobarColor "#00FF88" ""
                    , ppHidden = xmobarColor "#0088FF" ""
                    , ppHiddenNoWindows = xmobarColor "#88FF00" ""
                    }

-- LayoutHook: certain workspaces get a specific layout or layout order.
myLayoutHook = onWorkspace "web" myTileFirst $
               onWorkspace "steam" mySteam
               myTileFirst
                 where
                   -- Tile First Layout
                   myTileFirst = avoidStruts ( smartBorders ( renamed [CutLeft 9] ( dwmStyle shrinkText defaultTheme ( tiled ||| renamed [Replace "MiTa"] (Mirror tiled) ) ) ||| renamed [Replace "SiTa"] simpleTabbed ) ||| noBorders Full )
                     where
                       drawer = simpleDrawer 0.0 0.3 ( ClassName "drawerTerminal" )
                       tiled = Tall nmaster delta ratio
                       nmaster = 1
                       ratio = 1/2
                       delta = 3/100

                   -- mirror modifier used for chat and steam
                   mirror base a = reflectHoriz $ a $ reflectHoriz base

                   -- Layout(s) for steam workspace
                   mySteam = renamed [Replace "Steam"] $ avoidStruts (mySteam' Grid)
                   -- Steam modifier, used on steam workspace
                   mySteam' base = mirror base $ withIM size roster
                     where
                      -- Ratios of the screen roster will occupy
                      size = 1%6
                      -- Match roster window
                      roster = Title "Friends"

-- Topicspace
myTopics :: [Topic]
myTopics =
   [ "1", "2", "3", "4" -- 4 unnamed workspaces
   , "web", "irc", "mail", "photo"
   , "steam", "music", "work", "virt"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList
       [ ("1",      "~")
       , ("2",      "~")
       , ("3",      "~")
       , ("4",      "~")
       , ("web",    "Download")
       , ("irc",    "Download")
       , ("mail",   "Download")
       , ("steam",  ".steam")
       , ("music",  "Musik")
       , ("photo",  "Bilder")
       , ("work",   "work")
       , ("virt",   "work/virt")
       ]
   , defaultTopicAction = const spawnShell
   , defaultTopic = "1"
   , topicActions = M.fromList
       [ ("im",         spawn "pidgin")
       , ("irc",        spawn "urxvt -e weechat-curses")
       , ("mail",       spawn "thunderbird")
       , ("web",        spawn "chromium --password-store=gnome")
       , ("steam",      spawn "steam")
       , ("music",      spawn "urxvt -e ncmpcpp")
       , ("photo",      spawn "darktable")
       , ("virt",       spawn "virtualbox")
       ]
   }

-- Functions
setWallpaper :: String -> X () 
setWallpaper strWallpaper = spawn $ "feh --bg-fill /home/phoenix/Bilder/" ++ strWallpaper

-- Helpfunctions for TopicSpace
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn shelldir = spawn $ "urxvt -e /bin/sh -c 'cd " ++ shelldir ++ " && " ++ "exec zsh" ++ "'"

spawnTmux :: X ()
spawnTmux = currentTopicDir myTopicConfig >>= spawnTmuxIn

spawnTmuxIn :: Dir -> X ()
spawnTmuxIn tmuxdir = spawn $ "urxvt -e /bin/sh -c 'cd " ++ tmuxdir ++ " && tmux -q has-session -t " ++ tmuxdir ++ "&& tmux attach-session -d -t " ++ tmuxdir ++ " || tmux new-session -s" ++ tmuxdir ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

-- Application List
appList :: [String]
appList =  [  "ristretto", "xbmc", "libreoffice", "wireshark"
           , "virt-manager", "gimp", "emerillon"
           ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask          , xK_asciicircum), spawn "urxvt -name floatingTerminal") -- %! Launch terminal
    , ((modMask .|. shiftMask, xK_asciicircum), spawn "urxvt -name drawerTerminal") -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q                   ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q                   ), spawn "if type xmonad; then pkill -TERM -P `pgrep -o xmonad` && xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- Volume Control
    , ((0, xF86XK_AudioLowerVolume                    ), spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -- -1%")
    , ((0, xF86XK_AudioRaiseVolume                    ), spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1%")
    , ((0, xF86XK_AudioMute                           ), spawn "pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
    -- Brightness Control
    , ((mod1Mask, xF86XK_AudioLowerVolume             ), spawn "sudo light -sq 10")
    , ((mod1Mask, xF86XK_AudioRaiseVolume             ), spawn "sudo light -aq 10")
    -- Brightness Hotkeys: 1 25 50 75 100
    , ((mod1Mask .|. mod4Mask, xK_1                   ), spawn "sudo light -q 1")
    , ((mod1Mask .|. mod4Mask, xK_2                   ), spawn "sudo light -q 25")
    , ((mod1Mask .|. mod4Mask, xK_3                   ), spawn "sudo light -q 50")
    , ((mod1Mask .|. mod4Mask, xK_4                   ), spawn "sudo light -q 75")
    , ((mod1Mask .|. mod4Mask, xK_5                   ), spawn "sudo light -q 100")
    -- Show and hide dock
    , ((mod4Mask, xK_b                                ), sendMessage ToggleStruts)
    -- Show and hide battery overlay
    , ((mod4Mask, xK_i                                ), spawn "/usr/bin/showbatt")
    -- show and hide border
    , ((mod4Mask, xK_n                                ), withFocused toggleBorder)
    , ((mod4Mask .|. shiftMask, xK_Return             ), spawnShell)
    -- Firefox Hotkey
    , ((mod4Mask, xK_f                                ), spawn "chromium --password-store=gnome")
    -- MPD Control External mpd Server Bibliothekar
    , ((mod4Mask, xF86XK_AudioPlay                    ), spawn "mpc -h librarian toggle")
    , ((mod4Mask, xF86XK_AudioPrev                    ), spawn "mpc -h librarian prev")
    , ((mod4Mask, xF86XK_AudioNext                    ), spawn "mpc -h librarian next")
    , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Down  ), spawn "mpc -h librarian toggle")
    , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Left  ), spawn "mpc -h librarian prev")
    , ((mod1Mask .|. mod4Mask .|. controlMask, xK_Right ), spawn "mpc -h librarian next")
    -- Local mpd control
    , ((0, xF86XK_AudioPlay                           ), spawn "/home/phoenix/bin/playpause")
    , ((0, xF86XK_AudioPrev                           ), spawn "mpc prev")
    , ((0, xF86XK_AudioNext                           ), spawn "mpc next")
    , ((mod1Mask .|. mod4Mask, xK_Down                ), spawn "/home/phoenix/bin/playpause")
    , ((mod1Mask .|. mod4Mask, xK_Left                ), spawn "mpc prev")
    , ((mod1Mask .|. mod4Mask, xK_Right               ), spawn "mpc next")
    , ((mod1Mask .|. mod4Mask, xF86XK_AudioLowerVolume                    ), spawn "mpc volume -1")
    , ((mod1Mask .|. mod4Mask, xF86XK_AudioRaiseVolume                    ), spawn "mpc volume +1")
    -- Lock Xsession with Key Combination
    , ((mod1Mask .|. mod4Mask, xK_BackSpace), spawn "xscreensaver-command --lock")
    -- Select workspace from prompt
    , ((mod4Mask              , xK_z     ), promptedGoto)
    , ((mod4Mask .|. shiftMask, xK_z     ), promptedShift)
    -- Cycle Through Workspaces
    , ((mod4Mask .|. shiftMask, xK_h     ), prevWS)
    , ((mod4Mask .|. shiftMask, xK_l     ), nextWS)
    -- Reload Wallpaper
    , ((mod4Mask .|. shiftMask, xK_p     ), promptWallpaper myXPConfig)
    -- Select workspace from Grid
    , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
    , ((mod4Mask, xK_o), spawnSelected defaultGSConfig appList)
    -- SSH Hotkey
    , ((modMask .|. shiftMask, xK_s), sshPrompt defaultXPConfig)
    -- , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    -- repeat the binding for non-American layout keyboards
    -- , ((modMask              , xK_question), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myConfig xmprocHandle = defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook 
        , startupHook = myStartupHook
        , workspaces = myTopics
        , keys = myKeys
        , logHook = myLogHook xmprocHandle
        , modMask = myModMask     -- Rebind Mod to the Windows key
        , terminal = myTerminal
        , borderWidth = 1
        , focusedBorderColor = "orange"
        }

-- Main Loop
main = do
    -- Set Wallpaper
    -- spawn $ "feh --bg-fill " ++ myWallpaper
    -- Spawn trayer
    spawn "trayer --monitor primary --align right --widthtype percent --width 10 --edge top --height 22 --tint 0x111111 --alpha 0 --transparent true --SetDockType true --SetPartialStrut true"
    -- Spawn stratum0trayicon
    spawn "stratum0trayicon"
    -- Spawn nm-applett
    spawn "nm-applet"
    -- Spawn dunst
    spawn "dunst"
    -- Spawn pasystray
    spawn "pasystray"
    -- Spawn xscreensaver
    spawn "/usr/bin/xscreensaver -no-splash"
    -- Spawn mpd_inhibit
    spawn "/home/phoenix/bin/mpdinhibit"
    -- start Xmobar
    xmproc1 <- spawnPipe "/usr/bin/xmobar -x 0 /home/phoenix/.xmobarrc"
    -- Setup gpg-agent to use the right pinentry
    spawn "echo UPDATESTARTUPTTY | gpg-connect-agent"
    -- start XMonad
    xmonad $ withUrgencyHook NoUrgencyHook (myConfig xmproc1 );
