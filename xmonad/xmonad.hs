import XMonad hiding ( (|||) )

-- Layout
import XMonad.Layout.FixedColumn
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Master


-- Actions
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces


-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

-- Java Workaround
import XMonad.Hooks.SetWMName
import Data.Ratio ((%))

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Prompt.Ssh

-- Misc
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad.Util.NamedScratchpad
import XMonad.Util.Themes

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Variables
myWallpaper :: [Char]
myWallpaper = "skate_bg.jpg"

myWallpapers :: [[Char]]
myWallpapers = ["ww_3.jpg", "ww_gun.jpg", "samcha.png", "ME.png","bi.jpg",
                "bg.jpg","tgm.png","samcha.png","ww_girlonroof.jpg",
                "ww_girlhp.jpg","ww_drachen.jpg","ww_schloss.png",
                "ww_schweden.png","skate_bg.jpg"]

myTerminal :: [Char]
myTerminal  = "urxvt"

myModMask :: KeyMask
myModMask   = mod4Mask

myXPConfig :: XPConfig
myXPConfig = def

-- Wallpaper Prompt
data Wallpaper = Wallpaper

instance XPrompt Wallpaper where
    showXPrompt Wallpaper = "WP: "


promptWallpaper :: XPConfig -> X()
promptWallpaper c = do
     mkXPrompt Wallpaper c (mkComplFunFromList myWallpapers) setWallpaper

-- StartupHook
myStartupHook :: X()
myStartupHook = do
                setWMName "LG3D"
                setWallpaper myWallpaper


-- ManageHook für Docks und Apps
myManageHook :: ManageHook
myManageHook = manageHook def <+> namedScratchpadManageHook myScratchpads <+> manageDocks <+> composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "Darktable"        --> doShift "photo"
    , className =? "VirtualBox"       --> doShift "virt"
    , className =? "Pidgin"           --> doShift "im"
    , className =? "Steam"            --> doShift "steam"
    , className =? "Thunderbird"      --> doShift "mail"
    , className =? "Firefox"          --> doShift "web"
    , className =? "Gnucash"          --> doShift "finance"
    , className =? "skype"            --> doShift "skype"
    , className =? "seperate_mpv"     --> doShift "video"
    , className =? "Guild Wars"       --> doFloat
    , className =? "Conky"            --> doIgnore
    , isFullscreen                    --> doFullFloat
    ]


myScratchpads :: [NamedScratchpad]
myScratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "htop" "urxvt -name htopterm -e htop" (appName =? "htopterm")
        (customFloating $ W.RationalRect (0) (0) (1/2) (1)) ,

-- run stardict, find it by class name, place it in the floating window
-- 1/6 of screen width from the left, 1/6 of screen height
-- from the top, 2/3 of screen width by 2/3 of screen height
    NS "journal" "urxvt -name journalterm -e journalctl -n 100 -f" (appName =? "journalterm")
        (customFloating $ W.RationalRect (1/2) (0) (1/2) (1)) ,

    NS "python2" "urxvt -name python2term -e ipython2" (appName =? "python2term")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

    NS "python3" "urxvt -name python3term -e ipython3" (appName =? "python3term")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,
-- run gvim, find by role, don't float
    NS "floatterm" "urxvt -name floatingTerminal -e tmux new-session -A -s quick" (appName =? "floatingTerminal")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))

   ]

customPP :: PP
customPP = xmobarPP { ppLayout = xmobarColor "green" ""
                    , ppTitle = xmobarColor "cyan" ""
                    , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                    , ppCurrent = xmobarColor "#FF0088" ""
                    , ppSep = " | "
                    , ppWsSep = "/"
                    , ppVisible = xmobarColor "#00FF88" ""
                    , ppHidden = xmobarColor "#0088FF" ""
                    , ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort def
                    , ppHiddenNoWindows = xmobarColor "#88FF00" ""
                    }

-- LayoutHook: certain workspaces get a specific layout or layout order.
myLayoutHook = onWorkspace "web" myTileFirst $
               onWorkspace "steam" mySteam $
               onWorkspace "irc" myIRC $
               onWorkspace "skype" mySkype $
               myTileFirst
                 where
                   -- Tile First Layout
                   myTileFirst = avoidStruts (
                                  smartBorders (
                                   mkToggle (FULL ?? NOBORDERS ?? EOT) (
                                    renamed [CutLeft 9] (
                                     dwmStyle shrinkText myDWConfig (
                                      renamed [Replace "Tile"] (tiled) |||
                                      renamed [Replace "FixC"] (FixedColumn 1 20 83 10) |||
                                      renamed [Replace "MiTa"] (Mirror tiled)
                                     )
                                    ) |||
                                    renamed [Replace "SiTa"] ( simpleTabbed ) |||
                                    renamed [Replace "MiTa"] ( mastered (1/100) (1/2) $ simpleTabbed ) |||
                                    renamed [Replace "TRFC"] ( topRightMaster (FixedColumn 1 20 83 10) )
                                   )
                                  ) ||| noBorders Full
                                 )
                     where
                       tiled = Tall nmaster delta ratio
                       nmaster = 1
                       ratio = 1/2
                       delta = 1/100

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

                   myIRC = renamed [Replace "IRC"] $ avoidStruts (myIRC' Grid)
                   -- IRC modifier, used on steam workspace
                   myIRC' base = mirror base $ withIM size roster
                     where
                      -- Ratios of the screen roster will occupy
                      size = 1%3
                      -- Match roster window
                      roster = ClassName "Corebird"

                   mySkype = renamed [Replace "Skype"] $ avoidStruts (mySkype' Grid)
                   -- Skype modifier, used on steam workspace
                   mySkype' base = mirror base $ withIM size roster
                     where
                      -- Ratios of the screen roster will occupy
                      size = 1%3
                      -- Match roster window
                      roster = Title "phoenix_firebow - Skype™"

myDWConfig :: Theme
myDWConfig = (theme smallClean)
-- Topicspace
myTopics :: [Topic]
myTopics =
   [ "NSP",
     "web",
     "irc",
     "mail",
     "work",
     "photo",
     "steam",
     "music",
     "beam",
     "virt",
     "finance",
     "signal",
     "skype",
     "video"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = def
    { topicDirs = M.fromList
      (zip (tail myTopics) [
                      "Download"
                    , "Download"
                    , "Download"
                    , "work"
                    , "Bilder"
                    , ".steam"
                    , "Musik"
                    , "work/beam"
                    , "work"
                    , "work"
                    , "Download"
                    , "Download"
                    , "Videos"])
   , defaultTopicAction = const spawnShell
   , defaultTopic = "web"
   , topicActions = M.fromList
     (zip (tail myTopics) [
                    spawn "firefox"
                  , spawn "urxvt -e weechat-curses"
                  , spawn "thunderbird"
                  , spawn "emacsclient -c -a="
                  , spawn "darktable"
                  , spawn "steam"
                  , spawn "urxvt -e ncmpcpp"
                  , spawnShell
                  , spawn "virt-manager"
                  , spawn "gnucash"
                  , spawn "/home/phoenix/bin/signal"
                  , spawn "skype"
                  , spawnShell ])
   }

-- Functions
setWallpaper :: String -> X ()
setWallpaper strWallpaper = spawn $ "feh --bg-fill /home/phoenix/Bilder/" ++ strWallpaper

-- Helpfunctions for TopicSpace
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn shelldir = case shelldir of
                          "" -> spawn $ "urxvt"
                          otherwise -> spawn $ "urxvt -e /bin/sh -c 'cd " ++ shelldir ++ " && exec zsh'"


-- spawnTmux :: X ()
-- spawnTmux = currentTopicDir myTopicConfig >>= spawnTmuxIn
--
-- spawnTmuxIn :: Dir -> X ()
-- spawnTmuxIn tmuxdir = spawn $ "urxvt -e /bin/sh -c 'cd " ++ tmuxdir ++ " && tmux -q has-session -t " ++ tmuxdir ++ "&& tmux attach-session -d -t " ++ tmuxdir ++ " || tmux new-session -s" ++ tmuxdir ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt def goto

promptedShift :: X ()
promptedShift = workspacePrompt def $ windows . W.shift

-- Application List
appList :: [String]
appList =  [  "emacsclient -c -a=", "evince", "google-chrome-stable", "xbmc", "libreoffice", "wireshark"
           , "virt-manager", "gimp", "eog"
           ]

screenList :: [String]
screenList =  [  "xrandr --output DP-1 --off xrandr --output DP-2 --off --output HDMI1 --off", "xrandr --output HDMI1 --auto --above eDP-1", "xrandr --output HDMI --auto --right-of eDP-1", "xrandr --output HDMI --auto --left-of eDp1"
           , "xrandr --output DP-1 --off", "xrandr --output DP-2 --off", "dockhome", "undock"
           ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "~/.menu/menu main") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask,               xK_a     ), appendWorkspacePrompt myXPConfig)
    , ((modMask,               xK_d     ), removeWorkspace)

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- Scratchpads
    , ((modMask                 , xK_asciicircum), namedScratchpadAction myScratchpads "floatterm") -- %! Launch terminal
    , ((modMask .|. controlMask , xK_1), namedScratchpadAction myScratchpads "python2")
    , ((modMask .|. controlMask , xK_2), namedScratchpadAction myScratchpads "python3")
    , ((modMask .|. mod1Mask    , xK_j), namedScratchpadAction myScratchpads "journal")
    , ((modMask .|. mod1Mask    , xK_h), namedScratchpadAction myScratchpads "htop")

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
    , ((modMask              , xK_q                   ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- Volume Control
    , ((0, xF86XK_AudioLowerVolume                    ), spawn "/usr/bin/pulseaudio-ctl down")
    , ((0, xF86XK_AudioRaiseVolume                    ), spawn "/usr/bin/pulseaudio-ctl up")
    , ((0, xF86XK_AudioMute                           ), spawn "/usr/bin/pulseaudio-ctl mute")
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
    -- Fullscreen Hotkey
    , ((mod4Mask , xK_f                               ), sendMessage $ Toggle FULL)
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
    , ((mod4Mask, xK_g), goToSelected def)
    , ((mod4Mask, xK_o), spawnSelected def appList)
    , ((mod4Mask, xK_u), spawnSelected def screenList)
    -- SSH Hotkey
    , ((modMask .|. shiftMask, xK_s), sshPrompt def)
    ]
    ++
    -- mod-[1..9] %! Switch to Topic N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), f )
        | (i, k) <- zip (tail (XMonad.workspaces conf)) [xK_1 .. xK_9]
        , (f, m) <- [ (switchTopic myTopicConfig i, 0), (windows $ W.shift i, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myConfig = def
        { manageHook = myManageHook
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , workspaces = myTopics
        , keys = myKeys
        , logHook = dynamicLogString customPP >>= xmonadPropLog
        , modMask = myModMask     -- Rebind Mod to the Windows key
        , terminal = myTerminal
        , borderWidth = 1
        , focusFollowsMouse = False
        , focusedBorderColor = "blue"
        , handleEventHook = handleEventHook def
        }

-- Main Loop
main :: IO()
main = do
    -- Spawn xscreensaver
    spawn "/usr/bin/xscreensaver -no-splash"
    -- Start xmonad.target
    spawn "systemctl --user start xmonad.target"
    -- start XMonad
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh (myConfig);
