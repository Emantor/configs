import XMonad

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
myWallpaper = "ww_3.jpg"
myWallpapers = ["ww_3.jpg", "samcha.png", "ME.png"]
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
myStartupHook conkyOffset = do
             --   spawn "killall conky"
                spawn ("conky -c ~/conkyrc -a top_right -x +" ++ conkyOffset)
                setWMName "LG3D"
                setWallpaper myWallpaper


-- ManageHook für Docks und Apps 
myManageHook = manageHook defaultConfig <+> manageDocks <+> composeAll
    [ className =? "Gimp"        --> doFloat
    , className =? "Pidgin"      --> doShift "im"
    , className =? "Steam"       --> doShift "steam"
    , className =? "Thunderbird" --> doShift "mail"
    , className =? "Guild Wars"  --> doFloat
    , className =? "Conky"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]

-- LogHooks
myLogHook :: Handle -> X ()
myLogHook xmproc1 = dynamicLogWithPP customPP { ppOutput = hPutStrLn xmproc1 }

                       
customPP :: PP
customPP = xmobarPP { ppLayout = xmobarColor "green" ""
                    , ppTitle = xmobarColor "cyan" "" . shorten 80
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
               onWorkspace "im" myChat $
               onWorkspace "steam" mySteam
               myGridFirst
                 where
                   -- Tile First Layout
                   myTileFirst = avoidStruts ( smartBorders (tiled ||| Mirror tiled ||| Grid ||| simpleTabbed ) ||| noBorders Full )
                     where
                       tiled = Tall nmaster delta ratio
                       nmaster = 1
                       ratio = 1/2
                       delta = 3/100
                   -- Grid as First Layout
                   myGridFirst = avoidStruts ( smartBorders (Grid ||| tiled ||| Mirror tiled ||| simpleTabbed) ||| noBorders Full )
                     where
                       tiled = Tall nmaster delta ratio
                       nmaster = 1
                       ratio = 1/2
                       delta = 3/100

                   -- Layout(s) for chat workspace
                   myChat = renamed [Replace "Chat"] $ avoidStruts (myChat' Grid)
                   -- Chat modifier, used on 7:chat workspace
                   myChat' base = mirror base $ withIM size roster
                     where
                      -- Ratios of the screen roster will occupy
                      size = 1%6
                      -- Match roster window
                      roster = Title "Buddy-Liste"

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
   , "web", "im", "irc", "mail" 
   , "steam", "music", "work", "vpn"
   , "virt", "ik", "weber"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList
       [ ("1",      "~")
       , ("2",      "~")
       , ("3",      "~")
       , ("4",      "~")
       , ("web",    "Download")
       , ("im",     "Download")
       , ("irc",    "Download")
       , ("mail",   "Download")
       , ("sfb880", "work/sfb880")
       , ("logo",   "work/logo")
       , ("adm",    "/etc")
       , ("steam",  ".steam")
       , ("tmp",    "tmp")
       , ("music",  "Musik")
       , ("work",   "work")
       , ("vpn",    "/etc/openvpn")
       , ("ik",  "work/infam")
       , ("weber",  "work/weber")
       , ("virt",   "work/virt")
       ]
   , defaultTopicAction = const spawnShell
   , defaultTopic = "1"
   , topicActions = M.fromList
       [ ("im",         spawn "pidgin")
       , ("irc",        spawn "urxvt -e weechat-curses")
       , ("mail",       spawn "thunderbird")
       , ("web",        spawn "firefox")
       , ("steam",      spawn "steam")
       , ("music",      spawn "urxvt -e ncmpcpp")
       , ("virt",       spawn "virt-manager")
       ]
   }

-- Functions
setWallpaper :: String -> X () 
setWallpaper strWallpaper = spawn $ "feh --bg-fill /home/phoenix/Bilder/" ++ strWallpaper

-- Helpfunctions for TopicSpace
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvt -e /bin/sh -c 'cd " ++ dir ++ " && " ++ "zsh" ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

-- Application List
appList :: [String]
appList =  [  "smplayer", "xbmc", "firefox", "thunderbird"
           , "pidgin", "urxvt -e weechat-curses", "libreoffice", "wireshark"
           , "virt-manager", "gimp"
           ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
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
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then killall conky & xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- Volume Control
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -- -1%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1%")
    , ((0, xF86XK_AudioMute), spawn "~/bin/pa_toggle.sh")
    -- Brightness Control
    , ((mod1Mask, xF86XK_AudioLowerVolume), spawn "xbacklight -10")
    , ((mod1Mask, xF86XK_AudioRaiseVolume), spawn "xbacklight +10")
    -- Brightness Hotkeys: 1 25 50 75 100
    , ((mod1Mask .|. mod4Mask, xK_1), spawn "xbacklight -t 0 -set 1")
    , ((mod1Mask .|. mod4Mask, xK_2), spawn "xbacklight -t 0 -set 25")
    , ((mod1Mask .|. mod4Mask, xK_3), spawn "xbacklight -t 0 -set 50")
    , ((mod1Mask .|. mod4Mask, xK_4), spawn "xbacklight -t 0 -set 75")
    , ((mod1Mask .|. mod4Mask, xK_5), spawn "xbacklight -t 0 -set 100")
    -- Show and hide dock
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    -- show and hide border
    , ((mod4Mask, xK_n), withFocused toggleBorder)
    , ((mod4Mask .|. shiftMask, xK_Return), spawnShell)
    -- Firefox Hotkey
    , ((mod4Mask, xK_f), spawn "firefox")
    -- MPD Control External mpd Server Bibliothekar
    , ((mod4Mask, xF86XK_AudioPlay), spawn "mpc -h 192.168.213.151 toggle")
    , ((mod4Mask, xF86XK_AudioPrev), spawn "mpc -h 192.168.213.151 prev")
    , ((mod4Mask, xF86XK_AudioNext), spawn "mpc -h 192.168.213.151 next")
    -- Local mpd control
    , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
    , ((0, xF86XK_AudioPrev), spawn "mpc prev")
    , ((0, xF86XK_AudioNext), spawn "mpc next")
    -- Lock Xsession with Key Combination
    , ((mod1Mask .|. mod4Mask, xK_BackSpace), spawn "xscreensaver-command -lock")
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

myConfig xmprocHandle conkyOffset = defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook 
        , startupHook = myStartupHook conkyOffset
        , workspaces = myTopics
        , keys = myKeys
        , logHook = myLogHook xmprocHandle
        , modMask = myModMask     -- Rebind Mod to the Windows key
        , terminal = myTerminal
        , borderWidth = 1
        , focusedBorderColor = "blue"
        }

-- Main Loop
main = do
    -- Set Wallpaper
    -- spawn $ "feh --bg-fill " ++ myWallpaper
    -- start Xmobar
    xmproc1 <- spawnPipe "/usr/bin/xmobar -x 0 /home/phoenix/.xmobarrc"
    -- start XMonad
    rects <- openDisplay "" >>= getScreenInfo
    case rects of
            [Rectangle {rect_x = 0, rect_y = 0, rect_width = 1600, rect_height=900}] 
              -> xmonad $ withUrgencyHook NoUrgencyHook (myConfig xmproc1 "40");
            [  Rectangle {rect_x = 1080, rect_y = 0, rect_width = 1920, rect_height = 1200}
              ,Rectangle {rect_x = 3000, rect_y = 0, rect_width = 1920, rect_height = 1080}
              ,Rectangle {rect_x = 0, rect_y = 0, rect_width = 1080, rect_height = 1920}]
              -> xmonad $ withUrgencyHook NoUrgencyHook (myConfig xmproc1 "1940");
            _ -> xmonad $ withUrgencyHook NoUrgencyHook (myConfig xmproc1 "40")
