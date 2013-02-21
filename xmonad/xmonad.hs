import XMonad

-- Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect

-- Actions
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.DynamicLog
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

-- Misc
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig

-- ManageHook für Docks und Apps 
myManageHook = manageHook defaultConfig <+> manageDocks <+> composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Pidgin"    --> doShift "im"
    , className =? "Steam"    --> doShift "steam"
    , className =? "Thunderbird"    --> doShift "mail"
    , className =? "Guild Wars"    --> doFloat
    , isFullscreen --> doFullFloat
    ]

-- LogHooks
myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn xmproc }
                       
customPP :: PP
customPP = xmobarPP {ppLayout = xmobarColor "orange" "", ppTitle = xmobarColor "cyan" "" . shorten 80 } 

-- LayoutHook: certain workspaces get a specific layout or layout order.
myLayoutHook = onWorkspace "web" myTileFirst $
               onWorkspace "im" myChat $
               onWorkspace "steam" mySteam $
               myGridFirst
                 where
                   -- Tile First Layout
                   myTileFirst = avoidStruts (smartBorders (tiled ||| Grid) ||| noBorders Full)
                     where
                       tiled = Tall nmaster delta ratio
                       nmaster = 1
                       ratio = 1/2
                       delta = 3/100
                   -- Grid as First Layout
                   myGridFirst = avoidStruts (smartBorders (Grid ||| tiled) ||| noBorders Full)
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
   , "web", "im", "irc", "mail", "sfb880", "logo"
   , "adm", "steam", "tmp", "music", "work", "vpn"
   , "infam"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
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
       , ("infam",  "work/infam")
       ]
   , defaultTopicAction = const $ spawnShell
   , defaultTopic = "dashboard"
   , topicActions = M.fromList $
       [ ("im",         spawn "pidgin")
       , ("irc",        spawn "urxvt -e weechat-curses")
       , ("mail",       spawn "thunderbird")
       , ("web",        spawn "firefox")
       , ("steam",      spawn "steam")
       , ("music",      spawn "urxvt -e ncmpcpp")
       ]
   }

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

-- Xmonad starten
main = do
    -- Xmobar starten
    xmproc <- spawnPipe "/usr/local/bin/xmobar /home/phoenix/.xmobarrc"
    -- Xmonad starten
    xmonad $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = myLayoutHook 
        , startupHook = setWMName "LG3D"
        , workspaces = myTopics
        , logHook = myLogHook xmproc
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "urxvt"
        , borderWidth = 2
        , focusedBorderColor = "#FF0000"
        } `additionalKeys`
        -- Volume Control
        [ ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -- -1%")
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1%")
        , ((0, xF86XK_AudioMute), spawn "~/bin/pa_toggle.sh")
        -- Brightness Control
        , ((mod1Mask, xF86XK_AudioLowerVolume), spawn "xbacklight -10")
        , ((mod1Mask, xF86XK_AudioRaiseVolume), spawn "xbacklight +10")
        -- Brightness Hotkeys: 1 25 50 75 100
        , ((mod1Mask .|. mod4Mask, xK_1), spawn "xbacklight =1")
        , ((mod1Mask .|. mod4Mask, xK_2), spawn "xbacklight =25")
        , ((mod1Mask .|. mod4Mask, xK_3), spawn "xbacklight =50")
        , ((mod1Mask .|. mod4Mask, xK_4), spawn "xbacklight =75")
        , ((mod1Mask .|. mod4Mask, xK_5), spawn "xbacklight =100")
        -- Show and hide dock
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        -- show and hide border
        , ((mod4Mask, xK_n), withFocused toggleBorder)
        , ((mod4Mask .|. shiftMask, xK_Return), spawnShell)
        -- Firefox Hotkey
        , ((mod4Mask, xK_f), spawn "firefox")
        -- MPD Control External mpd Server Bibliothekar
        , ((mod4Mask, xF86XK_AudioPlay), spawn "mpc -h bibliothekar toggle")
        , ((mod4Mask, xF86XK_AudioPrev), spawn "mpc -h bibliothekar prev")
        , ((mod4Mask, xF86XK_AudioNext), spawn "mpc -h bibliothekar next")
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
        -- Select workspace from Grid
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        , ((mod4Mask, xK_o), spawnSelected defaultGSConfig ["smplayer","xbmc","firefox","thunderbird","pidgin","urxvt -e weechat-curses","libreoffice","wireshark"])
        ]
