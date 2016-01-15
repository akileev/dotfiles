-------------------------------------------------------------------------------
-- Configuration for using xmonad inside xfce4.
--
-- Author: Johannes 'wulax' Sjölund
-- Based on the work of Øyvind 'Mr.Elendig' Heggstad
--
-- Last tested with xmonad 0.11 and xfce 4.12.
--
-- 1. Start xmonad by adding it to "Application Autostart" in xfce.
-- 2. Make sure xfwm4 is disabled from autostart, or uninstalled.
-- 3. Make sure xfdesktop is disabled from autostart, or uninstalled
--    since it may prevent xfce-panel from appearing once xmonad is started.
-------------------------------------------------------------------------------
 
import qualified Data.Map as M
 
import qualified XMonad.StackSet as W
import Control.Exception
import System.Exit
 
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import Data.Ratio ((%)) 
import XMonad.Util.SpawnOnce
 
conf = ewmh defaultConfig
        { manageHook        = pbManageHook <+> myManageHook
                                           <+> manageDocks
                                           <+> manageHook defaultConfig
        , layoutHook        = avoidStruts (myLayoutHook)
        , startupHook       = myStartHook
        , handleEventHook   = ewmhDesktopsEventHook <+> fullscreenEventHook
        , borderWidth       = 1
        , focusedBorderColor= "#444444"
        , normalBorderColor = "#444444"
        , workspaces        = myWorkspaces
        , modMask           = mod4Mask
        , keys              = myKeys
         }
    where
        tall                = ResizableTall 1 (3/100) (1/2) []
 
-- Main --
main :: IO ()
main =
    xmonad $ conf
        { startupHook       = startupHook conf
                            >> setWMName "LG3D" -- Java app focus fix
        , logHook           =  ewmhDesktopsLogHook
         }
 
-- Workspaces
myWorkspaces = 
    [
        "1:term", "2:dev", "3:virt", 
        "4:web", "5:soc", "6:media", 
        "7:other", "8", "9"
    ]
 
-- Tabs theme --
myTabTheme = defaultTheme
    { activeColor           = "white"
    , inactiveColor         = "grey"
    , urgentColor           = "red"
    , activeBorderColor     = "grey"
    , inactiveBorderColor   = "grey"
    , activeTextColor       = "black"
    , inactiveTextColor     = "black"
    , decoHeight            = 22
    , fontName              = "xft:Ubuntu Mono:size=12"
    }

myStartHook = ewmhDesktopsStartup <+>
              spawnOnce "bash -l ~/.xmonad/autostart.sh" <+>
              setWMName "LG3D"
 
 
-- Layouts --
myLayoutHook = onWorkspace "5:soc" pidginLayout $ tile ||| mtile ||| full ||| tab
  where
    rt      = ResizableTall 1 (2/100) (1/2) []
    -- Pidgin and Thunderbird
    gridLayout = Grid
    pidginLayout = withIM (20/100) (Role "buddy_list") gridLayout
    -- normal vertical tile
    tile    = named "[]="   $ smartBorders rt
    -- normal horizontal tile
    mtile   = named "M[]="  $ smartBorders $ Mirror rt
    -- fullscreen with tabs
    tab     = named "T"     $ noBorders $ tabbed shrinkText myTabTheme
    -- two tab panes beside each other, move windows with SwapWindow
    tabB    = noBorders     $ tabbed shrinkText myTabTheme
    -- fullscreen without tabs
    full        = named "[]"    $ noBorders Full
 
 
-- Default managers
--
-- Match a string against any one of a window's class, title, name or
-- role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]
 
-- Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_CLASS"
 
-- Match against @WM_WINDOW_ROLE@.
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"
 
-- ManageHook --
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks ]
    , [ manageHook defaultConfig ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    , [ fmap not isDialog --> doF avoidMaster ]
    ]
 
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions]
    where myActions =
            [ ("Xfrun4"                         , doFloat)
            , ("Xfce4-notifyd"                  , doIgnore)
            , ("MPlayer"                        , doFloat)
            , ("mpv"                            , doFloat)
            , ("jetbrains-idea"                 , doShift "2:dev")
            , ("jetbrains-idea-ce"              , doShift "2:dev")
            , ("sublime_text"                   , doShift "2:dev")
            , ("QtCreator"                      , doShift "2:dev")
            , ("Google-chrome-stable"           , doShift "4:web")
            , ("Chromium-browser"               , doShift "4:web")
            , ("chromium"                       , doShift "4:web")
            , ("Transmission-gtk"               , doShift "6:media")
            , ("Smplayer"                       , doShift "6:media")
            , ("smplayer"                       , doShift "6:media")
            , ("Audacious"                      , doShift "6:media")
            , ("Oracle VM VirtualBox Manager"   , doShift "3:virt")
            , ("VirtualBox"                     , doShift "3:virt")
            , ("Thunar"                         , doShift "7:other")
            , ("Thunderbird"                    , doShift "5:soc")
            , ("Pidgin"                         , doShift "5:soc")
            , ("Skype"                          , doShift "5:soc")
            , ("gimp-image-window"              , (ask >>= doF . W.sink))
            , ("gimp-toolbox"                   , (ask >>= doF . W.sink))
            , ("gimp-dock"                      , (ask >>= doF . W.sink))
            , ("gimp-image-new"                 , doFloat)
            , ("gimp-toolbox-color-dialog"      , doFloat)
            , ("gimp-layer-new"                 , doFloat)
            , ("gimp-vectors-edit"              , doFloat)
            , ("gimp-levels-tool"               , doFloat)
            , ("preferences"                    , doFloat)
            , ("gimp-keyboard-shortcuts-dialog" , doFloat)
            , ("gimp-modules"                   , doFloat)
            , ("unit-editor"                    , doFloat)
            , ("screenshot"                     , doFloat)
            , ("gimp-message-dialog"            , doFloat)
            , ("gimp-tip-of-the-day"            , doFloat)
            , ("plugin-browser"                 , doFloat)
            , ("procedure-browser"              , doFloat)
            , ("gimp-display-filters"           , doFloat)
            , ("gimp-color-selector"            , doFloat)
            , ("gimp-file-open-location"        , doFloat)
            , ("gimp-color-balance-tool"        , doFloat)
            , ("gimp-hue-saturation-tool"       , doFloat)
            , ("gimp-colorize-tool"             , doFloat)
            , ("gimp-brightness-contrast-tool"  , doFloat)
            , ("gimp-threshold-tool"            , doFloat)
            , ("gimp-curves-tool"               , doFloat)
            , ("gimp-posterize-tool"            , doFloat)
            , ("gimp-desaturate-tool"           , doFloat)
            , ("gimp-scale-tool"                , doFloat)
            , ("gimp-shear-tool"                , doFloat)
            , ("gimp-perspective-tool"          , doFloat)
            , ("gimp-rotate-tool"               , doFloat)
            , ("gimp-open-location"             , doFloat)
            , ("gimp-file-open"                 , doFloat)
            , ("animation-playbac"              , doFloat)
            , ("gimp-file-save"                 , doFloat)
            , ("file-jpeg"                      , doFloat)
            , ("Whisker"                        , doFloat)
            ]
 
-- Helpers --
-- avoidMaster:  Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c
 
 
-- Keyboard --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,                xK_Return   ), spawn "urxvt")
    , ((modMask,                xK_o        ), spawn "xfrun4")
    , ((modMask,                xK_d        ), spawn "bash -l $HOME/.xmonad/dmenu.sh")
    , ((modMask,                xK_f        ), spawn "thunar")
    , ((modMask,                xK_c        ), kill)
    , ((modMask,                xK_b        ), sendMessage ToggleStruts)
 
    -- layouts
    , ((modMask,                xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,  xK_space    ), setLayout $ XMonad.layoutHook conf)
 
    -- floating layer stuff
    , ((modMask,                xK_t        ), withFocused $ windows . W.sink)
 
    -- refresh
    , ((modMask,                xK_n        ), refresh)
 
    -- focus
    , ((modMask,                xK_Tab      ), windows W.focusDown)
    , ((modMask,                xK_j        ), windows W.focusDown)
    , ((modMask,                xK_k        ), windows W.focusUp)
    , ((modMask,                xK_m        ), windows W.focusMaster)
    , ((modMask,                xK_Right    ), nextWS)
    , ((modMask,                xK_Left     ), prevWS)
    , ((modMask .|. shiftMask,  xK_Right    ), shiftToNext >> nextWS)
    , ((modMask .|. shiftMask,  xK_Left     ), shiftToPrev >> prevWS)
 
    -- swapping
    , ((modMask .|. shiftMask,  xK_Return   ), windows W.swapMaster)
    , ((modMask .|. shiftMask,  xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask,  xK_k        ), windows W.swapUp)
    , ((modMask,                xK_s        ), sendMessage $ SwapWindow)
 
    -- increase or decrease number of windows in the master area
    , ((modMask,                xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                xK_period   ), sendMessage (IncMasterN (-1)))
 
    -- resizing
    , ((modMask,                xK_h        ), sendMessage Shrink)
    , ((modMask,                xK_l        ), sendMessage Expand)
    , ((modMask .|. shiftMask,  xK_h        ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask,  xK_l        ), sendMessage MirrorExpand)
 
    -- quit, or restart
    , ((modMask .|. shiftMask,  xK_q        ), spawn "xmonad --recompile; xmonad --restart")
    , ((modMask,                xK_q        ), spawn "~/.xmonad/session-dialog.sh")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
