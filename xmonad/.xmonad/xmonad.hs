{--
Xmonad Config
Programs needed:
rofi
i3lock
trayer
volumeicon-alsa
fonts-font-awesome
feg
fdpowermon
--}

import XMonad
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

{-- Settings --}
myModMask            = mod4Mask
myTerminal           = "alacritty"
myFont               = "xft:Ubuntu Mono:regular:size=14:antialias=true:hinting=true"
myWorkspaces         = ["home", "dev", "web1", "web2", "chat", "media", "mail", "vbox", "sys" ]
myBorderWidth        = 1
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#FFFFFF"
myClickJustFocuses   = False
myFocusFollowsMouse  = True
myLauncher           = "rofi -show run"
myScreenlock         = "i3lock -i ~/Dotfiles/assets/wallpapers/insanya-bg.png"
myXmobarrc           = "~/.xmobar/xmobarrc"

{-- Key bindings --}
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)       -- Launch a terminal
  , ((modm,               xK_e     ), spawn "emacsclient -c -a 'emacs' --eval '(dired nil)'") -- Launch emacsclient
  , ((modm,               xK_p     ), spawn myLauncher)                   -- Launch rofi
  , ((modm .|. shiftMask, xK_q     ), kill)                               -- Close focused window
  , ((modm .|. shiftMask, xK_0     ), spawn myScreenlock)                 -- Launch Screenlock
  , ((modm,               xK_space ), sendMessage NextLayout)             -- Rotate through the available layout algorithms
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to defs
  , ((modm,               xK_n     ), refresh)                            -- Resize viewed windows to the correct size
  , ((modm,               xK_Tab   ), windows W.focusDown)                -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)                -- Move focus to the next window
  , ((modm,               xK_k     ), windows W.focusUp  )                -- Move focus to the previous window
  , ((modm,               xK_m     ), windows W.focusMaster  )            -- Move focus to the master window
  , ((modm,               xK_Return), windows W.swapMaster)               -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )               -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )               -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_h     ), sendMessage Shrink)                 -- Shrink the master area
  , ((modm .|. shiftMask, xK_l     ), sendMessage Expand)                 -- Expand the master area
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)     -- Push window back into tiling
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))         -- Increment number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))      -- Deincrement numbers of windows in the master area
  , ((modm .|. shiftMask, xK_r     ), io (exitWith ExitSuccess))                    -- Quit xmonad
  , ((modm              , xK_r     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
  ]
  ++
  -- mod-[1..9], Switch to workspace N, mod-shift-[1..9], Move client to workspace N
  [
    ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

  {-- Layouts --}
myLayout = avoidStruts (Mirror tiled ||| tabbed shrinkText myTabTheme ||| tiled)
  where
    tiled   = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
    nmaster = 1                        -- The default number of windows in the master pane
    ratio   = 3/4                      -- Default proportion of screen occupied by master pane
    delta   = 3/100                    -- Percent of screen to increment by when resizing panes

myTabTheme = def {
  fontName            = myFont
  , activeColor         = "#00A524"
  , inactiveColor       = "#000000"
  , activeBorderColor   = "#00A524"
  , inactiveBorderColor = "#000000"
  , activeTextColor     = "#000000"
  , inactiveTextColor   = "#FFFFFF"
  }

{-- Window rules: To find the property name associated with a program -> xprop | grep WM_CLASS --}
myManageHook = composeAll
  [
    resource =? "Firefox"        --> doShift ( myWorkspaces !! 2)
  , resource =? "desktop_window" --> doFloat
  , resource =? "kdesktop"       --> doFloat
  ]

{-- Event handling --}
myEventHook = mempty

{-- Status bars and logging --}
myLogHook = return ()

{-- Startup actions hooks --}
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnce "feh --bg-center ~/Dotfiles/assets/wallpapers/insanya-bg.png &"
  spawnOnce "emacs --daemon &"
  spawnOnce "trayer --edge bottom --align right --widthtype request --expand true --transparent true --alpha 0 --tint 0x000000 --height 22 &"
  spawnOnce "volumeicon &"
  spawnOnce "nm-applet &"
  spawnOnce "fdpowermon &"
  setWMName "LG3D"

{-- Run XMonad --}
main :: IO()
main = do
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $ docks def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    startupHook        = myStartupHook,
    logHook            = myLogHook <+> dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc
        , ppCurrent = xmobarColor "#8884FF" "" . wrap "[" "]" -- Current workspace in xmobar
        , ppHidden  = xmobarColor "#F9AFFF" "" . wrap "(" ")" -- Hidden workspaces in xmobar
        , ppVisible = xmobarColor "#F9AFFF" ""                -- Visible but not current workspace
        , ppHiddenNoWindows = xmobarColor "#F9AFFF" ""        -- Hidden workspaces (no windows)
        , ppTitle = xmobarColor "#000000" ""                  -- Title of active window in xmobar
        , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
        , ppUrgent = xmobarColor "#0000FF" "" . wrap "!" "!"  -- Urgent workspace
        , ppLayout = xmobarColor "#B0ADFF" ""
        }
    }
