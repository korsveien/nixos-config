import System.IO
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.SetWMName
import XMonad.Layout.ThreeColumns

import Data.Maybe
import Graphics.X11.ExtraTypes
import System.IO
import System.Exit
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Inspirations:
-- https://github.com/ruhatch/.dotfiles/blob/master/.xmonad/xmonad.hs
------------------------------------------------------------------------

-----------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that

myManageHook = composeAll [
    -- className =? "Firefox"          --> doShift (myWorkspaces !! 0),
    -- className =? "Spotify"          --> doShift (myWorkspaces !! 10),
    resource  =? "desktop_window"   --> doIgnore,
    isFullscreen --> (doF W.focusDown <+> doFullFloat)]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
    noBorders (fullscreenFull Full) |||
    (lessBorders OnlyFloat $ avoidStruts $ (
    spacing 16 $
    gaps [(U,16), (D,16), (R,16), (L,16)] $
    Tall 1 (3/100) (1/2) ) |||
    ThreeCol 1 (3/100) (1/3) -- |||
-- Mirror (Tall 1 (3/100) (1/2))) -- |||
--  tabbed shrinkText tabConfig |||
--  Full -- |||
--  spiral (6/7)
    )


myTerminal = "termite"

myLauncher = "rofi -show run"

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#Af745f"

-- Width of the window border in pixels.
myBorderWidth = 0

myNormalBorderColor = "#333"

myFocusedBorderColor = "tomato"


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> kill))
-- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook =
    spawn "feh --bg-scale $HOME/nixos-config/background.png"
    <+> setDefaultCursor xC_left_ptr
    <+> spawn "compton -b"

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	----------------------------------------------------------------------
	-- Custom key bindings
	--

	-- Spawn the launcher using command specified by myLauncher.
	-- Use this to launch programs without a key binding.
	[ ((modMask, xK_space),
		spawn myLauncher)

	-- Start a terminal
	, ((modMask, xK_Return),
	    spawn $ XMonad.terminal conf)

    -- Reset the layouts on the current workspace to default
	, ((modMask .|. shiftMask, xK_space),
		setLayout $ XMonad.layoutHook conf)

    -- Close focused window
	, ((modMask .|. shiftMask, xK_w),
		kill)

	-- Cycle through the available layout algorithms
	, ((modMask, xK_p),
	   sendMessage NextLayout)

	-- 

	-- Move focus to the next window
	, ((modMask, xK_Tab),
		windows W.focusDown)

	-- Move focus to the next window
	, ((modMask, xK_j),
		windows W.focusDown)

	-- Move focus to the previous window
	, ((modMask, xK_k),
		windows W.focusUp)

	-- Move focus to the master window
	, ((modMask, xK_m),
		windows W.focusMaster)

    -- Swap the focused window and the master window
	, ((modMask .|. shiftMask, xK_Return),
		windows W.swapMaster)

	-- Start the app launcher
	, ((modMask, xK_space),
		spawn myLauncher)

	-- Push the window back into tiling
	, ((modMask, xK_t),
		withFocused $ windows . W.sink)

	-- Restart Xmonad
	, ((modMask , xK_q),
	  restart "xmonad" True)
	]

	++
	
	-- mod-[1..9] Switch to workspace N
	-- mod-shift-[1..9], Move client to workspace N
	[((m .|. modMask, k), windows $ f i)
		| (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
		, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

 ------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
    xmproc <- spawnPipe "xmobar -d $HOME/.xmonad/xmobar.hs"
    xmonad $ defaults {
        logHook = do
            fadeInactiveLogHook 0.95
            dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = const "",
            ppLayout = const "",
            ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "",
            ppSep = "   "
      }
			, manageHook = manageDocks <+> myManageHook
			, startupHook = setWMName "LG3D" <+> myStartupHook
  }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    startupHook        = myStartupHook,
    handleEventHook    = E.fullscreenEventHook
}

