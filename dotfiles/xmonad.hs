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
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import qualified Data.Map as M

myLayout = smartBorders $ avoidStruts $ minimize (mkToggle (NOBORDERS ?? FULL ?? EOT) (tiled ||| Mirror tiled ||| Full))
  where
    tiled   = gaps [(U,6), (R,6), (L,6), (R,6)] $ spacing 6 $ Tall nmaster delta ratio

	-- The default number of windows in the master pane
    nmaster = 1

	-- Default proportion of screen occupied by master pane
    ratio =  1/2

	-- Percent of screen to increment by when resizing panes
    delta   = 3/100

main :: IO ()
main = do
xmproc <- spawnPipe "xmobar" -- start xmobar
xmonad $ defaultConfig { terminal = "termite"
	, manageHook = manageDocks <+> manageHook defaultConfig
	, layoutHook = avoidStruts $ myLayout
	, logHook = dynamicLogWithPP xmobarPP
		{ ppOutput = hPutStrLn xmproc
		, ppLayout = (\_ -> "")
		, ppTitle = xmobarColor "white" "" . shorten 50
		}
	, normalBorderColor = "#333"
	, focusedBorderColor = "lime green"
	} `additionalKeysP` [ ("M-w", raiseBrowser)
						, ("M-e", raiseEditor)
						, ("M-p", spawn "rofi -show run")
						, ("M-f", (sendMessage $ Toggle FULL))
					 ]
