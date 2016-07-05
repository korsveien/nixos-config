import System.IO
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- put a 2px space around every window
myLayout = spacing 15 $ Tall 1 (3/100) (1/2)


-- key bindings

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
	} `additionalKeysP` [ ("M-w", raiseBrowser)
						, ("M-e", raiseEditor)]
