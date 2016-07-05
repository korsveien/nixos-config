import System.IO
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeysP)

-- put a 2px space around every window
myLayout = spacing 15 $ Tall 1 (3/100) (1/2)


-- key bindings

main :: IO ()
main = do xmonad $ defaultConfig { terminal = "termite"
	, layoutHook = avoidStruts $ myLayout
	} `additionalKeysP` [ ("M-w", raiseBrowser)
						, ("M-e", raiseEditor)]
