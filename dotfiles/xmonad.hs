import XMonad
import XMonad.Layout.Spacing
import XMonad.Actions.WindowGo
import System.IO
import XMonad.Util.EZConfig(additionalKeysP)

-- put a 2px space around every window
myLayout = spacing 15 $ Tall 1 (3/100) (1/2)


-- key bindings

main :: IO ()
main = do xmonad $ defaultConfig { terminal = "termite"
	, layoutHook = myLayout
	} `additionalKeysP` [ ("M-w", raiseBrowser)
						, ("M-e", raiseEditor)]
