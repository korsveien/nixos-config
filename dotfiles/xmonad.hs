import XMonad
import XMonad.Layout.Spacing
import System.IO

-- put a 2px space around every window
myLayout = spacing 15 $ Tall 1 (3/100) (1/2)

main :: IO ()
main = do xmonad $ defaultConfig { terminal = "termite"
			         -- , modMask = mod4Mask
				 , layoutHook = myLayout
}
