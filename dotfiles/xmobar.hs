Config { font = "xft:Hack:pixelsize=14:style=bold"
	   , alpha = 255
       , border = NoBorder
	   , bgColor = "#333"
	   , fgColor = "white"
	   , position = Top
	   , lowerOnStart = True
	   , allDesktops = False
	   , persistent = True
       , commands = [ Run Date "%d/%m  %H:%M" "date" 10
			        , Run StdinReader
				    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader%} { %date%"
       }

