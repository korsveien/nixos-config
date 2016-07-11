Config { font = "xft:Hack:pixelsize=14:style=bold"
	   , alpha = 0
       , border = NoBorder
	   , bgColor = "#333"
	   , fgColor = "white"
	   , position = Top
	   , lowerOnStart = True
	   , allDesktops = False
	   , textOffset = 20
	   , persistent = True
       , commands = [ Run Date "%d %b  %H:%M" "date" 10
			        , Run StdinReader
					, Run Memory ["-t", "Mem: <usedratio>%"] 10
				    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "      } %StdinReader%  { %date%      "
       }

