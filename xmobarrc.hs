Config { font = "xft:Hasklig:size=9"
        , borderColor = "black"
        , position = Bottom
        , bgColor = "#222222"
        , fgColor = "grey"
        , commands = [ Run Date "%a, %b %d %Y | %T" "date" 10, Run UnsafeStdinReader ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = " %UnsafeStdinReader% }{<fc=grey>%date%</fc> "
        }
