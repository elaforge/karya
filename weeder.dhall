-- config for the weeder tool
{ roots =
    [ ".*\\.main"
    -- Called from REPL.
    , "Cmd\\.Repl\\.L.*"
    , "Cmd\\.Repl\\.Global\\..*"

    -- Frequently called from REPL.
    , "Cmd\\.Create\\..*"
    , "Cmd\\.Factor\\..*"
    , "Cmd\\.ModifyEvents\\..*"
    , "Cmd\\.ModifyNotes\\..*"
    , "Cmd\\.Perf\\..*"
    , "Cmd\\.Ruler\\.Meter\\..*"
    , "Cmd\\.Ruler\\.Meters\\..*"
    , "Cmd\\.Ruler\\.Tala\\..*"

    -- Config, only possibly linked in.
    , "User\\..*"

    -- Generated.
    , "Audio\\.SampleRateC.*"

    -- Libraries of definitions.
    , "Derive\\.Attrs\\..*"
    , "Cmd\\.Instrument\\.Drums\\..*"
    , "Cmd\\.Instrument\\.Mridangam\\..*"

    -- misc
    , "Cmd\\.Simple\\..*"

    -- util libraries
    , "Util\\.Styled\\..*"
    , "Util\\.Rect\\..*"
    , "Util\\.Then\\..*"
    , "Util\\.Debug\\..*"
    , "Ui\\.Color\\.*"

    ]
, type-class-roots = True
}
