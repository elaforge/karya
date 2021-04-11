-- config for the weeder tool
{ roots =
    [ ".*\\.main"
    -- Called from REPL.
    , "Cmd\\.Repl\\.L.*"
    , "Cmd\\.Repl\\.Global\\..*"
    -- Variables starting with _ are either "commented out" or called from
    -- ghci.
    , "\\._[^.]+$"

    -- Frequently called from REPL.
    , "Cmd\\.Create\\..*"
    , "Cmd\\.Factor\\..*"
    , "Cmd\\.ModifyEvents\\..*"
    , "Cmd\\.ModifyNotes\\..*"
    , "Cmd\\.Perf\\..*"
    , "Cmd\\.Ruler\\.Meter\\..*"
    , "Cmd\\.Ruler\\.Meters\\..*"
    , "Cmd\\.Ruler\\.Tala\\..*"

    , "User\\..*" -- Config, only possibly linked in.

    -- called from ghci
    , "D\\..*"
    , "Ness\\..*"
    , "Solkattu\\.Db\\..*"

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
