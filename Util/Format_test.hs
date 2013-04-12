module Util.Format_test where
import qualified Data.Text as Text

import qualified Util.Format as Format
import Util.Test


test_format = do
    let run width = Text.lines . Format.run (Just width)
    equal (run 75 $ do
            Format.write "hi there "
            Format.indented 2 (Format.write "next lines\nindented\nyay ")
            Format.write "but then\nthis is not\n")
        [ "hi there next lines"
        , "  indented"
        , "  yay but then"
        , "this is not"
        ]

    equal (run 15 $ Format.wrapped_words 4
            "hi these are some normal words for the wrapping")
        [ "hi these are"
        , "    some normal"
        , "    words for"
        , "    the"
        , "    wrapping"
        ]
    equal (run 15 $ Format.wrapped_words 4 "hi this-word-is-too-long to wrap")
        [ "hi"
        , "    this-word-is-too-long"
        , "    to wrap"
        ]
    equal (run 15 $ Format.write "hi" >> Format.wrapped_words 4 "ho ho")
        ["hiho ho"]
