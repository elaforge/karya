module Derive.Call.Lily_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ShowVal as ShowVal
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest


test_is_ly = do
    let not_ly = DeriveTest.extract ex . DeriveTest.linear_derive_tracks id
            where ex e = (DeriveTest.e_twelve e, DeriveTest.e_attributes e)
        is_ly = first (map ex) . LilypondTest.derive_linear True id
            where
            ex e = (Lilypond.event_pitch e,
                ShowVal.show_val (Lilypond.event_attributes e))

    let tracks =
            [ (">", [(1, 1, "+always")])
            , ("> | is-ly", [(0, 1, "+ly1"), (1, 1, "+ly2")])
            , ("> | not-ly", [(1, 1, "+no1"), (2, 1, "+no2")])
            ] ++ UiTest.regular_notes 4
    equal (not_ly tracks)
        ([("4a", "-"), ("4b", "+always+no1"), ("4c", "+no2"), ("4d", "-")], [])
    equal (is_ly tracks)
        ([("a'", "+ly1"), ("b'", "+always+ly2"), ("c'", "-"), ("d'", "-")], [])
