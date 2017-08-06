-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that create code events for the lilypond backend.
module Derive.Call.Prelude.Lily (note_calls) where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import Global


note_calls :: Derive.CallMaps Derive.Note
note_calls = Make.call_maps
    [ ("8va", c_8va)
    , ("clef", c_clef)
    , ("dyn", c_dyn)
    , ("ly-!", c_reminder_accidental)
    , ("ly-(", add_code "ly-begin-slur"
        "Begin a slur. The normal slur transformer doesn't work in some cases,\
        \ for instance inside tuplets." Lily.SuffixFirst "(")
    , ("ly-)", add_code "ly-end-slur"
        "End a slur. The normal slur transformer doesn't work in some cases,\
        \ for instance inside tuplets." Lily.SuffixLast ")")
    , ("ly-[", add_code "ly-begin-beam"
        "Begin a beam. Override lilypond's automatic beaming."
        Lily.SuffixFirst "[")
    , ("ly-]", add_code "ly-end-beam"
        "End a beam. Override lilypond's automatic beaming."
        Lily.SuffixFirst "]")
        -- SuffixFirst because it's unlikely the beam should go over a barline.
    , ("ly-<", c_crescendo)
    , ("ly-<>", c_crescendo_diminuendo)
    , ("ly->", c_diminuendo)
    , ("ly-?", c_cautionary_accidental)
    , ("ly-^~", c_tie_direction "^")
    , ("ly-_~", c_tie_direction "_")
    , ("ly-key", c_ly_key)
    , ("ly-post", c_ly_post)
    , ("ly-pre", c_ly_pre)
    , ("ly-span", c_ly_span)
    , ("ly-sus", c_ly_sustain)
    , ("ly^", c_ly_text_above)
    , ("ly_", c_ly_text_below)
    , ("ly-", c_ly_articulation)
    , ("ly\\", c_ly_command)
    , ("meter", c_meter)
    , ("subdivision", c_subdivision)
    , ("movement", c_movement)
    , ("xstaff", c_xstaff)
    , ("xstaff-a", c_xstaff_around)
    ]
    <> Derive.call_maps
        [ ("if-ly", c_if_ly)
        ]
        [ ("ly-global", c_ly_global)
        , ("ly-track", c_ly_track)
        , ("not-ly-track", c_not_ly_track)
        , ("unless-ly", c_unless_ly)
        , ("when-ly", c_when_ly)
        ]

c_when_ly :: Derive.Transformer Derive.Note
c_when_ly = transformer "when-ly"
    "With no arguments, evaluate the deriver only when in lilypond mode.\
    \ Unlike `ly-track`, this doesn't evaluate subtracks, so you can use it to\
    \ emit an entirely different set of tracks.\n\
    \ With arguments, evaluate them as a transformer and apply it only\
    \ when in lilypond mode.  Otherwise, the deriver is unchanged."
    $ Sig.callt (Sig.many_vals "arg" "Call expression.") (when_ly False)

c_unless_ly :: Derive.Transformer Derive.Note
c_unless_ly = transformer "unless-ly"
    "The inverse of when-ly, evaluate the deriver or apply the args only when\
    \ not in lilypond mode."
    $ Sig.callt (Sig.many_vals "arg" "Call expression.") (when_ly True)

when_ly :: Bool -> [BaseTypes.Val] -> Derive.PassedArgs Score.Event
    -> Derive.NoteDeriver -> Derive.NoteDeriver
when_ly inverted vals args deriver = case vals of
    [] -> when_lily deriver mempty
    call : vals -> when_lily (apply args (to_sym call) vals deriver) deriver
    where
    to_sym = Expr.Symbol . BaseTypes.show_call_val
    when_lily = if inverted then flip Lily.when_lilypond else Lily.when_lilypond
    apply args sym vals deriver = do
        call <- Eval.get_transformer sym
        Eval.apply_transformer (Derive.passed_ctx args) call vals deriver

c_ly_global :: Derive.Transformer Derive.Note
c_ly_global = transformer "ly-global"
    ("Evaluate the deriver only when in lilypond mode, like `when-ly`, but\
    \ also set the " <> ShowVal.doc Constants.ly_global <> " instrument."
    ) $ Sig.call0t $ \_ deriver ->
        Lily.when_lilypond (Lily.global deriver) mempty

c_ly_track :: Derive.Transformer Derive.Note
c_ly_track = transformer "ly-track"
    "Evaluate the deriver only when in lilypond mode, otherwise ignore this\
    \ track but evaluate its subtracks. Apply this to a track\
    \ to omit lilypond-only articulations, or to apply different articulations\
    \ to lilypond and non-lilypond output. Only use it in the track title!"
    $ Sig.call0t $ \args deriver ->
        Lily.when_lilypond deriver $ Lily.derive_notes args

c_not_ly_track :: Derive.Transformer Derive.Note
c_not_ly_track = transformer "not-ly-track"
    "The inverse of `ly-track`, evaluate the track only when not in lilypond\
    \ mode. Only use it in the track title!"
    $ Sig.call0t $ \args deriver -> flip Lily.when_lilypond deriver $
        Lily.derive_notes args

c_if_ly :: Derive.Generator Derive.Note
c_if_ly = generator "if-ly"
    "Conditional for lilypond." $ Sig.call ((,)
    <$> required "is-ly" "Evaluated in lilypond mode."
    <*> required "not-ly" "Evaluated when not in lilypond mode."
    ) $ \(is_ly, not_ly) args -> Lily.when_lilypond
        (Eval.reapply_string (Args.context args)
            (BaseTypes.show_call_val is_ly))
        (Eval.reapply_string (Args.context args)
            (BaseTypes.show_call_val not_ly))

c_8va :: Make.Calls Derive.Note
c_8va = code0_pair_call "ottava" "Emit lilypond ottava mark.\
    \ If it has duration, end with `8va 0`."
    (Sig.defaulted "octave" 0 "Transpose this many octaves up or down.") $
    \oct -> return (ottava oct, ottava 0)
    where
    ottava :: Int -> Lily.Code
    ottava n = (Lily.Prefix, "\\ottava #" <> showt n)

c_xstaff :: Make.Calls Derive.Note
c_xstaff = code0_call "xstaff"
    "Emit lilypond to put the notes on a different staff."
    (required "staff" "Switch to this staff.") $ \staff ->
        return (Lily.Prefix, change staff)
    where
    change :: Direction -> Lily.Ly
    change staff = "\\change Staff = " <> Types.to_lily (ShowVal.show_val staff)

c_xstaff_around :: Make.Calls Derive.Note
c_xstaff_around = code0_around_call "xstaff-around"
    "Emit lilypond to put the notes on a different staff."
    (required "staff" "Switch to this staff.") $ \staff ->
        return ((Lily.Prefix, change staff),
            (Lily.Prefix, change (other staff)))
    where
    change :: Direction -> Lily.Ly
    change staff = "\\change Staff = " <> Types.to_lily (ShowVal.show_val staff)
    other Up = Down
    other Down = Up

data Direction = Up | Down deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal Direction where show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck Direction
instance Typecheck.TypecheckSymbol Direction

c_dyn :: Make.Calls Derive.Note
c_dyn = code0_call "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    (required "dynamic" "Should be `p`, `ff`, etc.")
    (return . (,) Lily.SuffixAll . ("\\"<>))

c_clef :: Make.Calls Derive.Note
c_clef = code0_call "clef" "Emit lilypond clef change."
    (required "clef" "Should be `bass`, `treble`, etc.")
    (return . (,) Lily.Prefix . ("\\clef "<>))

c_meter :: Make.Calls Derive.Note
c_meter = global_code0_call "meter"
    "Emit lilypond meter change. It will be interpreted as global no matter\
    \ where it is. Simultaneous different meters aren't supported yet, but\
    \ `subdivision` supports simultaneous different spellings."
    (required "meter" "Should be `4/4`, `6/8`, etc. An ambiguous meter like\
        \ `6/8` will default to 3+3, but you can explicitly set the\
        \ subdivision, e.g. `2+2+2/8`.") $
    \meter -> Derive.with_val Constants.v_meter (meter :: Text)

c_subdivision :: Make.Calls Derive.Note
c_subdivision = code0_pair_call "subdivision"
    "Emit a subdivision change. This is the same format as `meter`, but it\
    \ affects the subdivision for this instrument only, instead of setting\
    \ the global meter. This is useful when instruments are playing\
    \ cross-rhythms and should beam accordingly."
    (fromMaybe "" <$> required "meter" "Same as `meter` call.") $
    \meter -> return
        ( (Lily.SetEnviron Constants.v_subdivision, meter)
        , (Lily.SetEnviron Constants.v_subdivision, "")
        )

c_movement :: Make.Calls Derive.Note
c_movement = global_code0_call "movement"
    "Start a new movement with the given title."
    (required "title" "Title of this movement.") $
    \title -> Derive.with_val Constants.v_movement (title :: Text)

c_reminder_accidental :: Make.Calls Derive.Note
c_reminder_accidental = Make.environ_note Module.ly "ly-reminder-accidental"
    mempty "Force this note to display an accidental."
    Constants.v_ly_append_pitch ("!" :: Lily.Ly)

c_cautionary_accidental :: Make.Calls Derive.Note
c_cautionary_accidental = Make.environ_note Module.ly "ly-cautionary-accidental"
    mempty "Force this note to display a cautionary accidental."
    Constants.v_ly_append_pitch ("?" :: Lily.Ly)

c_tie_direction :: Lily.Ly -> Make.Calls Derive.Note
c_tie_direction code = Make.environ_note Module.ly "ly-tie-direction"
    mempty "Force the note's tie to go either up or down."
    Constants.v_ly_tie_direction code

-- I want it to either attach to the end of the first note transformed, or
-- be free-standing but suffix markup.
c_crescendo :: Make.Calls Derive.Note
c_crescendo = make_code_call "ly-crescendo"
    "Start a crescendo hairpin.  If it has non-zero duration, stop the\
    \ crescendo at the event's end, otherwise the crescendo will stop at the\
    \ next hairpin or dynamic marking." Sig.no_args $
    \_ () -> crescendo_diminuendo "\\<"

c_diminuendo :: Make.Calls Derive.Note
c_diminuendo = make_code_call "ly-diminuendo"
    "Start a diminuendo hairpin.  If it has non-zero duration, stop the\
    \ diminuendo at the event's end, otherwise the diminuendo will stop at the\
    \ next hairpin or dynamic marking." Sig.no_args $
    \_ () -> crescendo_diminuendo "\\>"

c_crescendo_diminuendo :: Make.Calls Derive.Note
c_crescendo_diminuendo = make_code_call "ly-crescendo-diminuendo"
    "Crescendo followed by diminuendo, on one note."
    Sig.no_args $ \_ () args ->
        Lily.code0 (Args.start args) (Lily.SuffixFirst, "\\espressivo")

crescendo_diminuendo :: Lily.Ly -> Derive.PassedArgs d -> Derive.NoteDeriver
crescendo_diminuendo hairpin args
    -- TODO or is a transformer, I think I should set transformer duration to 0
    | Args.end args > Args.start args = start <> end
    | otherwise = start
    where
    start = Lily.code0 (Args.start args) (Lily.SuffixFirst, hairpin)
    end = Lily.code0 (Args.end args) (Lily.SuffixFirst, "\\!")

c_ly_text_above :: Make.Calls Derive.Note
c_ly_text_above = code_call "ly-text-above" "Attach text above the note."
    (required "text" "Text to attach. Double quotes can be omitted.") $
    return . (,) Lily.SuffixFirst . ("^"<>) . lily_str

c_ly_text_below :: Make.Calls Derive.Note
c_ly_text_below = code_call "ly-text-below" "Attach text below the note."
    (required "text" "Text to attach. Double quotes can be omitted.") $
    (return . (,) Lily.SuffixFirst . ("_"<>) . lily_str)

c_ly_articulation :: Make.Calls Derive.Note
c_ly_articulation = code_call "ly-articulation"
    "Append a `-articulation` to a note."
    (required "text" "Code to attach. A `-` is prepended.") $
    (return . (,) Lily.SuffixFirst . ("-"<>))

c_ly_command :: Make.Calls Derive.Note
c_ly_command = code_call "ly-command"
    "Append a `\\command` to a note."
    (required "text" "Code to attach. A `\\` is prepended.") $
    (return . (,) Lily.SuffixFirst . ("\\"<>))

add_code :: Derive.CallName -> Doc.Doc -> Lily.CodePosition -> Lily.Ly
    -> Make.Calls Derive.Note
add_code name doc pos code = code_call name doc Sig.no_args $
    \() -> return (pos, code)

lily_str :: Text -> Lily.Ly
lily_str = Types.to_lily

c_ly_pre :: Make.Calls Derive.Note
c_ly_pre = code0_call "ly-pre"
    "Emit arbitrary lilypond code that will go before concurrent notes."
    (required "code" "A leading \\ will be prepended.") $
    \code -> return (Lily.Prefix, "\\" <> code)

c_ly_post :: Make.Calls Derive.Note
c_ly_post = code0_call "ly-post"
    "Emit arbitrary lilypond code that will go after concurrent notes."
    (required "code" "A leading \\ will be prepended.") $
    \code -> return (Lily.SuffixAll, "\\" <> code)

c_ly_key :: Make.Calls Derive.Note
c_ly_key = code0_call "ly-key"
    "Emit a key change. This only emits a lilypond key change, it doesn't\
    \ actually set the key. This means diatonic operations won't work as\
    \ expected. Also, you have to add it to every staff manually.\
    \ On the up side, it doesn't force a structural change like `=` does."
    (required "key" "You can use any of the keys from the Twelve scale.") $
    \key -> do
        key <- Derive.require_right id $ Process.parse_key key
        return (Lily.Prefix, Types.to_lily key)

c_ly_sustain :: Make.Calls Derive.Note
c_ly_sustain = code0_call "ly-sus" "Emit \\sustainOn and \\sustainOff markup."
    (required "state" "t for \\sustainOn, f for \\sustainOff,\
        \ ft for \\sustainOff\\sustainOn.") $
    \mode -> case mode of
        Off -> return (Lily.SuffixAll, "\\sustainOff")
        On -> return (Lily.SuffixAll, "\\sustainOn")
        OffOn -> return (Lily.SuffixAll, "\\sustainOff\\sustainOn")

data SustainMode = Off | On | OffOn deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal SustainMode where
    show_val m = case m of
        Off -> "f"
        On -> "t"
        OffOn -> "ft"
instance Typecheck.Typecheck SustainMode
instance Typecheck.TypecheckSymbol SustainMode

c_ly_span :: Make.Calls Derive.Note
c_ly_span = make_code_call "ly-span"
    "Emit a bit of text followed by a dashed line until the end of the event.\
    \ This is useful for things like `accel.` or `cresc.` If it has a\
    \ a zero duration, emit the start if the text is given, or the end if it's\
    \ not."
    (Sig.required "text" "Text.") $ \_ text -> ly_span text

ly_span :: Maybe Lily.Ly -> Derive.PassedArgs a -> Derive.NoteDeriver
ly_span maybe_text args
    | Args.duration args == 0 = case maybe_text of
        Just text -> start text
        Nothing -> end
    | otherwise = case maybe_text of
        Just text -> start text <> end
        Nothing -> Derive.throw "use zero dur to end a span"
    where
    start text = mconcat
        [ Lily.code0 (Args.start args) $ (,) Lily.Prefix $
            "\\override TextSpanner.bound-details.left.text = \\markup { "
            <> Types.to_lily text <> " }"
        , Lily.code0 (Args.start args) (Lily.SuffixFirst, "\\startTextSpan")
        ]
    end = Lily.code0 (Args.end args) (Lily.SuffixLast, "\\stopTextSpan")

-- * util

-- | Attach ly code to the first note in the transformed deriver.
code_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Lily.Code) -> Make.Calls Derive.Note
code_call name doc sig make_code = (gen, trans)
    where
    gen = generator name doc $ Sig.call sig $ \val args -> do
        code <- make_code val
        -- Code calls mostly apply code to a single note.  It would be
        -- convenient to derive Call.note, but then I'd have to invert, and
        -- since inversion and sub-events are incompatible I would then have to
        -- ignore sub-events.  That in turn would mean I couldn't split the
        -- code calls into a separate track, which is notationally convenient.
        --
        -- The price is that if I want to put the call on the note track,
        -- I have to append |, which is easy to forget.
        require_nonempty
            =<< Lily.first_note_code code args (Lily.derive_notes args)
    trans = transformer name doc $
        Sig.callt sig $ \val _args deriver ->
            flip Lily.when_lilypond deriver $ do
                code <- make_code val
                Lily.add_first code deriver

require_nonempty :: Stream.Stream Score.Event -> Derive.NoteDeriver
require_nonempty events
    | null (Stream.events_of events) =
        Derive.throw "this call expects sub-events but none were found"
    | otherwise = return events

-- | Emit a free-standing fragment of lilypond code, attached to the current
-- instrument.
code0_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Lily.Code) -> Make.Calls Derive.Note
code0_call name doc sig make_code =
    make_code_call name (doc <> code0_doc) sig $ \_ val args ->
        Lily.code0 (Args.start args) =<< make_code val

code0_around_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver (Lily.Code, Lily.Code))
    -> Make.Calls Derive.Note
code0_around_call name doc sig make_code = (gen, trans)
    where
    around_doc = code0_doc
        <> " The transformer will wrap each event in (start, end) pairs.\
        \ This way you can wrap all notes on a certain track with\
        \ complementary bits of lilypond code."
    gen = generator name (doc <> around_doc) $
        Sig.call sig $ \val args -> Lily.only_lilypond $ do
            (code1, _) <- make_code val
            Lily.code0 (Args.start args) code1 <> Lily.derive_notes args
    trans = transformer name (doc <> around_doc) $
        Sig.callt sig $ \val _args deriver ->
            Lily.when_lilypond (transform val deriver) deriver
    transform val deriver = do
        (code1, code2) <- make_code val
        Post.emap_asc_ (apply code1 code2) <$> deriver
    apply code1 code2 event =
        [ Lily.code0_event event start code1
        , event
        , Lily.code0_event event end code2
        ]
        where (start, end) = (Score.event_start event, Score.event_end event)

-- | Like 'code0_call', except that the call can emit 2 Codes.  The second
-- will be used at the end of the event if it has non-zero duration and is
-- a transformer.
code0_pair_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver (Lily.Code, Lily.Code))
    -> Make.Calls Derive.Note
code0_pair_call name doc sig make_code =
    make_code_call name (doc <> code0_doc) sig $ \is_transformer val args -> do
        (code1, code2) <- make_code val
        let (start, end) = Args.range args
        Lily.code0 start code1 <> if is_transformer || start == end
            then mempty else Lily.code0 end code2

code0_doc :: Doc.Doc
code0_doc = "\nThis either be placed in a separate track as a zero-dur\
    \ event, or it can be attached to an individual note as a transformer."

-- | Like 'code0_call', but the code uses the 'Constants.ly_global' instrument.
global_code0_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Make.Calls Derive.Note
global_code0_call name doc sig call =
    make_code_call name doc sig $ \_ val args ->
        Lily.global $ call val (Derive.place (Args.start args) 0 Call.note)

-- | Emit a free-standing fragment of lilypond code.
make_code_call :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (Bool -> a -> Derive.NoteArgs -> Derive.NoteDeriver)
    -- ^ First arg is True if this is a transformer call.
    -> Make.Calls Derive.Note
make_code_call name doc sig call = (gen, trans)
    where
    gen = generator name doc $ Sig.call sig $ \val args ->
        Lily.only_lilypond $ call False val args <> Lily.derive_notes args
    trans = transformer name doc $ Sig.callt sig $ \val args deriver ->
        Lily.when_lilypond (call True val args <> deriver) deriver

generator :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.GeneratorF d) -> Derive.Generator d
generator name = Derive.generator Module.ly name Tags.ly

transformer :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.TransformerF d) -> Derive.Transformer d
transformer name = Derive.transformer Module.ly name Tags.ly
