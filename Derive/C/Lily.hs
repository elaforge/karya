-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that create code events for the lilypond backend.
module Derive.C.Lily (library) where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Process as Process
import qualified Perform.Lilypond.Types as Types

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.both
        [ ("8va", c_8va)
        , ("clef", c_clef)
        , ("dyn", c_dyn)
        , ("ly-!", attach0 "ly-reminder-accidental"
            "Force this note to display an accidental."
            (Ly.SetEnviron Constants.v_append_pitch, "!"))
        , ("ly-?", attach0 "ly-cautionary-accidental"
            "Force this note to display a cautionary accidental."
            (Ly.SetEnviron Constants.v_append_pitch, "?"))

        , ("ly-(", attach0 "ly-begin-slur"
            "Separately mark a lilypond slur, when `(` isn't cutting it."
            (Ly.append Constants.First, "("))
        , ("ly-)", attach0 "ly-end-slur"
            "Separately mark a lilypond slur, when `(` isn't cutting it."
            (Ly.append Constants.Last, ")"))
        , ("ly-[", attach0 "ly-begin-beam"
            "Begin a beam. Override lilypond's automatic beaming."
            (Ly.append Constants.First, "["))
        , ("ly-]", attach0 "ly-end-beam"
            "End a beam. Override lilypond's automatic beaming."
            (Ly.append Constants.First, "]"))
            -- Constants.First because it's unlikely the beam should go over
            -- a barline.
        , ("ly-<", c_hairpin "\\<")
        , ("ly->", c_hairpin "\\>")
        , ("ly-<>", emit0 "ly-crescendo-diminuendo"
            "Crescendo followed by diminuendo, on one note."
            (Ly.Position Constants.FreeAppend, "\\espressivo"))
        , ("ly-^~", c_tie_direction "^")
        , ("ly-_~", c_tie_direction "_")
        , ("ly-key", c_ly_key)
        , ("ly-post", c_ly_post)
        , ("ly-attach", c_ly_attach)
        , ("ly-emit", c_ly_emit)
        , ("ly-pre", c_ly_pre)
        , ("ly-span", c_ly_span)
        , ("ly-sus", c_ly_sustain)
        , ("ly-tr~", c_ly_tr_span)
        , ("ly^", c_ly_text "^")
        , ("ly_", c_ly_text "_")
        , ("ly-", c_ly_articulation)
        , ("tempo", c_tempo)
        , ("meter", c_meter)
        , ("subdivision", c_subdivision)
        , ("movement", c_movement)
        , ("xstaff", c_xstaff)
        , ("xstaff-a", c_xstaff_around)
        ]
    , Library.generators
        [ ("if-ly", c_if_ly)
        ]
    , Library.transformers
        [ ("ly-global", c_ly_global)
        , ("ly-track", c_ly_track)
        , ("not-ly-track", c_not_ly_track)
        , ("unless-ly", c_unless_ly)
        , ("when-ly", c_when_ly)
        ]
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

when_ly :: Bool -> [DeriveT.Val] -> Derive.PassedArgs Score.Event
    -> Derive.NoteDeriver -> Derive.NoteDeriver
when_ly inverted vals args deriver = case vals of
    [] -> when_lily deriver mempty
    call : vals -> when_lily (apply args (to_sym call) vals deriver) deriver
    where
    to_sym = Expr.Symbol . DeriveT.show_call_val
    when_lily = if inverted then flip Ly.when_lilypond else Ly.when_lilypond
    apply args sym vals deriver = do
        call <- Eval.get_transformer sym
        Eval.apply_transformer (Derive.passed_ctx args) call vals deriver

c_ly_global :: Derive.Transformer Derive.Note
c_ly_global = transformer "ly-global"
    ("Evaluate the deriver only when in lilypond mode, like `when-ly`, but\
    \ also set the " <> ShowVal.doc Constants.ly_global <> " instrument."
    ) $ Sig.call0t $ \_ deriver ->
        Ly.when_lilypond (Ly.global deriver) mempty

c_ly_track :: Derive.Transformer Derive.Note
c_ly_track = transformer "ly-track"
    "Evaluate the deriver only when in lilypond mode, otherwise ignore this\
    \ track but evaluate its subtracks. Apply this to a track\
    \ to omit lilypond-only articulations, or to apply different articulations\
    \ to lilypond and non-lilypond output. Only use it in the track title!"
    $ Sig.call0t $ \args deriver ->
        Ly.when_lilypond deriver $ Sub.derive_subs args

c_not_ly_track :: Derive.Transformer Derive.Note
c_not_ly_track = transformer "not-ly-track"
    "The inverse of `ly-track`, evaluate the track only when not in lilypond\
    \ mode. Only use it in the track title!"
    $ Sig.call0t $ \args deriver -> flip Ly.when_lilypond deriver $
        Sub.derive_subs args

c_if_ly :: Derive.Generator Derive.Note
c_if_ly = generator "if-ly"
    "Conditional for lilypond." $ Sig.call ((,)
    <$> Sig.required "is-ly" "Evaluated in lilypond mode."
    <*> Sig.required "not-ly" "Evaluated when not in lilypond mode."
    ) $ \(is_ly, not_ly) args -> Ly.when_lilypond
        (Eval.eval_quoted (Args.context args) is_ly)
        (Eval.eval_quoted (Args.context args) not_ly)

c_8va :: Library.Calls Derive.Note
c_8va = emit_pair "ottava" "Emit lilypond ottava mark.\
    \ If it has duration, end with `8va 0`."
    (Sig.defaulted "octave" 0 "Transpose this many octaves up or down.") $
    \oct -> (ottava oct, ottava 0)
    where
    ottava n =
        (Ly.Position Constants.FreePrepend, "\\ottava #" <> showt (n :: Int))

c_xstaff :: Library.Calls Derive.Note
c_xstaff = emit_start "xstaff"
    "Emit lilypond to put the notes on a different staff."
    (Sig.required "staff" "Switch to this staff.") $ \staff ->
        return (Ly.Position Constants.FreePrepend, change staff)
    where change staff = "\\change Staff = " <> lily_str (to_lily staff)

c_xstaff_around :: Library.Calls Derive.Note
c_xstaff_around = attach_wrap_notes "xstaff-around"
    "Emit lilypond to put the notes on a different staff."
    (Sig.required "staff" "Switch to this staff.") $ \staff -> return
        (change staff, change (other staff))
    where
    change staff = "\\change Staff = " <> lily_str (to_lily staff)
    other Call.Up = Call.Down
    other Call.Down = Call.Up

to_lily :: Call.UpDown -> Text
to_lily Call.Up = "up"
to_lily Call.Down = "down"

c_dyn :: Library.Calls Derive.Note
c_dyn = emit_start "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    (Sig.required "dynamic" "Should be `p`, `ff`, etc.")
    (return . (Ly.Position Constants.FreeAppend,) . ("\\"<>))

c_clef :: Library.Calls Derive.Note
c_clef = emit_start "clef" "Emit lilypond clef change."
    (Sig.required "clef" "Should be `bass`, `treble`, etc.")
    (return . (Ly.Position Constants.FreePrepend,) . ("\\clef "<>))

c_tempo :: Library.Calls Derive.Note
c_tempo = emit_start "tempo" "Emit tempo marking."
    (Sig.required "text" "4 = 120, etc.")
    (return . (Ly.Position Constants.FreePrepend,) . ("\\tempo "<>))

c_meter :: Library.Calls Derive.Note
c_meter = emit_global "meter"
    "Emit lilypond meter change. It will be interpreted as global no matter\
    \ where it is. Simultaneous different meters aren't supported yet, but\
    \ `subdivision` supports simultaneous different spellings."
    (Sig.required "meter" "Should be `4/4`, `6/8`, etc. An ambiguous meter like\
        \ `6/8` will default to 3+3, but you can explicitly set the\
        \ subdivision, e.g. `2+2+2/8`.") $
    \meter -> return (Ly.SetEnviron Constants.v_meter, meter)

c_subdivision :: Library.Calls Derive.Note
c_subdivision = emit_pair "subdivision"
    "Emit a subdivision change. This is the same format as `meter`, but it\
    \ affects the subdivision for this instrument only, instead of setting\
    \ the global meter. This is useful when instruments are playing\
    \ cross-rhythms and should beam accordingly."
    (fromMaybe "" <$> Sig.required "meter" "Same as `meter` call.") $
    \meter ->
        ( (Ly.SetEnviron Constants.v_subdivision, meter)
        , (Ly.SetEnviron Constants.v_subdivision, "")
        )

c_movement :: Library.Calls Derive.Note
c_movement = emit_global "movement"
    "Start a new movement with the given title."
    (Sig.required "title" "Title of this movement.") $
    \title -> return (Ly.SetEnviron Constants.v_movement, title)

c_tie_direction :: Ly.Ly -> Library.Calls Derive.Note
c_tie_direction code = attach0 "ly-tie-direction"
    "Force the note's tie to go either up or down."
    (Ly.SetEnviron Constants.v_tie_direction, code)

c_hairpin :: Ly.Ly -> Library.Calls Derive.Note
c_hairpin code = emit_pair "ly-hairpin"
    "Start a crescendo or diminuendo hairpin.  If it has non-zero duration,\
    \ stop at the event's end, otherwise it will stop at the\
    \ next hairpin or dynamic marking." Sig.no_args $
    \() ->
        ( (Ly.Position Constants.FreeAppend, code)
        , (Ly.Position Constants.FreeAppend, "\\!")
        )

c_ly_text :: Ly.Ly -> Library.Calls Derive.Note
c_ly_text dir = attach First "ly-text" "Attach text above or below the note."
    (Sig.required "text" "Text to attach.") $
    (Ly.append Constants.First,) . (dir<>) . lily_str

c_ly_articulation :: Library.Calls Derive.Note
c_ly_articulation = attach All "ly-articulation"
    "Append a `-articulation` to notes."
    (Sig.required "text" "Code to attach. A `-` is prepended.") $
    ((Ly.append Constants.First,) . ("-"<>))

c_ly_pre :: Library.Calls Derive.Note
c_ly_pre = emit_start "ly-pre"
    "Emit arbitrary lilypond code that will go before concurrent notes."
    (Sig.required "code" "A leading \\ will be prepended.") $
    \code -> return (Ly.Position Constants.FreePrepend, "\\" <> code)

c_ly_post :: Library.Calls Derive.Note
c_ly_post = emit_start "ly-post"
    "Emit arbitrary lilypond code that will go after concurrent notes."
    (Sig.required "code" "A leading \\ will be prepended.") $
    \code -> return (Ly.Position Constants.FreeAppend, "\\" <> code)

c_ly_emit :: Library.Calls Derive.Note
c_ly_emit = emit_start "ly-emit"
    "Emit a single fragment of freestanding lilypond code."
    ((,)
    <$> Sig.required "code" "Emit this code."
    <*> Sig.defaulted "pos" Constants.FreeAppend
        "Where to put it: 'Derive.Call.Ly.CodePosition'."
    ) $ \(code, pos) -> return (Ly.Position pos, code)

c_ly_attach :: Library.Calls Derive.Note
c_ly_attach = attach All "ly-attach"
    "Attach lilypond code to each transformed note."
    ((,)
    <$> Sig.required "code" "Attach this code."
    <*> Sig.defaulted "pos" (Ly.append Constants.Last)
        "Where to put it: 'Derive.Call.Ly.CodePosition'."
    ) $ \(code, pos) -> (pos, code)

c_ly_key :: Library.Calls Derive.Note
c_ly_key = emit_start "ly-key"
    "Emit a key change. This only emits a lilypond key change, it doesn't\
    \ actually set the key. This means diatonic operations won't work as\
    \ expected. Also, you have to add it to every staff manually.\
    \ On the up side, it doesn't force a structural change like `=` does."
    (Sig.required "key" "You can use any of the keys from the Twelve scale.") $
    \key -> do
        key <- Derive.require_right id $ Process.parse_key key
        return (Ly.Position Constants.FreePrepend, Types.to_lily key)

c_ly_sustain :: Library.Calls Derive.Note
c_ly_sustain = emit_start "ly-sus" "Emit \\sustainOn and \\sustainOff markup."
    (Sig.required "state" "t for \\sustainOn, f for \\sustainOff,\
        \ ft for \\sustainOff\\sustainOn.") $
    \mode -> return $ case mode of
        Off -> (Ly.Position Constants.FreePrepend, "\\sustainOff")
        On -> (Ly.Position Constants.FreePrepend, "\\sustainOn")
        OffOn -> (Ly.Position Constants.FreePrepend, "\\sustainOff\\sustainOn")

c_ly_tr_span :: Library.Calls Derive.Note
c_ly_tr_span = emit_pair "ly-tr-span"
    "Emit a \\startTrillSpan - \\stopTrillSpan pair."
    Sig.no_args $ \() ->
        ( (Ly.Position Constants.FreePrepend, "\\startTrillSpan")
        , (Ly.Position Constants.FreeAppend, "\\stopTrillSpan")
        )

data SustainMode = Off | On | OffOn deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal SustainMode where
    show_val m = case m of
        Off -> "f"
        On -> "t"
        OffOn -> "ft"
instance Typecheck.Typecheck SustainMode
instance Typecheck.TypecheckSymbol SustainMode

c_ly_span :: Library.Calls Derive.Note
c_ly_span = emit "ly-span"
    "Emit a bit of text followed by a dashed line until the end of the event.\
    \ This is useful for things like `accel.` or `cresc.` If it has a\
    \ a zero duration, emit the start if the text is given, or the end if it's\
    \ not."
    (Sig.required "text" "Text.") ly_span

-- TODO maybe this should use attach, so it can go on a single note and
-- automatically extend to the end.
ly_span :: Maybe Ly.Ly -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [(ScoreTime, Ly.FreeCode)]
ly_span maybe_text (start, end)
    | start == end = return $ case maybe_text of
        Just text -> start_code text
        Nothing -> end_code
    | otherwise = case maybe_text of
        Just text -> return $ start_code text ++ end_code
        Nothing -> Derive.throw "use zero dur to end a span"
    where
    start_code text =
        [ (start,) $ (Ly.Position Constants.FreePrepend,) $
            -- Lilypond likes to put it above, but for tempo and dynamic marks
            -- I think they should go below.
            "\\textSpannerDown\
            \ \\override TextSpanner.bound-details.left.text = \\markup { "
            <> Types.to_lily text <> " }"
        , (start, (Ly.Position Constants.FreeAppend, "\\startTextSpan"))
        ]
    end_code = [(end, (Ly.Position Constants.FreeAppend, "\\stopTextSpan"))]


-- * Attach

data AttachTo = First -- ^ attach code to only the first event
    | All -- ^ attach code to all the events
    deriving (Eq, Show)

-- | The attach family attaches lilypond code to existing notes.  This is
-- suitable for code which applies directly to a single note.
attach :: AttachTo -> Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Ly.Code) -> Library.Calls Derive.Note
attach to name doc sig get_code =
    transform_notes name doc sig $ \arg deriver -> add (get_code arg) deriver
    where
    add = case to of
        First -> Ly.add_first
        All -> Ly.add_all

-- | 'attach' with no arguments.
attach0 :: Derive.CallName -> Doc.Doc -> Ly.Code -> Library.Calls Derive.Note
attach0 name doc code =
    attach All name (doc <> "\nLilypond code: " <> Doc.Doc (pretty code))
        Sig.no_args (const code)

transform_notes :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Library.Calls Derive.Note
transform_notes name doc sig transform =
    Make.transform_notes Module.ly name Tags.ly doc sig $
        \arg deriver -> Ly.when_lilypond (transform arg deriver) deriver

-- * Emit

-- | The emit family creates 0 dur events that just carry lilypond code, and
-- are not real notes.  This is suitable for score level directives, such as
-- dynamics or tempo markings.  Technically in lilypond those are attached to
-- notes too, but you wouldn't want to put one on every single note in
-- a section.
emit :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [(ScoreTime, Ly.FreeCode)])
    -> Library.Calls Derive.Note
emit = emit_transform id False

emit_start :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Ly.FreeCode) -> Library.Calls Derive.Note
emit_start name doc sig get_code = emit_transform id True name doc sig $
    \val (start, _) -> (:[]) . (start,) <$> get_code val

-- | Like 'emit_start', but also set the instrument to 'Constants.ly_global'.
emit_global :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Ly.FreeCode) -> Library.Calls Derive.Note
emit_global name doc sig get_code = emit_transform Ly.global True name doc sig $
    \val (start, _) -> (:[]) . (start,) <$> get_code val

emit_transform :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Bool -- ^ if True, require that the generator have 0 duration
    -> Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> (ScoreTime, ScoreTime)
        -> Derive.Deriver [(ScoreTime, Ly.FreeCode)])
    -> Library.Calls Derive.Note
emit_transform transform assert_0dur name doc_ sig get_events =
    Library.Calls gen trans
    where
    gen = generator name doc $ Sig.call sig $ \val args -> Ly.only_lilypond $ do
        Sub.assert_no_subs args
        when (assert_0dur && Args.duration args /= 0) $
            Derive.throw $ "this emits a single bit of lilypond code,\
                \ so it should be either a transformer or a 0 dur generator: "
                <> pretty (Args.range args)
        make val args
    trans = transformer name doc $ Sig.callt sig $ \val args deriver ->
        Ly.when_lilypond (make val args <> deriver) deriver
    make val args = transform $ mconcatMap (uncurry Ly.code0)
        =<< get_events val (Args.range args)
    doc = doc_ <> "\n" <> emit_doc

emit0 :: Derive.CallName -> Doc.Doc -> Ly.FreeCode -> Library.Calls Derive.Note
emit0 name doc code =
    emit name (doc <> "\nLilypond code: " <> Doc.Doc (pretty code))
    Sig.no_args $ \() (start, _) -> return [(start, code)]

-- | Like 'emit_start', except that the call can emit 2 Codes.  The second
-- will be used at the end of the event if it has non-zero duration.
emit_pair :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> (Ly.FreeCode, Ly.FreeCode)) -> Library.Calls Derive.Note
emit_pair name doc sig get_code = emit name doc sig (\val -> return . get val)
    where
    get val (start, end)
        | start == end = [(start, pre)]
        | otherwise = [(start, pre), (end, post)]
        where (pre, post) = get_code val

-- | Wrap each individual note event in code.
attach_wrap_notes :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver (Ly.Ly, Ly.Ly)) -> Library.Calls Derive.Note
attach_wrap_notes name doc sig get_code = Library.Calls gen trans
    where
    around_doc = emit_doc
        <> " The transformer will wrap each event in (start, end) pairs.\
        \ This way you can wrap all notes on a certain track with\
        \ complementary bits of lilypond code."
    gen = generator name (doc <> around_doc) $
        Sig.call sig $ \val args -> Ly.only_lilypond $
            transform val $ Sub.derive_subs args
    trans = transformer name (doc <> around_doc) $
        Sig.callt sig $ \val _args deriver ->
            Ly.when_lilypond (transform val deriver) deriver
    transform val deriver = do
        (pre, post) <- get_code val
        Ly.add_all (Ly.prepend, pre) $
            Ly.add_all (Ly.append Constants.Last, post) deriver

emit_doc :: Doc.Doc
emit_doc = "\nThis either be placed in a separate track as a zero-dur\
    \ event, or it can be attached to an individual note as a transformer."

-- * util

lily_str :: Text -> Ly.Ly
lily_str = Types.to_lily

generator :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.GeneratorF d) -> Derive.Generator d
generator name = Derive.generator Module.ly name Tags.ly

transformer :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.TransformerF d) -> Derive.Transformer d
transformer name = Derive.transformer Module.ly name Tags.ly
