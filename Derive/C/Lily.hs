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
        [ ("8va", c8va)
        , ("clef", cClef)
        , ("dyn", cDyn)
        , ("ly-!", attach0 "ly-reminder-accidental"
            "Force this note to display an accidental."
            (Ly.SetEnviron Constants.vAppendPitch, "!"))
        , ("ly-?", attach0 "ly-cautionary-accidental"
            "Force this note to display a cautionary accidental."
            (Ly.SetEnviron Constants.vAppendPitch, "?"))

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
        , ("ly-<", cHairpin "\\<")
        , ("ly->", cHairpin "\\>")
        , ("ly-<>", emit0 "ly-crescendo-diminuendo"
            "Crescendo followed by diminuendo, on one note."
            (Ly.Position Constants.FreeAppend, "\\espressivo"))
        , ("ly-^~", cTieDirection "^")
        , ("ly-_~", cTieDirection "_")
        , ("ly-key", cLyKey)
        , ("ly-post", cLyPost)
        , ("ly-attach", cLyAttach)
        , ("ly-emit", cLyEmit)
        , ("ly-pre", cLyPre)
        , ("ly-span", cLySpan)
        , ("ly-sus", cLySustain)
        , ("ly-tr~", cLyTrSpan)
        , ("ly^", cLyText "^")
        , ("ly_", cLyText "_")
        , ("ly-", cLyArticulation)
        , ("tempo", cTempo)
        , ("meter", cMeter)
        , ("subdivision", cSubdivision)
        , ("movement", cMovement)
        , ("xstaff", cXstaff)
        , ("xstaff-a", cXstaffAround)
        ]
    , Library.generators
        [ ("if-ly", cIfLy)
        ]
    , Library.transformers
        [ ("ly-global", cLyGlobal)
        , ("ly-track", cLyTrack)
        , ("not-ly-track", cNotLyTrack)
        , ("unless-ly", cUnlessLy)
        , ("when-ly", cWhenLy)
        ]
    ]

cWhenLy :: Derive.Transformer Derive.Note
cWhenLy = transformer "when-ly"
    "With no arguments, evaluate the deriver only when in lilypond mode.\
    \ Unlike `ly-track`, this doesn't evaluate subtracks, so you can use it to\
    \ emit an entirely different set of tracks.\n\
    \ With arguments, evaluate them as a transformer and apply it only\
    \ when in lilypond mode.  Otherwise, the deriver is unchanged."
    $ Sig.callt (Sig.manyVals "arg" "Call expression.") (whenLy False)

cUnlessLy :: Derive.Transformer Derive.Note
cUnlessLy = transformer "unless-ly"
    "The inverse of when-ly, evaluate the deriver or apply the args only when\
    \ not in lilypond mode."
    $ Sig.callt (Sig.manyVals "arg" "Call expression.") (whenLy True)

whenLy :: Bool -> [DeriveT.Val] -> Derive.PassedArgs Score.Event
    -> Derive.NoteDeriver -> Derive.NoteDeriver
whenLy inverted vals args deriver = case vals of
    [] -> whenLily deriver mempty
    call : vals -> whenLily (apply args (toSym call) vals deriver) deriver
    where
    toSym = Expr.Symbol . DeriveT.showCallVal
    whenLily = if inverted then flip Ly.whenLilypond else Ly.whenLilypond
    apply args sym vals deriver = do
        call <- Eval.getTransformer sym
        Eval.applyTransformer (Derive.passedCtx args) call vals deriver

cLyGlobal :: Derive.Transformer Derive.Note
cLyGlobal = transformer "ly-global"
    ("Evaluate the deriver only when in lilypond mode, like `when-ly`, but\
    \ also set the " <> ShowVal.doc Constants.lyGlobal <> " instrument."
    ) $ Sig.call0t $ \_ deriver ->
        Ly.whenLilypond (Ly.global deriver) mempty

cLyTrack :: Derive.Transformer Derive.Note
cLyTrack = transformer "ly-track"
    "Evaluate the deriver only when in lilypond mode, otherwise ignore this\
    \ track but evaluate its subtracks. Apply this to a track\
    \ to omit lilypond-only articulations, or to apply different articulations\
    \ to lilypond and non-lilypond output. Only use it in the track title!"
    $ Sig.call0t $ \args deriver ->
        Ly.whenLilypond deriver $ Sub.deriveSubs args

cNotLyTrack :: Derive.Transformer Derive.Note
cNotLyTrack = transformer "not-ly-track"
    "The inverse of `ly-track`, evaluate the track only when not in lilypond\
    \ mode. Only use it in the track title!"
    $ Sig.call0t $ \args deriver -> flip Ly.whenLilypond deriver $
        Sub.deriveSubs args

cIfLy :: Derive.Generator Derive.Note
cIfLy = generator "if-ly"
    "Conditional for lilypond." $ Sig.call ((,)
    <$> Sig.required "is-ly" "Evaluated in lilypond mode."
    <*> Sig.required "not-ly" "Evaluated when not in lilypond mode."
    ) $ \(isLy, notLy) args -> Ly.whenLilypond
        (Eval.evalQuoted (Args.context args) isLy)
        (Eval.evalQuoted (Args.context args) notLy)

c8va :: Library.Calls Derive.Note
c8va = emitPair "ottava" "Emit lilypond ottava mark.\
    \ If it has duration, end with `8va 0`."
    (Sig.defaulted "octave" (0 :: Double)
        "Transpose this many octaves up or down."
    ) $
    \oct -> (ottava oct, ottava 0)
    where
    ottava n =
        (Ly.Position Constants.FreePrepend, "\\ottava #" <> showt (n :: Int))

cXstaff :: Library.Calls Derive.Note
cXstaff = emitStart "xstaff"
    "Emit lilypond to put the notes on a different staff."
    (Sig.required "staff" "Switch to this staff.") $ \staff ->
        pure (Ly.Position Constants.FreePrepend, change staff)
    where change staff = "\\change Staff = " <> lilyStr (toLily staff)

cXstaffAround :: Library.Calls Derive.Note
cXstaffAround = attachWrapNotes "xstaff-around"
    "Emit lilypond to put the notes on a different staff."
    (Sig.required "staff" "Switch to this staff.") $ \staff -> pure
        (change staff, change (other staff))
    where
    change staff = "\\change Staff = " <> lilyStr (toLily staff)
    other Call.Up = Call.Down
    other Call.Down = Call.Up

toLily :: Call.UpDown -> Text
toLily Call.Up = "up"
toLily Call.Down = "down"

cDyn :: Library.Calls Derive.Note
cDyn = emitStart "dyn"
    "Emit a lilypond dynamic. If there are notes below, they are derived\
    \ unchanged."
    (Sig.required "dynamic" "Should be `p`, `ff`, etc.")
    (pure . (Ly.Position Constants.FreeAppend,) . ("\\"<>))

cClef :: Library.Calls Derive.Note
cClef = emitStart "clef" "Emit lilypond clef change."
    (Sig.required "clef" "Should be `bass`, `treble`, etc.")
    (pure . (Ly.Position Constants.FreePrepend,) . ("\\clef "<>))

cTempo :: Library.Calls Derive.Note
cTempo = emitStart "tempo" "Emit tempo marking."
    (Sig.required "text" "4 = 120, etc.")
    (pure . (Ly.Position Constants.FreePrepend,) . ("\\tempo "<>))

cMeter :: Library.Calls Derive.Note
cMeter = emitGlobal "meter"
    "Emit lilypond meter change. It will be interpreted as global no matter\
    \ where it is. Simultaneous different meters aren't supported yet, but\
    \ `subdivision` supports simultaneous different spellings."
    (Sig.required "meter" "Should be `4/4`, `6/8`, etc. An ambiguous meter like\
        \ `6/8` will default to 3+3, but you can explicitly set the\
        \ subdivision, e.g. `2+2+2/8`.") $
    \meter -> pure (Ly.SetEnviron Constants.vMeter, meter)

cSubdivision :: Library.Calls Derive.Note
cSubdivision = emitPair "subdivision"
    "Emit a subdivision change. This is the same format as `meter`, but it\
    \ affects the subdivision for this instrument only, instead of setting\
    \ the global meter. This is useful when instruments are playing\
    \ cross-rhythms and should beam accordingly."
    (fromMaybe "" <$> Sig.required "meter" "Same as `meter` call.") $
    \meter ->
        ( (Ly.SetEnviron Constants.vSubdivision, meter)
        , (Ly.SetEnviron Constants.vSubdivision, "")
        )

cMovement :: Library.Calls Derive.Note
cMovement = emitGlobal "movement"
    "Start a new movement with the given title."
    (Sig.required "title" "Title of this movement.") $
    \title -> pure (Ly.SetEnviron Constants.vMovement, title)

cTieDirection :: Ly.Ly -> Library.Calls Derive.Note
cTieDirection code = attach0 "ly-tie-direction"
    "Force the note's tie to go either up or down."
    (Ly.SetEnviron Constants.vTieDirection, code)

cHairpin :: Ly.Ly -> Library.Calls Derive.Note
cHairpin code = emitPair "ly-hairpin"
    "Start a crescendo or diminuendo hairpin.  If it has non-zero duration,\
    \ stop at the event's end, otherwise it will stop at the\
    \ next hairpin or dynamic marking." Sig.noArgs $
    \() ->
        ( (Ly.Position Constants.FreeAppend, code)
        , (Ly.Position Constants.FreeAppend, "\\!")
        )

cLyText :: Ly.Ly -> Library.Calls Derive.Note
cLyText dir = attach First "ly-text" "Attach text above or below the note."
    (Sig.required "text" "Text to attach.") $
    (Ly.append Constants.First,) . (dir<>) . lilyStr

cLyArticulation :: Library.Calls Derive.Note
cLyArticulation = attach All "ly-articulation"
    "Append a `-articulation` to notes."
    (Sig.required "text" "Code to attach. A `-` is prepended.") $
    ((Ly.append Constants.First,) . ("-"<>))

cLyPre :: Library.Calls Derive.Note
cLyPre = emitStart "ly-pre"
    "Emit arbitrary lilypond code that will go before concurrent notes."
    (Sig.required "code" "A leading \\ will be prepended.") $
    \code -> pure (Ly.Position Constants.FreePrepend, "\\" <> code)

cLyPost :: Library.Calls Derive.Note
cLyPost = emitStart "ly-post"
    "Emit arbitrary lilypond code that will go after concurrent notes."
    (Sig.required "code" "A leading \\ will be prepended.") $
    \code -> pure (Ly.Position Constants.FreeAppend, "\\" <> code)

cLyEmit :: Library.Calls Derive.Note
cLyEmit = emitStart "ly-emit"
    "Emit a single fragment of freestanding lilypond code."
    ((,)
    <$> Sig.required "code" "Emit this code."
    <*> Sig.defaulted "pos" Constants.FreeAppend
        "Where to put it: 'Derive.Call.Ly.CodePosition'."
    ) $ \(code, pos) -> pure (Ly.Position pos, code)

cLyAttach :: Library.Calls Derive.Note
cLyAttach = attach All "ly-attach"
    "Attach lilypond code to each transformed note."
    ((,)
    <$> Sig.required "code" "Attach this code."
    <*> Sig.defaulted "pos" (Ly.append Constants.Last)
        "Where to put it: 'Derive.Call.Ly.CodePosition'."
    ) $ \(code, pos) -> (pos, code)

cLyKey :: Library.Calls Derive.Note
cLyKey = emitStart "ly-key"
    "Emit a key change. This only emits a lilypond key change, it doesn't\
    \ actually set the key. This means diatonic operations won't work as\
    \ expected. Also, you have to add it to every staff manually.\
    \ On the up side, it doesn't force a structural change like `=` does."
    (Sig.required "key" "You can use any of the keys from the Twelve scale.") $
    \key -> do
        key <- Derive.requireRight id $ Process.parseKey key
        pure (Ly.Position Constants.FreePrepend, Types.toLily key)

cLySustain :: Library.Calls Derive.Note
cLySustain = emitStart "ly-sus" "Emit \\sustainOn and \\sustainOff markup."
    (Sig.required "state" "t for \\sustainOn, f for \\sustainOff,\
        \ ft for \\sustainOff\\sustainOn.") $
    \mode -> pure $ case mode of
        Off -> (Ly.Position Constants.FreePrepend, "\\sustainOff")
        On -> (Ly.Position Constants.FreePrepend, "\\sustainOn")
        OffOn -> (Ly.Position Constants.FreePrepend, "\\sustainOff\\sustainOn")

cLyTrSpan :: Library.Calls Derive.Note
cLyTrSpan = emitPair "ly-tr-span"
    "Emit a \\startTrillSpan - \\stopTrillSpan pair."
    Sig.noArgs $ \() ->
        ( (Ly.Position Constants.FreePrepend, "\\startTrillSpan")
        , (Ly.Position Constants.FreeAppend, "\\stopTrillSpan")
        )

data SustainMode = Off | On | OffOn deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal SustainMode where
    showVal m = case m of
        Off -> "f"
        On -> "t"
        OffOn -> "ft"
instance Typecheck.Typecheck SustainMode

cLySpan :: Library.Calls Derive.Note
cLySpan = emit "ly-span"
    "Emit a bit of text followed by a dashed line until the end of the event.\
    \ This is useful for things like `accel.` or `cresc.` If it has a\
    \ a zero duration, emit the start if the text is given, or the end if it's\
    \ not."
    (Sig.required "text" "Text.") lySpan

-- TODO maybe this should use attach, so it can go on a single note and
-- automatically extend to the end.
lySpan :: Maybe Ly.Ly -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [(ScoreTime, Ly.FreeCode)]
lySpan maybeText (start, end)
    | start == end = pure $ case maybeText of
        Just text -> startCode text
        Nothing -> endCode
    | otherwise = case maybeText of
        Just text -> pure $ startCode text ++ endCode
        Nothing -> Derive.throw "use zero dur to end a span"
    where
    startCode text =
        [ (start,) $ (Ly.Position Constants.FreePrepend,) $
            -- Lilypond likes to put it above, but for tempo and dynamic marks
            -- I think they should go below.
            "\\textSpannerDown\
            \ \\override TextSpanner.bound-details.left.text = \\markup { "
            <> Types.toLily text <> " }"
        , (start, (Ly.Position Constants.FreeAppend, "\\startTextSpan"))
        ]
    endCode = [(end, (Ly.Position Constants.FreeAppend, "\\stopTextSpan"))]


-- * Attach

data AttachTo = First -- ^ attach code to only the first event
    | All -- ^ attach code to all the events
    deriving (Eq, Show)

-- | The attach family attaches lilypond code to existing notes.  This is
-- suitable for code which applies directly to a single note.
attach :: AttachTo -> Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Ly.Code) -> Library.Calls Derive.Note
attach to name doc sig getCode =
    transformNotes name doc sig $ \arg deriver -> add (getCode arg) deriver
    where
    add = case to of
        First -> Ly.addFirst
        All -> Ly.addAll

-- | 'attach' with no arguments.
attach0 :: Derive.CallName -> Doc.Doc -> Ly.Code -> Library.Calls Derive.Note
attach0 name doc code =
    attach All name (doc <> "\nLilypond code: " <> Doc.Doc (pretty code))
        Sig.noArgs (const code)

transformNotes :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Library.Calls Derive.Note
transformNotes name doc sig transform =
    Make.transformNotes Module.ly name Tags.ly doc sig $
        \arg deriver -> Ly.whenLilypond (transform arg deriver) deriver

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
emit = emitTransform id False

emitStart :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Ly.FreeCode) -> Library.Calls Derive.Note
emitStart name doc sig getCode = emitTransform id True name doc sig $
    \val (start, _) -> (:[]) . (start,) <$> getCode val

-- | Like 'emit_start', but also set the instrument to 'Constants.ly_global'.
emitGlobal :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver Ly.FreeCode) -> Library.Calls Derive.Note
emitGlobal name doc sig getCode = emitTransform Ly.global True name doc sig $
    \val (start, _) -> (:[]) . (start,) <$> getCode val

emitTransform :: (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Bool -- ^ if True, require that the generator have 0 duration
    -> Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> (ScoreTime, ScoreTime)
        -> Derive.Deriver [(ScoreTime, Ly.FreeCode)])
    -> Library.Calls Derive.Note
emitTransform transform assert0dur name doc_ sig getEvents =
    Library.Calls gen trans
    where
    gen = generator name doc $ Sig.call sig $ \val args -> Ly.onlyLilypond $ do
        Sub.assertNoSubs args
        when (assert0dur && Args.duration args /= 0) $
            Derive.throw $ "this emits a single bit of lilypond code,\
                \ so it should be either a transformer or a 0 dur generator: "
                <> pretty (Args.range args)
        make val args
    trans = transformer name doc $ Sig.callt sig $ \val args deriver ->
        Ly.whenLilypond (make val args <> deriver) deriver
    make val args = transform $ mconcatMap (uncurry Ly.code0)
        =<< getEvents val (Args.range args)
    doc = doc_ <> "\n" <> emitDoc

emit0 :: Derive.CallName -> Doc.Doc -> Ly.FreeCode -> Library.Calls Derive.Note
emit0 name doc code =
    emit name (doc <> "\nLilypond code: " <> Doc.Doc (pretty code))
    Sig.noArgs $ \() (start, _) -> pure [(start, code)]

-- | Like 'emit_start', except that the call can emit 2 Codes.  The second
-- will be used at the end of the event if it has non-zero duration.
emitPair :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> (Ly.FreeCode, Ly.FreeCode)) -> Library.Calls Derive.Note
emitPair name doc sig getCode = emit name doc sig (\val -> pure . get val)
    where
    get val (start, end)
        | start == end = [(start, pre)]
        | otherwise = [(start, pre), (end, post)]
        where (pre, post) = getCode val

-- | Wrap each individual note event in code.
attachWrapNotes :: Derive.CallName -> Doc.Doc -> Sig.Parser a
    -> (a -> Derive.Deriver (Ly.Ly, Ly.Ly)) -> Library.Calls Derive.Note
attachWrapNotes name doc sig getCode = Library.Calls gen trans
    where
    aroundDoc = emitDoc
        <> " The transformer will wrap each event in (start, end) pairs.\
        \ This way you can wrap all notes on a certain track with\
        \ complementary bits of lilypond code."
    gen = generator name (doc <> aroundDoc) $
        Sig.call sig $ \val args -> Ly.onlyLilypond $
            transform val $ Sub.deriveSubs args
    trans = transformer name (doc <> aroundDoc) $
        Sig.callt sig $ \val _args deriver ->
            Ly.whenLilypond (transform val deriver) deriver
    transform val deriver = do
        (pre, post) <- getCode val
        Ly.addAll (Ly.prepend, pre) $
            Ly.addAll (Ly.append Constants.Last, post) deriver

emitDoc :: Doc.Doc
emitDoc = "\nThis either be placed in a separate track as a zero-dur\
    \ event, or it can be attached to an individual note as a transformer."

-- * util

lilyStr :: Text -> Ly.Ly
lilyStr = Types.toLily

generator :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.GeneratorF d) -> Derive.Generator d
generator name = Derive.generator Module.ly name Tags.ly

transformer :: Derive.CallName -> Doc.Doc
    -> Derive.WithArgDoc (Derive.TransformerF d) -> Derive.Transformer d
transformer name = Derive.transformer Module.ly name Tags.ly
