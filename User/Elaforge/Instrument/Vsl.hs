-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Vienna Symphonic Library.
module User.Elaforge.Instrument.Vsl where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Texts as Texts

import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Articulation as Articulation
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Tag as Tag

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified User.Elaforge.Instrument.VslInst as VslInst

import           Global
import           Types


synth :: MidiInst.Synth
synth = MidiInst.synth "vsl" "Vienna Symphonic Library" $
    MidiInst.synthControls controls patches
    where
    controls =
        [ (11, "expression") -- apparently like cc7 volume?
        , (22, "attack") -- attack time
        , (23, "release") -- release time
        , (24, "filter") -- low pass filter
        , (20, "slot-xf") -- xfade between a and b slot positions
        , (2,  "velocity-xf") -- xfade between vel layers, set breath dyn
        , (28, "velocity-xf-on") -- velocity xfade on/off
        , (29, "rsamp-on") -- release samples on/off
        , (25, "delay") -- scale per patch delay
        , (26, "tuning") -- scale out of tune curve
        , (27, "humanize") -- scale both 'delay' and 'tuning'
        , (30, "dyn-range") -- scale effect of velocity
        , (21, "start-scaler") -- scale start offset
        , (14, "reverb") -- reverb wet
        , (15, "reverb-on") -- reverb on/off
        ]

-- * util

-- | For interactive use, find keyswitches with the given attributes.
findAttrs :: InstT.Name -> String -> [Text]
findAttrs inst withAttrs =
    map ShowVal.showVal $ filter (`Attrs.contain` search)
        (patchAttributes (MidiInst.patchPatch patch))
    where
    search = either (error . untxt) id (Parse.parseAttrs withAttrs)
    patch = fromMaybe (error $ "patch not found: " ++ show inst) $
        List.find ((==inst) . Patch.patchName . MidiInst.patchPatch) patches

-- | Write matrices to a file for visual reference.
writeMatrices :: IO ()
writeMatrices = Text.IO.writeFile "matrices.txt" $ Text.unlines $
    map (showMatrix . fst . fst) instruments

showMatrix :: VslInst.Instrument -> Text
showMatrix (name, _, attrs) =
    name <> ":\n" <> Text.unlines (map format matrices)
    where
    matrices = Lists.chunked cols $ concatMap (Lists.chunked cols)
        (mapShape strip attrs)
    format = Text.unlines . Texts.columns 1
        . zipWith (:) colHeader . (header:) . map (map ShowVal.showVal)
    header = take cols $ map showt [1..]
    colHeader = take (cols+1) $ map Text.singleton $ '-' : ['a'..]
    strip = stripAttrs . map (`Attrs.difference` variants)
    variants = VslInst.updown <> VslInst.crescdim <> VslInst.highlow
    cols = 12

-- | Transform elements but retain the matrix's shape.
mapShape :: ([a] -> [b]) -> [[a]] -> [[b]]
mapShape f rows = split (map length rows) $ f (concat rows)
    where
    split (len:lens) xs = pre : split lens post
        where (pre, post) = splitAt len xs
    split [] _ = []

-- * instrument definition

patches :: [MidiInst.Patch]
patches =
    [ openStrings hmap $ addCode hmap (makePatch inst category)
    | ((inst, hmap), category) <- instruments
    ]
    where
    addCode hmap patch = MidiInst.code
        #= MidiInst.noteCalls (noteCalls hmap (MidiInst.patchPatch patch)) $
            patch
    openStrings Nothing = id
    openStrings (Just hmap) =
        MidiInst.environ EnvKey.openStrings (hmapStringNns hmap)

instruments :: [((VslInst.Instrument, Maybe HarmonicMap), Text)]
instruments = concatMap tag $
    (soloStringInstruments, Tag.cStrings)
    : noHmap
    [ (VslInst.strings, Tag.cStrings)
    , (VslInst.harps, Tag.cStrings)
    , (VslInst.woodwinds1, Tag.cWoodwinds)
    , (VslInst.woodwinds2, Tag.cWoodwinds)
    , (VslInst.brass1, Tag.cBrass)
    ]
    where
    tag (inst, t) = map (, t) inst
    noHmap = map (first (map (, Nothing)))

soloStringInstruments :: [(VslInst.Instrument, Maybe HarmonicMap)]
soloStringInstruments = map (second Just)
    [ (VslInst.soloViolin, violinHarmonics)
    , (VslInst.soloViola, violaHarmonics)
    , (VslInst.soloCello, celloHarmonics)
    , (VslInst.soloBass, bassHarmonics)
    ]

-- | Add various note calls, depending on the attributes that the patch
-- understands.
noteCalls :: Maybe HarmonicMap -> Patch.Patch -> [MidiInst.Call Derive.Note]
noteCalls maybeHmap patch = concat
    [ withAttr Attrs.trill [g "tr" (Trill.cNoteTrill True Nothing Nothing)]
    , withAttr Attrs.trem [MidiInst.generator "trem"
        (Trill.cTremoloGenerator (Just ([Attrs.harm], VslInst.rep)))]
    , withAttr VslInst.grace [g "g" (graceCall (patchAttributes patch))]
    , withAttr VslInst.legato
        [g "(" (Articulation.cAttrSlur VslInst.fa Attrs.legato)]
    , MidiInst.nullCalls (noteCall patch)
    , [MidiInst.both "sec" cInferSeconds]
    ]
    where
    g = MidiInst.generator
    withAttr attr calls = if hasAttr attr patch then calls else []
    noteCall patch = Note.noteCall ""
        "This is like the standard note call, but ignores attrs that are\
            \ already handled with keyswitches."
        mempty $
        maybe (Note.defaultNote config)
            (harmonic (Patch.patchAttributeMap patch) config)
            maybeHmap
        where config = noteConfig patch
    noteConfig patch = Note.useAttributes
        { Note.configStaccato = not $ hasAttr Attrs.staccato patch }

patchAttributes :: Patch.Patch -> [Attrs.Attributes]
patchAttributes = Common.mappedAttributes . Patch.patchAttributeMap

hasAttr :: Attrs.Attributes -> Patch.Patch -> Bool
hasAttr attr = any (`Attrs.contain` attr) . patchAttributes

graceCall :: [Attrs.Attributes] -> Derive.Generator Derive.Note
graceCall attrs =
    GraceUtil.cAttrGrace (Map.filter (`elem` attrs) graceIntervals)

graceIntervals :: Map Int Attrs.Attributes
graceIntervals = Map.fromList $
    [(n, VslInst.grace <> VslInst.up <> attrs) | (n, attrs) <- ints]
    ++ [(-n, VslInst.grace <> VslInst.down <> attrs) | (n, attrs) <- ints]
    where ints = zip [1..] VslInst.intervalsToOct

-- | If +harm+nat (and optionally a string) attributes are present, try to
-- play this pitch as a natural harmonic.  That means replacing the pitch and
-- reapplying the default note call.
harmonic :: Patch.AttributeMap -> Note.Config -> HarmonicMap
    -> Note.GenerateNote
harmonic attrMap config hmap args = do
    attrs <- Call.getAttributes
    let has = Attrs.contain attrs
    let matched = maybe mempty fst $ Common.lookupAttributes attrs attrMap
    withPitch <- if
        | Attrs.contain matched (Attrs.harm <> Attrs.natural) ->
            naturalHarmonic (has Attrs.gliss) $
                List.find has (hmapStrings hmap)
        -- VSL has its artificial harmonics pitched one octave too high.
        | has Attrs.harm -> pure $ Call.addConstant Controls.octave (-1)
        | otherwise -> pure id
    withPitch $ Note.defaultNote config args
    where
    naturalHarmonic gliss maybeString = do
        nn <- Derive.require "note pitch"
            =<< Derive.nnAt =<< Args.realStart args
        let pitch = Midi.toKey (round nn)
        case findHarmonic hmap gliss pitch maybeString of
            Nothing -> Derive.throw $ pretty pitch <> " unplayable on "
                <> maybe (pretty (hmapStrings hmap)) pretty maybeString
            Just key -> pure $
                Call.withPitch (PSignal.nnPitch (Midi.fromKey key))

-- * keyswitches

type Instrument = (InstT.Name, [Keyswitch])
type Keyswitch = (Attrs.Attributes, [Patch.Keyswitch])

makePatch :: VslInst.Instrument -> Text -> MidiInst.Patch
makePatch inst category =
    instrumentPatch category (second strip (makeInstrument inst))
    where strip = uncurry zip . first stripAttrs . unzip

instrumentPatch :: Text -> Instrument -> MidiInst.Patch
instrumentPatch category (name, keyswitches) =
    -- MidiInst.pressure means I expect to have velocity-xf enabled and
    -- assigned to cc2.
    MidiInst.pressure $
    MidiInst.common#Common.tags %= ((Tag.category, category) :) $
    MidiInst.attributeMap #= keyswitchMap keyswitches $
        MidiInst.namedPatch (-2, 2) name []

makeInstrument :: VslInst.Instrument -> Instrument
makeInstrument (name, keys, attrs) = (name, matrix keys attrs)

keyswitchMap :: [Keyswitch] -> Patch.AttributeMap
keyswitchMap = Patch.keyswitches . Lists.sortOn (priority . fst) . process
    where
    process keyswitches = zip attrs ks
        where (attrs, ks) = unzip (dropDups keyswitches)
    dropDups = Lists.uniqueOn fst
    priority attrs = Map.findWithDefault 0 attrs attributePriority

-- | Order attributes by priority.  This should correspond to specificity, or
-- to perceptual importance, as documented in 'Patch.AttributeMap'.
attributePriority :: Map Attrs.Attributes Int
attributePriority = Map.fromList ((`zip` [-1, -2 ..]) (reverse high)) <> low
    where
    high =
        [ VslInst.pizz
        , VslInst.spiccato
        , VslInst.harsh
        , VslInst.staccato
        , VslInst.detache
        , VslInst.detache <> VslInst.long
        ]
    low = Map.fromList [(VslInst.nv, 100)]

-- | Since the VSL matrix is only 12x12, a row of articulations greater than
-- that overflows to the next row.  Given that I'm definitely going to overflow
-- 12 rows, I wind up overflowing to the next matrix, and when counting tha AB
-- switch, each articulation has 3 or 4 keyswitches.
--
-- I could probably cram most instruments into 144 cells and only require 2-3
-- keyswitches, but it would be hard to read and easy to mess up, wouldn't let
-- me disable and enable cells by row, and with custom patches I'll probably
-- wind up with more than 144 anyway.
matrix :: VslInst.Keys -> [[Attrs.Attributes]] -> [Keyswitch]
matrix keys = add . Lists.chunked 12 . concatMap (Lists.chunked 12)
    where
    add matrices = do
        (matrixKs, rows) <- zip selectMatrix matrices
        (rowKs, row) <- zip yAxis rows
        (colKs, cell) <- zip xAxis row
        (abKs, attrs) <- maybe [([], cell)] (zip (map (:[]) ab)) $
            expandAb cell
        pure (attrs, matrixKs : rowKs : colKs : abKs)
    xAxis = keysFrom (VslInst.keyXAxis keys)
    yAxis = keysFrom (VslInst.keyYAxis keys)
    ab = keysFrom (VslInst.keyAb keys)
    selectMatrix = keysFrom (VslInst.keyMatrix keys)

keysFrom :: Midi.Key -> [Patch.Keyswitch]
keysFrom lowKey = map Patch.Keyswitch [lowKey ..]

-- * attrs

-- | Remove attrs which can be assumed as a default.  The idea is to make it
-- easier to address an articulation while still remaining non-ambiguous.
--
-- Attrs are removed in order unless removal would create a conflict with
-- another articulation.  The result is that the attrs early in the strip list
-- are removed first, so if you have both 'VslInst.med' and 'VslInst.short',
-- 'VslInst.med' will become the default, while 'VslInst.short' retains its
-- attribute.
stripAttrs :: [Attrs.Attributes] -> [Attrs.Attributes]
stripAttrs attrs = snd $ foldr stripAttr (Set.fromList attrs, attrs) strip
    where
    strip = reverse
        [ VslInst.sus, VslInst.vib, VslInst.perf, VslInst.fast, VslInst.fa
        , VslInst.norm, VslInst.na, VslInst.legato, VslInst.v1, Attrs.artificial
        , VslInst.med, VslInst.short
        ]

-- | Strip the given attr, but only if it wouldn't cause clashes.
stripAttr :: Attrs.Attributes -> (Set Attrs.Attributes, [Attrs.Attributes])
    -> (Set Attrs.Attributes, [Attrs.Attributes])
stripAttr attr (allAttrsSet, allAttrs)
    | any (`Attrs.contain` attr) allAttrs =
        List.mapAccumL stripRedundant allAttrsSet allAttrs
    | otherwise = (allAttrsSet, allAttrs)
    where
    -- Initially I had a naive version that search for clashes with a linear
    -- search in 'all_attrs'.  But it turns out to be slow since there are
    -- around 41 instruments * 12 attrs to strip * 285 attrs * 285 for linear
    -- search.  Or something.  Anyway, previously forcing all the patches took
    -- 0.39 CPU seconds, now it's down to 0.19.
    --
    -- This whole calculation winds up in 'patches' as a CAF, so it should be
    -- possible to run at compile time, presumably via TH.  In any case, even
    -- as a normal CAF, it only happens once per run.
    stripRedundant attrsSet attrs
        | Set.member stripped attrsSet = (attrsSet, attrs)
        | otherwise = (Set.insert stripped attrsSet, stripped)
        where stripped = Attrs.difference attrs attr

expandAb :: Attrs.Attributes -> Maybe [Attrs.Attributes]
expandAb attrs
    | Just stripped <- extract VslInst.updown =
        Just [stripped <> VslInst.up, stripped <> VslInst.down]
    | Just stripped <- extract VslInst.crescdim =
        Just $ map (`Attrs.difference` VslInst.dyn)
            [stripped <> VslInst.cresc, stripped <> VslInst.dim]
    | Just stripped <- extract VslInst.highlow =
        Just [stripped <> VslInst.high, stripped <> VslInst.low]
    | otherwise = Nothing
    where
    extract attr
        | Attrs.contain attrs attr = Just $ Attrs.difference attrs attr
        | otherwise = Nothing


-- * natural harmonics

data HarmonicMap = HarmonicMap
    { hmapStrings :: [OpenString]
    , hmapStringNns :: [Pitch.NoteNumber]
    -- | Map sounding pitch to possible strings and the key to play to get that
    -- pitch on that string.
    , hmapKeyToNatural :: Map Midi.Key [(OpenString, Midi.Key)]
    -- | Same as 'hmap_key_to_natural' except map from the gliss destination.
    , hmapKeyToGlissDestination :: Map Midi.Key [(OpenString, Midi.Key)]
    } deriving (Show)
type OpenString = Attrs.Attributes

findHarmonic :: HarmonicMap -> Bool -> Midi.Key -> Maybe OpenString
    -> Maybe Midi.Key
findHarmonic hmap gliss pitch maybeStr =
    maybe (fmap snd . Lists.head) lookup maybeStr =<< Map.lookup pitch m
    where
    m = (if gliss then hmapKeyToGlissDestination else hmapKeyToNatural)
        hmap

harmonicMap :: [(OpenString, Midi.Key)] -> HarmonicMap
harmonicMap strings = HarmonicMap
    { hmapStrings = map fst strings
    , hmapStringNns = map (Midi.fromKey . snd) strings
    , hmapKeyToNatural = make naturalHarmonics
    , hmapKeyToGlissDestination = make glissNaturalHarmonics
    }
    where
    make keyToInterval = Maps.multimap $ do
        (oct, (str, base)) <- zip [0..] strings
        (key, interval) <- keyToInterval
        pure (add base interval, (str, add key (oct * 12)))
        where add key n = Midi.toKey (Midi.fromKey key + n)

violinHarmonics, violaHarmonics, celloHarmonics, bassHarmonics
    :: HarmonicMap
violinHarmonics = harmonicMap $ map (first Attrs.attr)
    [("g", Key.g3), ("d", Key.d4), ("a", Key.a4), ("e", Key.e4)]
violaHarmonics = harmonicMap $ map (first Attrs.attr)
    [("c", Key.c3), ("g", Key.g3), ("d", Key.d4), ("a", Key.a4)]
celloHarmonics = harmonicMap $ map (first Attrs.attr)
    [("c", Key.c2), ("g", Key.g2), ("d", Key.d3), ("a", Key.a3)]
bassHarmonics = harmonicMap $ map (first Attrs.attr)
    [("e", Key.e1), ("a", Key.a1), ("d", Key.d2), ("g", Key.g2)]

naturalHarmonics :: [(Midi.Key, Int)]
naturalHarmonics = absolute
    [ (Key.c3, 0)
    , (Key.d3, 12)
    , (Key.e3, 7)
    , (Key.f3, 5)
    , (Key.g3, 4)
    , (Key.gs3, 3)
    , (Key.a3, 3)
    , (Key.as3, 2)
    ]
    where absolute = uncurry zip . second (drop 1 . scanl (+) 0) . unzip

glissNaturalHarmonics :: [(Midi.Key, Int)]
glissNaturalHarmonics = absolute
    [ (Key.c3, 12)
    , (Key.d3, 7)
    , (Key.e3, 5)
    , (Key.f3, 4)
    , (Key.g3, 3)
    , (Key.a3, 3)
    , (Key.as3, 2)
    ]
    where absolute = uncurry zip . second (drop 1 . scanl (+) 0) . unzip

-- * infer seconds

cInferSeconds :: Library.Calls Derive.Note
cInferSeconds = Make.transformNotes Module.instrument "infer-seconds"
    Tags.attr
    "Infer a `+sec#` attr based on the duration of the event and the available\
    \ keymaps for the current instrument and attribute set."
    ((,)
    <$> Sig.defaulted "attrs" (mempty :: Attrs.Attributes)
        "Add these attributes."
    <*> Sig.defaulted "round" (Nothing :: Maybe Sig.Dummy)
        "Round up to a longer `+sec#` attribute,\
        \ down to a shorter one, or to the closest."
    ) $ \(attrs, round) deriver -> do
        (_, inst) <- Derive.getInstrument =<< Call.getInstrument
        Post.emap1_ (inferSeconds round (Derive.instAttributes inst)) <$>
            Call.addAttributes attrs deriver

inferSeconds :: Maybe Call.UpDown -> [Attrs.Attributes] -> Score.Event
    -> Score.Event
inferSeconds round instAttrs event = case closest of
    Nothing -> event
    Just (secs, _) -> Score.addAttributes (VslInst.sec secs) event
    where
    dur = Score.eventDuration event
    closest = Lists.minimumOn (abs . subtract dur . fst) $ case round of
        Nothing -> relevantAttrs
        Just Call.Up -> filter ((>=dur) . fst) relevantAttrs
        Just Call.Down -> filter ((<=dur) . fst) relevantAttrs
    relevantAttrs :: [(RealTime, Attrs.Attributes)]
    relevantAttrs = filter (Attrs.contain eventAttrs . snd) $
        mapMaybe VslInst.parseSec instAttrs
    eventAttrs = Score.eventAttributes event
