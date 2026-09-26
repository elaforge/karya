-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Manage the optional keycaps window.
module Cmd.SyncKeycaps (open, close, update, getBindings) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Rect as Rect
import qualified Util.Lists as Lists
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Keymap as Keymap
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.Perf as Perf
import qualified Cmd.PhysicalKey as PhysicalKey

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Local.KeyLayout
import qualified Perform.Pitch as Pitch
import qualified Ui.Color as Color
import qualified Ui.Key as Key
import qualified Ui.Keycaps as Keycaps
import qualified Ui.KeycapsT as KeycapsT
import qualified Ui.Ui as Ui

import           Global


open :: Cmd.M m => m ()
open = do
    state <- keycapsState
    bindings <- getBindings state
    -- Put the window in the bottom right corner, to minimize conflict with
    -- ViewConfig.horizontal_tile.
    (sx, sy) <- Rect.lowerRight <$> Cmd.getScreen Nothing
    let pos = let (x, y) = KeycapsT.ltSize layout in (sx-x, sy-y)
    setUpdate $ Cmd.KeycapsUpdate state (Just (pos, layout))
        (Keycaps.resolveBindings layout bindings)

close :: Cmd.M m => m ()
close = setUpdate Cmd.KeycapsClose

update :: Cmd.M m => m ()
update = whenJustM (Cmd.gets Cmd.stateKeycaps) $ \oldState -> do
    state <- keycapsState
    when (oldState /= state) $ do
        bindings <- getBindings state
        setUpdate $ Cmd.KeycapsUpdate state Nothing
            (Keycaps.resolveBindings layout bindings)

setUpdate :: Cmd.M m => Cmd.KeycapsUpdate -> m ()
setUpdate kcUpdate = Cmd.modify $ \st ->
    st { Cmd.stateKeycapsUpdate = Just kcUpdate }

layout :: KeycapsT.Layout
layout = Keycaps.makeLayout Keycaps.macLabels

keycapsState :: Cmd.M m => m Cmd.KeycapsState
keycapsState = do
    kcMods <- Cmd.gets $ keyMods . Cmd.stateKeysDown
    kcOctave <- Cmd.gets $ Cmd.stateKbdEntryOctave . Cmd.stateEdit
    kcIsKbdEntry <- Cmd.isKbdEntry
    -- This could fail if there is no focused block.
    (kcTrackType, kcInstrument, kcScaleId) <-
        fmap (fromMaybe (Nothing, Nothing, Nothing)) $ Cmd.catchAbort $ do
            blockId <- Cmd.getFocusedBlock
            tracknum <- Cmd.abortUnless =<< Cmd.getInsertTracknum
            mbTrackId <- Ui.eventTrackAt blockId tracknum
            (,,) <$> traverse
                    (fmap ParseTitle.trackType . Ui.getTrackTitle)
                    mbTrackId
                <*> maybe (pure Nothing)
                    (\tid -> Perf.lookupInstrument (blockId, Just tid))
                    mbTrackId
                <*> (Just <$> Perf.getScaleId (blockId, mbTrackId))
    pure $ Cmd.KeycapsState
        { kcMods, kcOctave, kcIsKbdEntry
        , kcTrackType, kcInstrument, kcScaleId
        }

keyMods :: Map Cmd.Modifier Cmd.Modifier -> Set Cmd.Modifier
keyMods keysDown =
    Set.fromList [Cmd.KeyMod m | Cmd.KeyMod m <- Map.keys keysDown]

-- | Get the applicable bindings for the KeycapsState.
--
-- This duplicates Cmd.Track, and the checks duplicate the checks inside
-- the underlying calls like EditUtil.fallthrough and Cmd.is_kbd_entry.  It's
-- not exactly ideal, but it seemed more practical than, say, trying to get
-- all Cmds into the Keymap mold.  The kbd_entry ones would waste a lot of
-- allocation on each cmd constructing a Map just to look up a single key.
--
-- But the duplication means that the keycaps can be inaccurate, especially the
-- exact shadowing situtaion may be incorrect.
getBindings :: Cmd.M m => Cmd.KeycapsState -> m KeycapsT.Bindings
getBindings (Cmd.KeycapsState
        { kcMods, kcOctave, kcIsKbdEntry, kcTrackType, kcInstrument
        , kcScaleId
        }) = do
    instBindings <- case (kcIsKbdEntry, kcTrackType, kcInstrument) of
        (True, Just ParseTitle.NoteTrack, Just inst)
            | kcMods `elem` [mempty, shift] ->
                mergeNoteEntry (kcMods == shift) kcOctave <$>
                    getInstrumentBindings inst
        _ -> pure mempty
    pure $ Map.unions $ reverse
        [ Map.findWithDefault mempty kcMods globalBindings
        , case kcTrackType of
            Just ParseTitle.NoteTrack ->
                Map.findWithDefault mempty kcMods noteTrackBindings
            _ -> mempty
        , if not (kcIsKbdEntry && Set.null kcMods) then mempty
            else case kcTrackType of
                Just ParseTitle.PitchTrack -> pitchBindings
                Just ParseTitle.NoteTrack -> pitchBindings
                _ -> mempty
        , instBindings
        ]
    where
    shift = Set.singleton $ Cmd.KeyMod Key.Shift
    pitchBindings = fromMaybe mempty $ Map.lookup kcOctave
        =<< (\scaleId -> Map.lookup scaleId scaleToBindings)
        =<< kcScaleId

mergeNoteEntry :: Bool -> Pitch.Octave -> [Cmd.NoteEntryMap KeycapsT.Binding]
    -> KeycapsT.Bindings
mergeNoteEntry shifted octave = Map.unions . map get
    where
    get = \case
        Cmd.WithOctave m -> convert $ Map.findWithDefault mempty octave m
        Cmd.WithoutOctave m -> convert m
    convert = Map.fromList
        . Lists.mapMaybeFst (fmap (toLogical . charToKeycap) . unshift)
        . Map.toList
    -- Like Cmd.Keymap, I used shifted characters as a shorthand for Shift +
    -- unshifted.  This is possible since I know the exact key layout.
    unshift
        | shifted = KeyLayouts.toUnshifted Local.KeyLayout.layout
        | otherwise = Just

charToKeycap :: Char -> KeycapsT.Keycap
charToKeycap = Key.toMacLabel . Key.Char

-- ** instrument bindings

-- | Get bindings for the given instrument.  This isn't cached for now, but
-- it wouldn't be hard to keep a Map Instrument Bindings cache if it's slow.
getInstrumentBindings :: Cmd.M m => ScoreT.Instrument
    -> m [Cmd.NoteEntryMap KeycapsT.Binding]
getInstrumentBindings inst = do
    resolved <- Cmd.lookupInstrument inst
    let handlers = maybe []
            (Cmd.instCmds . Common.commonCode . Inst.instCommon
                . Cmd.instInstrument)
            resolved
    pure
        [ noteEntryBindings inst (Cmd.cmdName cmd) Config.noteColor
            noteEntry
        | Cmd.Handler (Just noteEntry) cmd <- handlers
        ]

noteEntryBindings :: ScoreT.Instrument -> Text -> Color.Color
    -> Cmd.NoteEntryMap Text -> Cmd.NoteEntryMap KeycapsT.Binding
noteEntryBindings inst source color = fmap binding
    where
    binding name
        | Text.null name = KeycapsT.noBinding
        | otherwise = KeycapsT.Binding
            { bColor = Just color
            , bText = name
            , bDoc = ">" <> pretty inst <> ": " <> source <> ": " <> name
            }

-- ** pitch bindings

scaleToBindings :: Map Pitch.ScaleId (Map Pitch.Octave KeycapsT.Bindings)
scaleToBindings = bindings <$> Scale.All.scales
    where
    bindings = \case
        Scale.Simple scale -> forOctaves scale
        Scale.Make _scaleId _doc envToScale ->
            either (const mempty) forOctaves $
                envToScale env Scale.All.lookupScale
    forOctaves scale =
        Map.fromList [(oct, scaleBindings oct scale) | oct <- [0..8]]
    -- This likely means Scale.Makes won't work, but I can fix that later if I
    -- want them.
    env = mempty

scaleBindings :: Pitch.Octave -> Scale.Scale -> KeycapsT.Bindings
scaleBindings octave scale =
    makePitches $ Either.rights $
        map (traverse (Scale.scaleInputToNote scale env)) (kbdInputs octave)
    where
    -- Env-sensitive notes would prevent me from caching.  Env is likely to be
    -- key and tuning, which I think should affect pitch, but not symbolic note
    -- names.
    env = mempty

-- | TODO unused, but I might want it if I need env-sensitivity.
_computePitchBindings :: Cmd.M m => Pitch.Octave -> m KeycapsT.Bindings
_computePitchBindings octave =
    makePitches <$> EditUtil.inputsToNotes (kbdInputs octave)

kbdInputs :: Pitch.Octave -> [(Char, Pitch.Input)]
kbdInputs octave = map (second makeInput) $ Map.toList $ PhysicalKey.pitchMap
    where
    makeInput p = Pitch.Input Pitch.AsciiKbd (Pitch.addOctave octave p) 0

makePitches :: [(Char, Pitch.Note)] -> KeycapsT.Bindings
makePitches charNotes = Map.fromList
    [ (toLogical $ charToKeycap char, binding note)
    | (char, note) <- charNotes
    ]
    where
    binding note = KeycapsT.Binding
        { bColor = Just Config.pitchColor
        , bText = Pitch.noteText note
        , bDoc = "pitch: " <> Pitch.noteText note
        }

-- ** global bindings

globalBindings :: Map (Set Cmd.Modifier) KeycapsT.Bindings
globalBindings = bindingsCache Nothing GlobalKeymap.allKeymap

noteTrackBindings :: Map (Set Cmd.Modifier) KeycapsT.Bindings
noteTrackBindings =
    bindingsCache (Just Config.noteColor) NoteTrackKeymap.keymap

bindingsCache :: Maybe Color.Color -> Cmd.Keymap m
    -> Map (Set Cmd.Modifier) KeycapsT.Bindings
bindingsCache mbColor keymap = Map.fromList $ Lists.keyOnSnd modBindings $
    map (Set.fromList . mapMaybe Keymap.simpleToMod)
    [ []
    , [Keymap.Shift]
    , [Keymap.PrimaryCommand]
    , [Keymap.Shift, Keymap.PrimaryCommand]
    , [Keymap.SecondaryCommand]
    , [Keymap.Shift, Keymap.SecondaryCommand]
    ]
    where
    modBindings kmods =
        Map.fromList $ map (second binding) $ keymapDocs kmods keymap
    binding doc = KeycapsT.Binding
        { bColor = mbColor
        , bText = abbreviate doc
        , bDoc = doc
        }

keymapDocs :: Set Cmd.Modifier -> Cmd.Keymap m
    -> [(KeycapsT.KeyDoc, KeycapsT.Doc)]
keymapDocs mods =
    map (bimap toLogical (\(Cmd.NamedCmd name _) -> name))
    . Lists.mapMaybeFst (keySpecLabel mods)
    . Map.toList

toLogical :: KeycapsT.Keycap -> KeycapsT.Keycap
toLogical label = case Text.unpack label of
    [c] | Just c2 <- KeyLayouts.toQwerty Local.KeyLayout.layout c ->
        Text.singleton c2
    _ -> label

-- | Make a short mnemonic that can fit in on a keycap.
abbreviate :: Text -> KeycapsT.KeyDoc
abbreviate =
    Text.pack . map symbolize . take 3 . filter (`Set.notMember` boring)
    . Text.words
    where
    symbolize w = Map.findWithDefault (Text.head w) w wordSymbol
    boring = Set.fromList ["to", "in", "or", "from", "then"]

wordSymbol :: Map Text Char
wordSymbol = Map.fromList $ concatMap (\(ks, v) -> map (,v) ks)
    [ (["rewind", "up", "above"], '↑')
    , (["advance", "down", "below"], '↓')
    , (["left", "previous"], '←')
    , (["right", "next"], '→')
    , (["play"], '▸')
    , (["stop"], '￭')
    , (["top"], '上')
    , (["bottom"], '下')
    ]

keySpecLabel :: Set Cmd.Modifier -> Cmd.KeySpec -> Maybe KeycapsT.Keycap
keySpecLabel mods (Cmd.KeySpec smods bindable) | mods == smods =
    case bindable of
        Cmd.Key _repeat key -> Just $ Key.toMacLabel key
        _ -> Nothing
keySpecLabel _ _ = Nothing
