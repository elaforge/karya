-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Manage the optional keycaps window.
module Cmd.SyncKeycaps (open, close, update, get_bindings) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
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
    state <- keycaps_state
    bindings <- get_bindings state
    -- Put the window in the bottom right corner, to minimize conflict with
    -- ViewConfig.horizontal_tile.
    (sx, sy) <- Rect.lower_right <$> Cmd.get_screen Nothing
    let pos = let (x, y) = KeycapsT.lt_size layout in (sx-x, sy-y)
    set_update $ Cmd.KeycapsUpdate state (Just (pos, layout))
        (Keycaps.resolve_bindings layout bindings)

close :: Cmd.M m => m ()
close = set_update Cmd.KeycapsClose

update :: Cmd.M m => m ()
update = whenJustM (Cmd.gets Cmd.state_keycaps) $ \old_state -> do
    state <- keycaps_state
    when (old_state /= state) $ do
        bindings <- get_bindings state
        set_update $ Cmd.KeycapsUpdate state Nothing
            (Keycaps.resolve_bindings layout bindings)

set_update :: Cmd.M m => Cmd.KeycapsUpdate -> m ()
set_update kc_update = Cmd.modify $ \st ->
    st { Cmd.state_keycaps_update = Just kc_update }

layout :: KeycapsT.Layout
layout = Keycaps.make_layout Keycaps.mac_labels

keycaps_state :: Cmd.M m => m Cmd.KeycapsState
keycaps_state = do
    kc_mods <- Cmd.gets $ key_mods . Cmd.state_keys_down
    kc_octave <- Cmd.gets $ Cmd.state_kbd_entry_octave . Cmd.state_edit
    kc_is_kbd_entry <- Cmd.is_kbd_entry
    -- This could fail if there is no focused block.
    (kc_track_type, kc_instrument, kc_scale_id) <-
        fmap (fromMaybe (Nothing, Nothing, Nothing)) $ Cmd.catch_abort $ do
            block_id <- Cmd.get_focused_block
            tracknum <- Cmd.abort_unless =<< Cmd.get_insert_tracknum
            mb_track_id <- Ui.event_track_at block_id tracknum
            (,,) <$> traverse
                    (fmap ParseTitle.track_type . Ui.get_track_title)
                    mb_track_id
                <*> maybe (return Nothing)
                    (\tid -> Perf.lookup_instrument (block_id, Just tid))
                    mb_track_id
                <*> (Just <$> Perf.get_scale_id (block_id, mb_track_id))
    return $ Cmd.KeycapsState
        { kc_mods, kc_octave, kc_is_kbd_entry
        , kc_track_type, kc_instrument, kc_scale_id
        }

key_mods :: Map Cmd.Modifier Cmd.Modifier -> Set Cmd.Modifier
key_mods keys_down =
    Set.fromList [Cmd.KeyMod m | Cmd.KeyMod m <- Map.keys keys_down]

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
get_bindings :: Cmd.M m => Cmd.KeycapsState -> m KeycapsT.Bindings
get_bindings (Cmd.KeycapsState
        { kc_mods, kc_octave, kc_is_kbd_entry, kc_track_type, kc_instrument
        , kc_scale_id
        }) = do
    inst_bindings <- case (kc_is_kbd_entry, kc_track_type, kc_instrument) of
        (True, Just ParseTitle.NoteTrack, Just inst)
            | kc_mods `elem` [mempty, shift] ->
                merge_note_entry (kc_mods == shift) kc_octave <$>
                    get_instrument_bindings inst
        _ -> return mempty
    return $ Map.unions $ reverse
        [ Map.findWithDefault mempty kc_mods global_bindings
        , case kc_track_type of
            Just ParseTitle.NoteTrack ->
                Map.findWithDefault mempty kc_mods note_track_bindings
            _ -> mempty
        , if not (kc_is_kbd_entry && Set.null kc_mods) then mempty
            else case kc_track_type of
                Just ParseTitle.PitchTrack -> pitch_bindings
                Just ParseTitle.NoteTrack -> pitch_bindings
                _ -> mempty
        , inst_bindings
        ]
    where
    shift = Set.singleton $ Cmd.KeyMod Key.Shift
    pitch_bindings = fromMaybe mempty $ Map.lookup kc_octave
        =<< (\scale_id -> Map.lookup scale_id scale_to_bindings)
        =<< kc_scale_id

merge_note_entry :: Bool -> Pitch.Octave -> [Cmd.NoteEntryMap KeycapsT.Binding]
    -> KeycapsT.Bindings
merge_note_entry shifted octave = Map.unions . map get
    where
    get = \case
        Cmd.WithOctave m -> convert $ Map.findWithDefault mempty octave m
        Cmd.WithoutOctave m -> convert m
    convert = Map.fromList
        . Seq.map_maybe_fst (fmap (to_logical . char_to_keycap) . unshift)
        . Map.toList
    -- Like Cmd.Keymap, I used shifted characters as a shorthand for Shift +
    -- unshifted.  This is possible since I know the exact key layout.
    unshift
        | shifted = KeyLayouts.to_unshifted Local.KeyLayout.layout
        | otherwise = Just

char_to_keycap :: Char -> KeycapsT.Keycap
char_to_keycap = Key.to_mac_label . Key.Char

-- ** instrument bindings

-- | Get bindings for the given instrument.  This isn't cached for now, but
-- it wouldn't be hard to keep a Map Instrument Bindings cache if it's slow.
get_instrument_bindings :: Cmd.M m => ScoreT.Instrument
    -> m [Cmd.NoteEntryMap KeycapsT.Binding]
get_instrument_bindings inst = do
    resolved <- Cmd.lookup_instrument inst
    let handlers = maybe []
            (Cmd.inst_cmds . Common.common_code . Inst.inst_common
                . Cmd.inst_instrument)
            resolved
    return
        [ note_entry_bindings inst (Cmd.cmd_name cmd) Config.note_color
            note_entry
        | Cmd.Handler (Just note_entry) cmd <- handlers
        ]

note_entry_bindings :: ScoreT.Instrument -> Text -> Color.Color
    -> Cmd.NoteEntryMap Text -> Cmd.NoteEntryMap KeycapsT.Binding
note_entry_bindings inst source color = fmap binding
    where
    binding name
        | Text.null name = KeycapsT.no_binding
        | otherwise = KeycapsT.Binding
            { b_color = Just color
            , b_text = name
            , b_doc = ">" <> pretty inst <> ": " <> source <> ": " <> name
            }

-- ** pitch bindings

scale_to_bindings :: Map Pitch.ScaleId (Map Pitch.Octave KeycapsT.Bindings)
scale_to_bindings = bindings <$> Scale.All.scales
    where
    bindings = \case
        Scale.Simple scale -> for_octaves scale
        Scale.Make _scale_id _doc env_to_scale ->
            either (const mempty) for_octaves $
                env_to_scale env Scale.All.lookup_scale
    for_octaves scale =
        Map.fromList [(oct, scale_bindings oct scale) | oct <- [0..8]]
    -- This likely means Scale.Makes won't work, but I can fix that later if I
    -- want them.
    env = mempty

scale_bindings :: Pitch.Octave -> Scale.Scale -> KeycapsT.Bindings
scale_bindings octave scale =
    make_pitches $ Either.rights $
        map (traverse (Scale.scale_input_to_note scale env)) (kbd_inputs octave)
    where
    -- Env-sensitive notes would prevent me from caching.  Env is likely to be
    -- key and tuning, which I think should affect pitch, but not symbolic note
    -- names.
    env = mempty

-- | TODO unused, but I might want it if I need env-sensitivity.
_compute_pitch_bindings :: Cmd.M m => Pitch.Octave -> m KeycapsT.Bindings
_compute_pitch_bindings octave =
    make_pitches <$> EditUtil.inputs_to_notes (kbd_inputs octave)

kbd_inputs :: Pitch.Octave -> [(Char, Pitch.Input)]
kbd_inputs octave = map (second make_input) $ Map.toList $ PhysicalKey.pitch_map
    where
    make_input p = Pitch.Input Pitch.AsciiKbd (Pitch.add_octave octave p) 0

make_pitches :: [(Char, Pitch.Note)] -> KeycapsT.Bindings
make_pitches char_notes = Map.fromList
    [ (to_logical $ char_to_keycap char, binding note)
    | (char, note) <- char_notes
    ]
    where
    binding note = KeycapsT.Binding
        { b_color = Just Config.pitch_color
        , b_text = Pitch.note_text note
        , b_doc = "pitch: " <> Pitch.note_text note
        }

-- ** global bindings

global_bindings :: Map (Set Cmd.Modifier) KeycapsT.Bindings
global_bindings = bindings_cache Nothing GlobalKeymap.all_cmd_map

note_track_bindings :: Map (Set Cmd.Modifier) KeycapsT.Bindings
note_track_bindings =
    bindings_cache (Just Config.note_color) NoteTrackKeymap.keymap

bindings_cache :: Maybe Color.Color -> Cmd.Keymap m
    -> Map (Set Cmd.Modifier) KeycapsT.Bindings
bindings_cache mb_color keymap = Map.fromList $ Seq.key_on_snd mod_bindings $
    map (Set.fromList . mapMaybe Keymap.simple_to_mod)
    [ []
    , [Keymap.Shift]
    , [Keymap.PrimaryCommand]
    , [Keymap.Shift, Keymap.PrimaryCommand]
    , [Keymap.SecondaryCommand]
    , [Keymap.Shift, Keymap.SecondaryCommand]
    ]
    where
    mod_bindings kmods =
        Map.fromList $ map (second binding) $ keymap_docs kmods keymap
    binding doc = KeycapsT.Binding
        { b_color = mb_color
        , b_text = abbreviate doc
        , b_doc = doc
        }

keymap_docs :: Set Cmd.Modifier -> Cmd.Keymap m
    -> [(KeycapsT.KeyDoc, KeycapsT.Doc)]
keymap_docs mods =
    map (bimap to_logical (\(Cmd.NamedCmd name _) -> name))
    . Seq.map_maybe_fst (key_spec_label mods)
    . Map.toList

to_logical :: KeycapsT.Keycap -> KeycapsT.Keycap
to_logical label = case Text.unpack label of
    [c] | Just c2 <- KeyLayouts.to_qwerty Local.KeyLayout.layout c ->
        Text.singleton c2
    _ -> label

-- | Make a short mnemonic that can fit in on a keycap.
abbreviate :: Text -> KeycapsT.KeyDoc
abbreviate =
    Text.pack . map symbolize . take 3 . filter (`Set.notMember` boring)
    . Text.words
    where
    symbolize w = Map.findWithDefault (Text.head w) w word_symbol
    boring = Set.fromList ["to", "in", "or", "from", "then"]

word_symbol :: Map Text Char
word_symbol = Map.fromList $ concatMap (\(ks, v) -> map (,v) ks)
    [ (["rewind", "up", "above"], '↑')
    , (["advance", "down", "below"], '↓')
    , (["left", "previous"], '←')
    , (["right", "next"], '→')
    , (["play"], '▸')
    , (["stop"], '￭')
    , (["top"], '上')
    , (["bottom"], '下')
    ]

key_spec_label :: Set Cmd.Modifier -> Cmd.KeySpec -> Maybe KeycapsT.Keycap
key_spec_label mods (Cmd.KeySpec smods bindable) | mods == smods =
    case bindable of
        Cmd.Key _repeat key -> Just $ Key.to_mac_label key
        _ -> Nothing
key_spec_label _ _ = Nothing
