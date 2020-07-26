-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Manage the optional keycaps window.
module Cmd.SyncKeycaps (open, close, update, get_bindings) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Keymap as Keymap
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.Perf as Perf

import qualified Derive.ParseTitle as ParseTitle
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
    set_update $ Cmd.KeycapsUpdate state (Just ((200, 200), layout))
        (Keycaps.resolve_bindings layout bindings)
    -- TODO put it in the bottom right corner, to minimize conflict with
    -- ViewConfig.horizontal_tile.

close :: Cmd.M m => m ()
close = set_update Cmd.KeycapsClose

update :: Cmd.M m => m ()
update = whenJustM (Cmd.gets Cmd.state_keycaps) $ \old_state -> do
    state <- keycaps_state
    when (old_state /= state) $ do
        bindings <- get_bindings state
        set_update $ Cmd.KeycapsUpdate state Nothing
            (Keycaps.resolve_bindings layout bindings)

keycaps_state :: Cmd.M m => m Cmd.KeycapsState
keycaps_state = do
    kc_mods <- Cmd.gets $ key_mods . Cmd.state_keys_down
    kc_octave <- Cmd.gets $ Cmd.state_kbd_entry_octave . Cmd.state_edit
    kc_is_kbd_entry <- Cmd.is_kbd_entry
    -- This could fail if there is no focused block.
    (kc_track_type, kc_instrument) <- fmap (fromMaybe (Nothing, Nothing)) $
        Cmd.catch_abort $ do
            block_id <- Cmd.get_focused_block
            tracknum <- Cmd.abort_unless =<< Cmd.get_insert_tracknum
            mb_track_id <- Ui.event_track_at block_id tracknum
            kc_track_type <- traverse
                (fmap ParseTitle.track_type . Ui.get_track_title)
                mb_track_id
            kc_instrument <- maybe (return Nothing)
                (\tid -> Perf.lookup_instrument (block_id, Just tid))
                mb_track_id
            return (kc_track_type, kc_instrument)
    return $ Cmd.KeycapsState
        { kc_mods, kc_octave, kc_is_kbd_entry, kc_track_type, kc_instrument }

-- No caching for now, but if it's slow I should be able to have either a
-- cache 'Map BlockId (Map TrackNum KeycapsT.Bindings)' or maybe
-- 'Map {Instrument,TrackType,ScaleId} KeycapsT.Bindings', that way I don't
-- have to invalidate.
get_bindings :: Cmd.M m => Cmd.KeycapsState -> m KeycapsT.Bindings
get_bindings (Cmd.KeycapsState
        { kc_mods, kc_octave, kc_is_kbd_entry, kc_track_type, kc_instrument
        }) = do
    -- TODO this duplicates the work in Cmd.Track
    inst_bindings <- case (kc_is_kbd_entry, kc_track_type, kc_instrument) of
        (True, Just ParseTitle.NoteTrack, Just inst) | Set.null kc_mods ->
            -- TODO also shift is ok
            get_instrument_bindings inst
        _ -> return []
    -- TODO this is analogous to the list in Track.get_track_cmds.
    return $ Map.unions $ reverse
        [ Map.findWithDefault mempty kc_mods global_bindings
        , case kc_track_type of
            Just ParseTitle.NoteTrack ->
                Map.findWithDefault mempty kc_mods note_track_bindings
            _ -> mempty
        , merge_note_entry kc_octave inst_bindings
        ]

merge_note_entry :: Pitch.Octave -> [Cmd.NoteEntryMap KeycapsT.Binding]
    -> KeycapsT.Bindings
merge_note_entry octave = Map.unions . map get
    where
    get = \case
        Cmd.WithOctave m -> convert $ Map.findWithDefault mempty octave m
        Cmd.WithoutOctave m -> convert m
    convert = Map.mapKeys (to_logical . char_to_keycap)

char_to_keycap :: Char -> KeycapsT.Keycap
char_to_keycap = Key.to_mac_label . Key.Char

get_instrument_bindings :: Cmd.M m => ScoreT.Instrument
    -> m [Cmd.NoteEntryMap KeycapsT.Binding]
get_instrument_bindings inst = do
    -- TODO this duplicates the work in Cmd.Track
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
    -- TODO char has shift in it, map back again, and later split shift back out
    binding name
        | Text.null name = KeycapsT.no_binding
        | otherwise = KeycapsT.Binding
            { b_color = Just color
            , b_text = name
            , b_doc = ">" <> pretty inst <> ": " <> source <> ": " <> name
            }

key_mods :: Map Cmd.Modifier Cmd.Modifier -> Set Cmd.Modifier
key_mods keys_down =
    Set.fromList [Cmd.KeyMod m | Cmd.KeyMod m <- Map.keys keys_down]

layout :: KeycapsT.Layout
layout = Keycaps.make_layout Keycaps.mac_labels

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

abbreviate :: Text -> KeycapsT.KeyDoc
abbreviate = Text.pack . map Text.head . take 3 . Text.words

key_spec_label :: Set Cmd.Modifier -> Cmd.KeySpec -> Maybe KeycapsT.Keycap
key_spec_label mods (Cmd.KeySpec smods bindable) | mods == smods =
    case bindable of
        Cmd.Key _repeat key -> Just $ Key.to_mac_label key
        _ -> Nothing
key_spec_label _ _ = Nothing

set_update :: Cmd.M m => Cmd.KeycapsUpdate -> m ()
set_update kc_update = Cmd.modify $ \st ->
    st { Cmd.state_keycaps_update = Just kc_update }
