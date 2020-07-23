-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Manage the optional keycaps window.
module Cmd.SyncKeycaps (open, close, update) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Debug as Debug
import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Info as Info
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Keymap as Keymap
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap

import qualified Local.KeyLayout
import qualified Ui.Color as Color
import qualified Ui.Key as Key
import qualified Ui.Keycaps as Keycaps
import qualified Ui.KeycapsT as KeycapsT

import           Global



open :: Cmd.M m => m ()
open = request (Just (Cmd.KeycapsOpen (200, 200) layout)) layout bindings
    where bindings = Map.findWithDefault mempty Set.empty global_bindings
    -- TODO put it in the bottom right corner, to minimize conflict with
    -- ViewConfig.horizontal_tile.

close :: Cmd.M m => m ()
close = request (Just Cmd.KeycapsClose) layout mempty

update :: Cmd.M m => Map Cmd.Modifier Cmd.Modifier -> m ()
update old_keys_down = whenJustM (Cmd.gets Cmd.state_keycaps) $ \_layout -> do
    -- TODO technically I should use the same layout, but since I only have one
    -- layout I know it can't have changed, which means I can cache everything
    -- in global_bindings.
    kmods <- Cmd.gets (key_mods . Cmd.state_keys_down)
    when (key_mods old_keys_down /= kmods) $ do
        request Nothing layout =<< get_bindings kmods

get_bindings :: Cmd.M m => Set Cmd.Modifier -> m KeycapsT.Bindings
get_bindings kmods = do
    -- TODO this duplicates the work in Cmd.Track
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.abort_unless =<< Cmd.get_insert_tracknum
    track_type <- fmap Info.track_type $
        Cmd.abort_unless =<< Info.lookup_track_type block_id tracknum
    return $ Map.unions
        [ Map.findWithDefault mempty kmods global_bindings
        , case track_type of
            Info.Note {} -> Map.findWithDefault mempty kmods note_track_bindings
            _ -> mempty
        ]

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
        Map.fromList $ map (second binding) $ get_commands kmods keymap
    binding doc = KeycapsT.Binding
        { b_color = mb_color
        , b_text = abbreviate doc
        , b_doc = doc
        }

get_commands :: Set Cmd.Modifier -> Cmd.Keymap m
    -> [(KeycapsT.KeyDoc, KeycapsT.Doc)]
get_commands mods =
    map (bimap to_logical (\(Cmd.CmdSpec doc _) -> doc))
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

request :: Cmd.M m => Maybe Cmd.KeycapsWindow -> KeycapsT.Layout
    -> KeycapsT.Bindings -> m ()
request window layout bindings = Cmd.modify $ \st -> st
    { Cmd.state_keycaps_update = Just $
        Cmd.KeycapsUpdate window (Keycaps.resolve_bindings layout bindings)
    }
