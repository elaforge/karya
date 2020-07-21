-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Manage the optional keycaps window.
module Cmd.SyncKeycaps (open, close, update) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.KeyLayouts as KeyLayouts
import qualified Cmd.Keymap as Keymap

import qualified Local.KeyLayout
import qualified Ui.Key as Key
import qualified Ui.Keycaps as Keycaps
import qualified Ui.KeycapsT as KeycapsT

import           Global



open :: Cmd.M m => m ()
open = request (Just (Cmd.KeycapsOpen (200, 200) layout)) bindings
    where bindings = Map.findWithDefault [] Set.empty mods_to_bindings
    -- TODO put it in the bottom right corner, to minimize conflict with
    -- ViewConfig.horizontal_tile.

close :: Cmd.M m => m ()
close = request (Just Cmd.KeycapsClose) []

update :: Cmd.M m => Map Cmd.Modifier Cmd.Modifier -> m ()
update old_keys_down = whenJustM (Cmd.gets Cmd.state_keycaps) $ \_layout -> do
    -- TODO technically I should use the same layout, but since I only have one
    -- layout I know it can't have changed, which means I can cache everything
    -- in mods_to_bindings.
    kmods <- Cmd.gets (key_mods . Cmd.state_keys_down)
    when (key_mods old_keys_down /= kmods) $
        whenJust (Map.lookup kmods mods_to_bindings) $ \bindings ->
            request Nothing bindings

key_mods :: Map Cmd.Modifier Cmd.Modifier -> Set Cmd.Modifier
key_mods keys_down =
    Set.fromList [Cmd.KeyMod m | Cmd.KeyMod m <- Map.keys keys_down]

layout :: KeycapsT.Layout
layout = Keycaps.make_layout Keycaps.mac_labels

mods_to_bindings :: Map (Set Cmd.Modifier) [KeycapsT.Binding]
mods_to_bindings = Map.fromList $ Seq.key_on_snd get_bindings $
    map (Set.fromList . mapMaybe Keymap.simple_to_mod)
    [ []
    , [Keymap.Shift]
    , [Keymap.PrimaryCommand]
    , [Keymap.Shift, Keymap.PrimaryCommand]
    , [Keymap.SecondaryCommand]
    , [Keymap.Shift, Keymap.SecondaryCommand]
    ]

get_bindings :: Set Cmd.Modifier -> [KeycapsT.Binding]
get_bindings kmods = Keycaps.make_bindings layout $ Map.fromList
    [ (key, (abbreviate doc, doc))
    | (key, doc) <- get_commands kmods
    ]

get_commands :: Set Cmd.Modifier -> [(KeycapsT.KeyDoc, KeycapsT.Doc)]
get_commands mods =
    map (bimap to_logical (\(Keymap.CmdSpec doc _) -> doc)) $
    Seq.map_maybe_fst (key_spec_label mods) $
    Map.toList GlobalKeymap.all_cmd_map

to_logical :: KeycapsT.Keycap -> KeycapsT.Keycap
to_logical label = case Text.unpack label of
    [c] | Just c2 <- KeyLayouts.to_qwerty Local.KeyLayout.layout c ->
        Text.singleton c2
    _ -> label

abbreviate :: Text -> KeycapsT.KeyDoc
abbreviate = Text.pack . map Text.head . take 3 . Text.words

key_spec_label :: Set Cmd.Modifier -> Keymap.KeySpec -> Maybe KeycapsT.Keycap
key_spec_label mods (Keymap.KeySpec smods bindable) | mods == smods =
    case bindable of
        Keymap.Key _repeat key -> Just $ Key.to_mac_label key
        _ -> Nothing
key_spec_label _ _ = Nothing

request :: Cmd.M m => Maybe Cmd.KeycapsWindow -> [KeycapsT.Binding] -> m ()
request window bindings = Cmd.modify $ \st -> st
    { Cmd.state_keycaps_update = Just $
        Cmd.KeycapsUpdate window (KeycapsT.Bindings bindings)
    }
