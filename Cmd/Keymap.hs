{- | Support for efficient keymaps.

    The sequece of Cmds which return Continue or Done is flexible, but probably
    inefficient in the presence of hundreds of commands.  In addition, it can't
    warn about Cmds that respond to overlapping Msgs, i.e. respond to the same
    key.

    Keymaps provide an efficient way to respond to a useful subset of Msgs,
    i.e.  those which are considered "key down" msgs or "controller change"
    type msgs.  The exact definition is in 'Cmd.Modifier'.
-}
module Cmd.Keymap where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


-- * building

-- | Simple cmd with no modifiers.
bind_key :: Key.Key -> String -> Cmd.CmdM m -> [Binding m]
bind_key = bind_mod []

-- | Bind a key with the given modifiers.
bind_mod :: [SimpleMod] -> Key.Key -> String -> Cmd.CmdM m -> [Binding m]
bind_mod smods key desc cmd = bind smods (Cmd.KeyMod key) desc (const cmd)

-- | 'bind_mouse' passes the Msg to the cmd, since mouse cmds are more likely
-- to want the msg to find out exactly where the click was.
bind_mouse :: [SimpleMod] -> UiMsg.MouseButton -> String
    -> (Msg.Msg -> Cmd.CmdM m) -> [Binding m]
bind_mouse smods btn desc cmd =
    bind smods (Cmd.MouseMod btn Nothing) desc cmd

-- | Bind a key with the given modifiers.
bind :: [SimpleMod] -> Cmd.Modifier -> String
    -> (Msg.Msg -> Cmd.CmdM m) -> [Binding m]
bind smods key desc cmd =
    [(key_spec mods key, cspec desc cmd) | mods <- all_mods]
    where
    all_mods = if null smods then [[]]
        else Seq.cartesian (map simple_to_mods smods)

-- ** CmdMap

-- | Create a CmdMap for efficient lookup and return warnings encountered
-- during construction.
make_cmd_map :: (Monad m) => [Binding m] -> (CmdMap m, [String])
make_cmd_map bindings = (Map.fromList bindings, warns)
    where
    warns = ["cmds overlap, picking the first one: " ++ Seq.join ", " cmds
        | cmds <- overlaps bindings]

-- | Create a cmd that dispatches into the given CmdMap.
--
-- To lookup a cmd, the Msg is restricted to a Cmd.Modifier.  Then non-modifiers
-- are stripped out of the mods and both are looked up in the keymap.  That
-- way, if keys overlap when you strike them the cmd still fires.
make_cmd :: (Monad m) => CmdMap m -> Msg.Msg -> Cmd.CmdM m
make_cmd cmd_map msg = do
    key <- Cmd.require (msg_to_mod msg)
    mods <- mods_down
    case Map.lookup (KeySpec mods key) cmd_map of
        Nothing -> do
            -- Log.notice $ "no match for " ++ show (KeySpec mods key)
            --     ++ " in " ++ show (Map.keys cmd_map)
            return Cmd.Continue
        Just (CmdSpec name cmd) -> do
            Log.notice $ "running command " ++ show name
            cmd msg
            -- TODO move quit back into its own cmd and turn this on
            -- return Cmd.Done


-- | The Msg contains the low level key information, but most commands should
-- probably use these higher level modifiers.  That way left and right shifts
-- work the same, and cmds can use Command as customary on the Mac and Control
-- as customary on linux.
--
-- Things you have to inspect the Msg directly for:
-- - differentiate ShiftL and ShiftR
-- - chorded keys
-- - use option on the mac
data SimpleMod =
    Shift
    -- | Primary command key: command on mac, control on linux and windows
    -- This should be used for core and global commands.
    | PrimaryCommand
    -- | Secondary comamnd key: control or option on mac, alt on linux and
    -- windows.  I'm not sure what this should be used for, but it should be
    -- different than Mod1 stuff.  Maybe static config user-added commands.
    | SecondaryCommand
    -- | Having mouse here allows for fancy chording.
    | Mouse UiMsg.MouseButton
    deriving (Eq, Ord, Show)

-- * implementation

-- | TODO This is a hardcoded mac layout, when I support other platforms
-- it'll have to be configurable.
simple_mod_map :: [(SimpleMod, [Key.Key])]
simple_mod_map =
    [ (Shift, [Key.ShiftL, Key.ShiftR])
    , (PrimaryCommand, [Key.MetaL, Key.MetaR])
    , (SecondaryCommand, [Key.ControlL, Key.ControlR])
    ]

simple_to_mods :: SimpleMod -> [Cmd.Modifier]
simple_to_mods (Mouse btn) = [Cmd.MouseMod btn Nothing]
simple_to_mods simple = maybe [] (map Cmd.KeyMod) (lookup simple simple_mod_map)

-- ** Binding

type Binding m = (KeySpec, CmdSpec m)

data KeySpec = KeySpec (Set.Set Cmd.Modifier) Cmd.Modifier
    deriving (Eq, Ord, Show)

key_spec :: [Cmd.Modifier] -> Cmd.Modifier -> KeySpec
key_spec mods key = KeySpec (Set.fromList mods) key

-- | Pair a Cmd with a descriptive string that can be used for logging, undo,
-- etc.
data CmdSpec m = CmdSpec String (Msg.Msg -> Cmd.CmdM m)

cspec :: String -> (Msg.Msg -> Cmd.CmdM m) -> CmdSpec m
cspec = CmdSpec

-- | Make a CmdSpec for a CmdM, i.e. a Cmd that doesn't take a Msg.
cspec_ :: String -> Cmd.CmdM m -> CmdSpec m
cspec_ desc cmd = CmdSpec desc (const cmd)

-- ** CmdMap

type CmdMap m = Map.Map KeySpec (CmdSpec m)

overlaps :: [Binding m] -> [[String]]
overlaps bindings =
    [map cmd_name grp | grp <- Seq.group_with fst bindings, length grp > 1]
    where cmd_name (kspec, CmdSpec name _) = show kspec ++ ": " ++ name

-- | Return the mods currently down, stripping out non-modifier keys and notes,
-- so that overlapping keys will still match.  Mouse mods are not filtered, so
-- each mouse chord can be bound individually.
mods_down :: (Monad m) => Cmd.CmdT m (Set.Set Cmd.Modifier)
mods_down = do
    mods <- fmap (filter is_mod . Map.keys) Cmd.keys_down
    return $ Set.fromList mods
    where
    is_mod (Cmd.KeyMod key) = Set.member key Key.modifiers
    is_mod (Cmd.MidiMod _ _) = False
    is_mod (Cmd.MouseMod _ _) = True

msg_to_mod :: Msg.Msg -> Maybe Cmd.Modifier
msg_to_mod msg = case Cmd.msg_to_mod msg of
    Nothing -> Nothing
    Just (False, _) -> Nothing
    Just (True, m) -> Just (Cmd.strip_modifier m)


-- * key layout

-- This way I can set up the mapping relative to key position and have it come
-- out right for both qwerty and dvorak.  It makes the overlapping-ness of
-- non-mapped keys hard to predict though.

qwerty = "1234567890-="
    ++ "qwertyuiop[]\\"
    ++ "asdfghjkl;'"
    ++ "zxcvbnm,./"

dvorak = "1234567890-="
    ++ "',.pyfgcrl[]\\"
    ++ "aoeuidhtns/"
    ++ ";qjkxbmwvz"

qwerty_to_dvorak = Map.fromList (zip qwerty dvorak)
-- TODO presumably this should eventually be easier to change
hardcoded_kbd_layout :: Map.Map Char Char
hardcoded_kbd_layout = qwerty_to_dvorak
