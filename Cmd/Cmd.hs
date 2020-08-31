-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{- | Core CmdT monad that cmds run in.

    A Cmd is what user actions turn into.  The main thing they do is edit
    'Ui.State', or Cmd.'State', but a special subset can also do IO
    actions like saving and loading files.

    The Cmd monad has two kinds of exception: abort or throw.  Abort means
    that the Cmd decided that it's not the proper Cmd for this Msg (keystroke,
    mouse movement, whatever) and another Cmd should get a crack at it.  Throw
    means that the Cmd failed and there is nothing to be done but log an error.
    When an exception is thrown, the ui and cmd states are rolled back and midi
    output is discarded.

    Cmds should be in the monad @Cmd.M m => m ...@.

    They have to be polymorphic because they run in both IO and Identity.  IO
    because some cmds such saving and loading files require IO, and Identity
    because the majority of cmds don't.  REPL cmds run in IO so they can load
    and save, and the result is that any cmd that wants to be used from both
    Identity cmds (bound to keystrokes) and the REPL must be polymorphic in the
    monad.

    Previously I used @M@ instead of @Monad m => CmdT m ...@ to establish
    Functor, but post-AMP I don't need that any more.  But to maintain
    consistency I'll keep using @M@.
-}
module Cmd.Cmd (
    module Cmd.Cmd, Performance(..)
) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as MonadState
import qualified Control.Monad.Trans as Trans

import qualified Data.Hashable as Hashable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Sound.OSC as OSC
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Util.CallStack as CallStack
import qualified Util.File as File
import qualified Util.GitTypes as GitTypes
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import           Cmd.Msg (Performance(..)) -- avoid a circular import
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.SaveGitTypes as SaveGitTypes
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import qualified Midi.Interface as Interface
import qualified Midi.Interface
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc
import qualified Midi.State

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Config as Shared.Config
import qualified Synth.Shared.Osc as Shared.Osc
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.KeycapsT as KeycapsT
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import           Global
import           Types


-- * Handler

-- | This is the toplevel object representing a cmd.  Fundamentally it's
-- just Msg -> Status, but it's also wrapped up in some documentation,
-- so cmds can be introspected.
data Handler m =
    Keymap !(Keymap m)
    | Handler !(Maybe (NoteEntryMap KeycapsT.KeyDoc)) !(NamedCmd m)
type HandlerId = Handler CmdId

handler :: Text -> (Msg.Msg -> m Status) -> Handler m
handler name cmd = Handler Nothing (NamedCmd name cmd)

call :: M m => Handler m -> Msg.Msg -> m Status
call handler = \msg ->
    let run (NamedCmd n cmd) = do
            Log.debug $ "running command: " <> n
            name n (cmd msg)
    in case handler of
        Handler _ cmd -> run cmd
        Keymap keymap -> do
            bindable <- abort_unless (msg_to_bindable msg)
            mods <- mods_down
            maybe (return Continue) run $
                Map.lookup (KeySpec mods bindable) keymap

-- | Return the mods currently down, stripping out non-modifier keys and notes,
-- so that overlapping keys will still match.  Mouse mods are not filtered, so
-- each mouse chord can be bound individually.
mods_down :: M m => m (Set Modifier)
mods_down = Set.fromList <$> fmap (filter is_mod . Map.keys) keys_down
    where
    is_mod (KeyMod {}) = True
    is_mod (MidiMod {}) = False
    is_mod (MouseMod {}) = True

-- | Pair a Cmd with a Doc that can be used for logging, undo, etc.
data NamedCmd m = NamedCmd {
    cmd_name :: !Text
    , cmd_call :: !(Msg.Msg -> m Status)
    }

-- | NoteEntry might depend on base octave, and might have different
-- mappings for unshifted or shifted.
data NoteEntryMap a =
    WithOctave (Map Pitch.Octave (Map Char a))
    | WithoutOctave (Map Char a)
    deriving (Show, Functor)

note_entry_lookup :: Pitch.Octave -> Char -> NoteEntryMap a -> Maybe a
note_entry_lookup octave char = \case
    WithOctave m -> Map.lookup char =<< Map.lookup octave m
    WithoutOctave m -> Map.lookup char m

-- ** Keymap

type Keymap m = Map KeySpec (NamedCmd m)

data KeySpec = KeySpec !(Set Modifier) !Bindable
    deriving (Eq, Ord, Show)

data Bindable =
    -- | Key IsRepeat Key
    Key Bool Key.Key
    -- | Click MouseButton Clicks
    | Click Types.MouseButton MouseOn Int
    | Drag Types.MouseButton MouseOn
    -- | Mouse button release.
    | Release Types.MouseButton MouseOn
    -- | Channel can be used to restrict bindings to a certain keyboard.  This
    -- should probably be something more abstract though, such as a device
    -- which can be set by the static config.
    | Note Midi.Channel Midi.Key
    deriving (Eq, Ord, Show)

-- | Where a click or drag occurred.
data MouseOn = OnTrack | OnDivider | OnSkeleton | Elsewhere
    deriving (Eq, Ord, Show)

msg_to_bindable :: Msg.Msg -> Maybe Bindable
msg_to_bindable msg = case msg of
    (get_key -> Just (is_repeat, key)) -> Just $ Key is_repeat key
    (Msg.mouse -> Just mouse) -> case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn ->
            Just $ Click btn on (UiMsg.mouse_clicks mouse)
        UiMsg.MouseDrag btn -> Just $ Drag btn on
        UiMsg.MouseUp btn -> Just $ Release btn on
        _ -> Nothing
    (Msg.midi -> Just (Midi.ChannelMessage chan (Midi.NoteOn key _))) ->
        Just $ Note chan key
    _ -> Nothing
    where
    on = maybe Elsewhere mouse_on (Msg.context msg)
    get_key msg = case Msg.key msg of
        Just (UiMsg.KeyDown, k) -> Just (False, k)
        Just (UiMsg.KeyRepeat, k) -> Just (True, k)
        _ -> Nothing

mouse_on :: UiMsg.Context -> MouseOn
mouse_on = maybe Elsewhere on . UiMsg.ctx_track
    where
    on (_, UiMsg.Track {}) = OnTrack
    on (_, UiMsg.Divider) = OnDivider
    on (_, UiMsg.SkeletonDisplay) = OnSkeleton

-- ** pretty instances

instance Pretty (Handler m) where
    format = \case
        Handler _ cmd -> Pretty.format cmd
        Keymap keymap -> Pretty.format keymap

instance Pretty (NamedCmd m) where
    pretty (NamedCmd name _) = "cmd:" <> name

instance Pretty KeySpec where
    pretty (KeySpec mods bindable) =
        Seq.join2 " " (show_mods mods) (show_bindable True bindable)
        where show_mods = Text.intercalate " + " . map show_mod . Set.toList

show_mod :: Modifier -> Text
show_mod m = case m of
    -- TODO this is only true on OS X
    KeyMod mod -> Key.show_mac_mod mod
    MouseMod button _ -> "mouse " <> showt button
    MidiMod chan key -> "midi " <> showt key <> " chan " <> showt chan

instance Pretty Bindable where
    pretty = show_bindable True

show_bindable :: Bool -> Bindable -> Text
show_bindable show_repeatable b = case b of
    Key is_repeat key -> pretty key
        <> if show_repeatable && is_repeat then " (repeatable)" else ""
    Click button on times -> click_times times <> "click "
        <> showt button <> " on " <> pretty on
    Drag button on -> "drag " <> showt button <> " on " <> pretty on
    Release button on -> "release " <> showt button <> " on " <> pretty on
    Note chan key -> "midi " <> showt key <> " channel " <> showt chan
    where
    click_times 0 = ""
    click_times 1 = "double-"
    click_times 2 = "triple-"
    click_times n = showt n <> "-"

instance Pretty MouseOn where
    pretty OnTrack = "track"
    pretty OnDivider = "divider"
    pretty OnSkeleton = "skeleton"
    pretty Elsewhere = "elsewhere"


-- * run CmdT

type CmdId = CmdT Identity.Identity

-- | Cmds used by the REPL, which all run in IO.
type CmdL a = CmdT IO a

data Status =
    -- | Continue processing, so another Cmd will have an opportunity to see
    -- the Msg.
    Continue
    -- | Stop further cmd processing, \"consuming\" the Msg.
    | Done
    -- | Hack to control import dependencies, see "Cmd.PlayC".
    | PlayMidi !PlayMidiArgs
    -- | Open a FloatingInput box.
    | FloatingInput !FloatingInput
    -- | Pack it up and go home.
    | Quit
    deriving (Show)

-- | Combine two Statuses by keeping the one with higher priority.
-- Yes, Status could be a Monoid but merge should be a rare operation.
merge_status :: Status -> Status -> Status
merge_status s1 s2 = if prio s1 >= prio s2 then s1 else s2
    where
    prio status = case status of
        Continue -> 0
        Done -> 1
        PlayMidi {} -> 2
        FloatingInput {} -> 3
        Quit -> 4

-- | Arguments for "Cmd.PlayC.play".  This is a special return value to trigger
-- a play, see "Cmd.PlayC" for details.
data PlayMidiArgs = PlayMidiArgs {
    play_sync :: !(Maybe SyncConfig)
    -- | Description of what is being played for logging.
    , play_name :: !Text
    , play_midi :: !Midi.Perform.MidiEvents
    , play_inv_tempo :: !(Maybe Transport.InverseTempoFunction)
    , play_repeat_at :: !(Maybe RealTime)
    -- | If there are im notes, this is the end of the last one.  This is so
    -- the play monitor thread knows when im will be done.
    , play_im_end :: !(Maybe RealTime)
    }
instance Show PlayMidiArgs where show _ = "((PlayMidiArgs))"

data FloatingInput =
    -- | Open a new floating text input.
    -- View, track, pos, (select start, select end).
    FloatingOpen !ViewId !TrackNum !ScoreTime !Text !(Int, Int)
    -- | Insert the given text into an already open edit box.
    | FloatingInsert !Text
    deriving (Show)

-- | Cmds can run in either Identity or IO, but are generally returned in IO,
-- just to make things uniform.
type RunCmd cmd_m val_m a =
    Ui.State -> State -> CmdT cmd_m a -> val_m (Result a)

-- | The result of running a Cmd.
type Result a =
    ( State
    , [Thru]
    , [Log.Msg]
    , Either Ui.Error (a, Ui.State, Update.UiDamage)
    )

run :: Monad m => a -> RunCmd m m a
run abort_val ustate cstate cmd = do
    (((ui_result, cstate2), midi), logs) <-
        (Log.run . Logger.run . flip MonadState.runStateT cstate
            . Ui.run ustate . (\(CmdT m) -> m))
        cmd
    -- Any kind of error rolls back state and discards midi, but not log msgs.
    -- Normally 'abort_val' will be Continue, but obviously if 'cmd' doesn't
    -- return Status it can't be.
    return $ case ui_result of
        Left Ui.Abort -> (cstate, [], logs, Right (abort_val, ustate, mempty))
        Left _ -> (cstate, [], logs, ui_result)
        _ -> (cstate2, midi, logs, ui_result)

-- | Like 'run', but write logs, and discard MIDI thru and updates.
run_ :: Monad m => Ui.State -> State -> CmdT m a
    -> m (Either String (a, State, Ui.State), [Log.Msg])
run_ ui_state cmd_state cmd = do
    (cmd_state, _thru, logs, result) <-
        run Nothing ui_state cmd_state (liftM Just cmd)
    return $ (, logs) $ case result of
        Left err -> Left $ prettys err
        Right (val, ui_state, _damage) -> case val of
            Nothing -> Left "aborted"
            Just v -> Right (v, cmd_state, ui_state)

-- | Like 'run_', but discard all the final states.
eval :: Monad m => Ui.State -> State -> CmdT m a
    -> m (Either String a, [Log.Msg])
eval ui_state cmd_state = fmap (first (second val_of)) . run_ ui_state cmd_state
    where
    val_of (a, _, _) = a

-- | Run the given command in Identity, but return it in IO, just as
-- a convenient way to have a uniform return type with 'run' (provided it is
-- run in IO).
run_id_io :: RunCmd Identity.Identity IO Status
run_id_io ui_state cmd_state cmd =
    return $ Identity.runIdentity (run Continue ui_state cmd_state cmd)

run_io :: RunCmd IO IO Status
run_io = run Continue

-- | Promote a CmdId to a generic cmd, which can also run as a CmdT IO.
-- TODO: shouldn't it be possible to do this for free?
lift_id :: M m => CmdId a -> m a
lift_id cmd = do
    (cmd_state, thru, logs, result) <- run_id <$> Ui.get <*> get <*> pure cmd
    mapM_ Log.write logs
    case result of
        Left err -> Ui.throw_error err
        Right (val, ui_state, damage) -> case val of
            Nothing -> abort
            Just val -> do
                put cmd_state
                mapM_ write_thru thru
                Ui.damage damage
                Ui.unsafe_put ui_state
                return val

-- | Run the Cmd in Identity, returning Nothing if it aborted.
run_id :: Ui.State -> State -> CmdT Identity.Identity a -> Result (Maybe a)
run_id ui_state cmd_state cmd =
    Identity.runIdentity (run Nothing ui_state cmd_state (fmap Just cmd))

-- | Run a set of Cmds as a single Cmd.  The first one to return non-Continue
-- will return.  Cmds can use this to dispatch to other Cmds.
sequence_cmds :: M m => [a -> m Status] -> a -> m Status
sequence_cmds [] _ = return Continue
sequence_cmds (cmd:cmds) msg = do
    status <- catch_abort (cmd msg)
    case status of
        Nothing -> sequence_cmds cmds msg
        Just Continue -> sequence_cmds cmds msg
        Just status -> return status

-- * CmdT and operations

type CmdStack m = Ui.StateT
    (MonadState.StateT State
        (Logger.LoggerT Thru
            (Log.LogT m)))

newtype CmdT m a = CmdT (CmdStack m a)
    deriving (Functor, Monad, MonadIO, Except.MonadError Ui.Error, Applicative)

class (Log.LogMonad m, Ui.M m) => M m where
    -- Not in MonadState for the same reasons as 'Ui.Ui.M'.
    get :: m State
    put :: State -> m ()
    -- | Log a note to send out.  This is the midi or im thru mechanism.
    write_thru :: Thru -> m ()
    -- | An abort is an exception to get out of CmdT, but it's considered the
    -- same as returning Continue.  It's so a command can back out if e.g. it's
    -- selected by the 'Keymap' but has an additional prerequisite such as
    -- having an active block.
    abort :: m a
    catch_abort :: m a -> m (Maybe a)

instance (Applicative m, Monad m) => M (CmdT m) where
    get = (CmdT . lift) MonadState.get
    put st = (CmdT . lift) (MonadState.put st)
    write_thru msg = (CmdT . lift . lift) (Logger.log msg)
    abort = Except.throwError Ui.Abort
    catch_abort m = Except.catchError (fmap Just m) catch
        where
        catch Ui.Abort = return Nothing
        catch err = Except.throwError err

data Thru =
    -- | Send MIDI thru.  You can give it a timestamp, but it should be 0 for
    -- thru, which will cause it to go straight to the front of the queue.  Use
    -- 'midi' for normal midi thru.
    MidiThru !Midi.Interface.Message
    | ImThru !OSC.Message
    deriving (Eq, Show)

midi_thru :: Midi.WriteDevice -> Midi.Message -> Thru
midi_thru dev msg =
    MidiThru $ Midi.Interface.Midi $ Midi.WriteMessage dev 0 msg

midi :: M m => Midi.WriteDevice -> Midi.Message -> m ()
midi dev msg = write_thru $ midi_thru dev msg

-- | For some reason, newtype deriving doesn't work on MonadTrans.
instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift -- whee!!

-- | Give CmdT unlifted access to all the logging functions.
instance Monad m => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . Log.write

-- | And to the UI state operations.
instance Monad m => Ui.M (CmdT m) where
    get = CmdT Ui.get
    unsafe_put st = CmdT (Ui.unsafe_put st)
    damage upd = CmdT (Ui.damage upd)
    get_damage = CmdT Ui.get_damage
    throw_error msg = CmdT (Ui.throw_error msg)

-- ** exceptions

-- | This is the same as Ui.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (CallStack.Stack, M m) => Text -> m a
throw = Ui.throw

-- | Run a subcomputation that is allowed to abort.
ignore_abort :: M m => m a -> m ()
ignore_abort m = void $ catch_abort m

-- | Run an IO action, rethrowing any IO exception as a Cmd exception.
rethrow_io :: IO a -> CmdT IO a
rethrow_io =
    either throw return <=< liftIO . Exception.handle handle . (Right <$>)
    where
    handle :: Exception.SomeException -> IO (Either Text a)
    handle = return . Left . ("io exception: "<>) . showt

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
abort_unless :: M m => Maybe a -> m a
abort_unless = maybe abort return

-- | Throw an exception with the given msg on Nothing.
require :: (CallStack.Stack, M m) => Text -> Maybe a -> m a
require msg = maybe (throw msg) return

require_right :: (CallStack.Stack, M m) => (err -> Text) -> Either err a -> m a
require_right fmt_err = either (throw . fmt_err) return

-- * State

{- | App global state.  Unlike 'Ui.State', this is not saved to disk.
    This is normally modified inside a 'CmdT', which is also a 'State.StateT',
    so it can also use the UI state functions.  If an exception is thrown, both
    this state and the UI state will be rolled back.

    This is kind of an unorganized wodge.  The problem is that since state is
    all centralized in one place, every special snowflake Cmd that needs its
    own bit of state winds up getting its own little knob in here.  On one
    hand, it's non-modular.  On the other hand, it lets me keep an eye on it.

    So far, most Cmds are pretty fundamental, so they more or less deserve
    their spots here.  If it gets out of control, though, I'll have to either
    come up with a clever way of storing typed data where they can't collide,
    say by having a Cmd return a new Cmd and keeping the state trapped inside,
    or a less clever but simpler and easier way like @Map Name Dynamic@.
-}
data State = State {
    state_config :: !Config
    -- | If set, the current 'Ui.State' was loaded from this file.
    -- This is so save can keep saving to the same file.
    , state_save_file :: !(Maybe (Writable, SaveFile))
    , state_saved :: !Saved
    , state_ky_cache :: !(Maybe KyCache)
    -- | Omit the usual derive delay for these blocks, and trigger a derive.
    -- This is set by integration, which modifies a block in response to
    -- another block being derived.  Blocks set to derive immediately are also
    -- considered to have block damage, if they didn't already.  This is
    -- cleared after every cmd.
    , state_derive_immediately :: !(Set BlockId)
    -- | History.
    , state_history :: !History
    , state_history_config :: !HistoryConfig
    , state_history_collect :: !HistoryCollect
    , state_selection_history :: !SelectionHistory

    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position.  The value has complete info.
    , state_keys_down :: !(Map Modifier Modifier)
    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_focused_view :: !(Maybe ViewId)
    -- | This contains a Rect for each screen.  The first one is the default
    -- one, if a default is needed, though normally views should show up next
    -- to other views.
    , state_screens :: ![Rect.Rect]
    -- | Just indicates that the keycaps window is open.  The window is global,
    -- stored in "Ui.PtrMap", so I don't need to store it here.
    , state_keycaps :: !(Maybe KeycapsState)
    , state_keycaps_update :: !(Maybe KeycapsUpdate)

    -- | This is similar to 'Ui.Block.view_status', except that it's global
    -- instead of per-view.  So changes are logged with a special prefix so
    -- logview can catch them.  Really I only need this map to suppress log
    -- spam.
    , state_global_status :: !(Map Text Text)
    , state_play :: !PlayState
    , state_hooks :: !Hooks

    -- External device tracking.
    -- | MIDI state of WriteDevices.
    , state_wdev_state :: !WriteDeviceState

    -- | MIDI state of ReadDevices, including configuration like pitch bend
    -- range.
    , state_rdev_state :: !InputNote.ReadDeviceState
    , state_edit :: !EditState
    -- | The status return for this Cmd.  This is used only by the REPL, since
    -- non-REPL cmds simply return Status as their return value.  REPL cmds
    -- can't do that because they commonly use the return value to return
    -- an interesting String back to the REPL.
    , state_repl_status :: !Status
    , state_debug_ui_msgs :: !Bool
    } deriving (Show)

data SaveFile = SaveState !Path.Canonical | SaveRepo !Path.Canonical
    deriving (Show, Eq)
data Writable = ReadWrite | ReadOnly deriving (Show, Eq)

-- | This tracks how much the score has been saved to disk.
data Saved = Saved {
    _saved_state :: !SavedState
    , _editor_open :: !Bool
    } deriving (Eq, Show)

-- True if state is synced to disk, either because it was just saved and
-- not significantly changed, or because it was just loaded.
data SavedState =
    -- | Just loaded from a file.  This is almost like SavedChanges, except
    -- that it's required so 'Cmd.Internal.sync_status' can tell the difference
    -- between the state changing because it was just loaded (set to
    -- SavedChanges) and changing due to an edit (set to UnsavedChanges).
    JustLoaded
    | UnsavedChanges
    | SavedChanges
    deriving (Eq, Show)

data KeycapsUpdate =
    KeycapsUpdate KeycapsState (Maybe ((Int, Int), KeycapsT.Layout))
        KeycapsT.RawBindings
    | KeycapsClose
    deriving (Show)

-- | The set of things that can affect a keycaps window.  So when this changes,
-- the window has to be updated.  I assume the KeycapsT.Layout is constant, so
-- it's not in here, which allows me to cache global keymaps in CAFs.
data KeycapsState = KeycapsState {
    kc_mods :: Set Modifier
    , kc_octave :: Pitch.Octave
    , kc_is_kbd_entry :: Bool
    , kc_track_type :: Maybe ParseTitle.Type
    , kc_instrument :: Maybe ScoreT.Instrument
    , kc_scale_id :: Maybe Pitch.ScaleId
    } deriving (Show, Eq)

-- | Absolute directory of the save file.
state_save_dir :: State -> Maybe FilePath
state_save_dir state = case state_save_file state of
    Nothing -> Nothing
    Just (_, SaveState fn) -> Just $ FilePath.takeDirectory $ Path.to_path fn
    Just (_, SaveRepo repo) -> Just $ FilePath.takeDirectory $ Path.to_path repo

-- | Unique name for this score, for the global im cache.
score_path :: State -> FilePath
score_path state = case state_save_file state of
    -- #untitled so it's clear where it came from and unlikely to conflict with
    -- a real filename.
    Nothing -> "#untitled"
    Just (_, SaveState fn) -> strip fn
    Just (_, SaveRepo repo) -> strip repo
    where
    strip = Path.drop_prefix (config_save_dir (state_config state))

-- | A loaded and parsed ky file, or an error string.  This also has the files
-- loaded and their timestamps, to detect when one has changed.
data KyCache =
    KyCache !(Either Text (Derive.Builtins, Derive.InstrumentAliases))
        !Fingerprint
    -- | This disables the cache mechanism.  Tests use this to avoid having
    -- to set SaveFile.
    | PermanentKy !(Derive.Builtins, Derive.InstrumentAliases)
    deriving (Show)

-- | Keep track of loaded files and a fingerprint for their contents.  This is
-- used to detect when they should be reloaded.
data Fingerprint = Fingerprint ![FilePath] !Int
    deriving (Eq, Show)

instance Semigroup Fingerprint where
    Fingerprint fnames1 fprint1 <> Fingerprint fnames2 fprint2 =
        Fingerprint (fnames1<>fnames2) (Hashable.hashWithSalt fprint1 fprint2)
instance Monoid Fingerprint where
    mempty = Fingerprint [] 0
    mappend = (<>)
instance Pretty Fingerprint where
    pretty (Fingerprint files word) = pretty files <> ":" <> pretty word

fingerprint :: [(FilePath, Text)] -> Fingerprint
fingerprint files =
    -- The code in 'Ui.ky' gets "" for its filename.
    Fingerprint (filter (not . null) fnames)
        (foldl' Hashable.hashWithSalt 0 contents)
    where (fnames, contents) = unzip files

initial_state :: Config -> State
initial_state config = State
    { state_config = config
    , state_save_file = Nothing
    , state_saved = Saved JustLoaded False
    , state_ky_cache = Nothing
    , state_derive_immediately = Set.empty
    -- This is a dummy entry needed to bootstrap a Cmd.State.  Normally
    -- 'hist_present' should always have the current state, but the initial
    -- setup cmd needs a State too.
    , state_history = initial_history (empty_history_entry Ui.empty)
    , state_history_config = empty_history_config
    , state_history_collect = empty_history_collect
    , state_selection_history = empty_selection_history
    , state_keys_down = Map.empty
    , state_focused_view = Nothing
    , state_screens = []
    , state_keycaps = Nothing
    , state_keycaps_update = Nothing
    , state_global_status = Map.empty
    , state_play = initial_play_state
    , state_hooks = mempty

    , state_wdev_state = empty_wdev_state
    , state_rdev_state = InputNote.empty_rdev_state
    , state_edit = initial_edit_state
    , state_repl_status = Continue
    , state_debug_ui_msgs = False
    }

-- | Reset the parts of the State which are specific to a \"session\".  This
-- should be called whenever an entirely new state is loaded.
reinit_state :: HistoryEntry -> State -> State
reinit_state present cstate = cstate
    { state_history = initial_history present
    -- Performance threads should have been killed by the caller.
    , state_play = initial_play_state
        { state_play_step = state_play_step (state_play cstate) }
    -- This is essential, otherwise lots of cmds break on the bad reference.
    , state_focused_view = Nothing
    , state_edit = initial_edit_state
        { state_time_step = state_time_step (state_edit cstate) }
    }

-- ** Config

-- | Config type variables that change never or rarely.  These mostly come from
-- the "App.StaticConfig".
data Config = Config {
    -- | App root, initialized from 'Config.get_app_dir'.
    config_app_dir :: !Path.AppDir
    , config_save_dir :: !Path.Canonical
    , config_midi_interface :: !Midi.Interface.Interface
    -- | Search path for local definition files, from 'Config.definition_path'.
    , config_ky_paths :: ![FilePath]
    -- | Reroute MIDI inputs and outputs.  These come from
    -- 'App.StaticConfig.rdev_map' and 'App.StaticConfig.wdev_map' and probably
    -- shouldn't be changed at runtime.
    , config_rdev_map :: !(Map Midi.ReadDevice Midi.ReadDevice)
    -- | WriteDevices can be score-specific, though, so another map is kept in
    -- 'Ui.State', which may override the one here.
    , config_wdev_map :: !(Map Midi.WriteDevice Midi.WriteDevice)
    , config_instrument_db :: !InstrumentDb
    -- | Library of calls for the deriver.
    , config_builtins :: !Derive.Builtins
    , config_highlight_colors :: !(Map Color.Highlight Color.Color)
    , config_im :: !Shared.Config.Config
    , config_git_user :: !SaveGit.User
    } deriving (Show)

-- | Get a midi writer that takes the 'config_wdev_map' into account.
state_midi_writer :: State -> Midi.Interface.Message -> IO ()
state_midi_writer state imsg = do
    let out = case imsg of
            Midi.Interface.Midi wmsg -> Midi.Interface.Midi $ map_wdev wmsg
            _ -> imsg
    mb_err <- Midi.Interface.write_message
        (config_midi_interface (state_config state)) out
    whenJust mb_err $ \err ->
        Log.warn $ "error writing " <> pretty out <> ": " <> err
    where
    map_wdev (Midi.WriteMessage wdev time msg) =
        Midi.WriteMessage (lookup_wdev wdev) time msg
    lookup_wdev wdev = Map.findWithDefault wdev wdev
        (config_wdev_map (state_config state))

-- | Convert a relative path to place it in the app dir.
to_absolute :: State -> Path.Relative -> FilePath
to_absolute state = Path.to_absolute (config_app_dir (state_config state))

-- | This was previously in 'Config', and configured via StaticConfig.  But it
-- turns out I don't really use StaticConfig.  It has a name here just so
-- I don't get references to 'Scale.All.lookup_scale' everywhere.
lookup_scale :: Derive.LookupScale
lookup_scale = Scale.All.lookup_scale

-- ** PlayState

-- | State concerning derivation, performance, and playing the performance.
data PlayState = PlayState {
    -- | Transport control channel for the player, if one is running.
    state_play_control :: !(Maybe Transport.PlayControl)
    -- | When changes are made to a block, its performance will be
    -- recalculated in the background.  When the Performance is forced, it will
    -- replace the existing performance in 'state_performance', if any.  This
    -- means there will be a window in which the performance is out of date,
    -- but this is better than hanging the responder every time it touches an
    -- insufficiently lazy part of the performance.
    , state_performance :: !(Map BlockId Performance)
    -- | However, some cmds, like play, want the most up to date performance
    -- even if they have to wait for it.  This map will be updated
    -- immediately.
    , state_current_performance :: !(Map BlockId Performance)
    -- | Keep track of current thread working on each performance.  If a
    -- new performance is needed before the old one is complete, it can be
    -- killed off.
    , state_performance_threads :: !(Map BlockId Thread)
    -- | Some play commands start playing from a short distance before the
    -- cursor.
    , state_play_step :: !TimeStep.TimeStep
    -- | Contain a StepState if step play is active.  Managed in
    -- "Cmd.StepPlay".
    , state_step :: !(Maybe StepState)
    -- | Globally speed up or slow down performance.  It mutiplies the
    -- timestamps by the reciprocal of this amount, so 2 will play double
    -- speed, and 0.5 will play half speed.
    , state_play_multiplier :: RealTime
    -- | If set, synchronize with a DAW when the selection is set, and on play
    -- and stop.
    , state_sync :: !(Maybe SyncConfig)
    } deriving (Show)

-- | Wrap Async to make it showable.  I use Async instead of ThreadId because
-- I want to make sure they can run their finalizers when the app quits, and
-- I can wait on an Async, but not on a ThreadId.  At the moment, the
-- finalizers are killing im subprocesses.
newtype Thread = Thread (Async.Async ())

instance Show Thread where
    show = show . Async.asyncThreadId . u
        where u (Thread t) = t

kill_thread :: Thread -> IO ()
kill_thread (Thread async) = Async.cancel async

kill_performance_threads :: State -> IO ()
kill_performance_threads =
    mapM_ kill_thread . Map.elems . state_performance_threads . state_play

-- | Get currently evaluating root BlockIds.
running_threads :: CmdT IO [BlockId]
running_threads = do
    threads <- gets $ Map.toAscList . state_performance_threads . state_play
    alive <- liftIO $ filterM (is_alive . snd) threads
    return $ map fst alive
    where
    is_alive (Thread async) = Maybe.isNothing <$> Async.poll async

initial_play_state :: PlayState
initial_play_state = PlayState
    { state_play_control = Nothing
    , state_performance = Map.empty
    , state_current_performance = Map.empty
    , state_performance_threads = Map.empty
    , state_play_step =
        TimeStep.time_step $ TimeStep.RelativeMark TimeStep.AllMarklists 0
    , state_step = Nothing
    , state_play_multiplier = RealTime.seconds 1
    , state_sync = Nothing
    }

-- | Step play is a way of playing back the performance in non-realtime.
data StepState = StepState {
    -- - constant
    -- | Keep track of the view step play was started in, so I know where to
    -- display the selection.
    step_view_id :: !ViewId
    -- | If step play only applies to a few tracks, list them.  If null,
    -- step play applies to all tracks.
    , step_tracknums :: [TrackNum]

    -- - modified
    -- | MIDI states before the step play position, in descending order.
    , step_before :: ![(ScoreTime, Midi.State.State)]
    -- | MIDI states after the step play position, in asceding order.
    , step_after :: ![(ScoreTime, Midi.State.State)]
    } deriving (Show)

-- | Configure synchronization.  MMC is used to set the play position and MTC
-- is used to start and stop playing.
--
-- MMC has start and stop msgs, but they seem useless, since they're sysexes,
-- which are not delivered precisely.
data SyncConfig = SyncConfig {
    sync_device :: !Midi.WriteDevice
    -- | Send MMC to this device.
    , sync_device_id :: !Mmc.DeviceId
    -- | If true, send MTC on the 'sync_device'.  If this is set, MMC play and
    -- stop will be omitted, since the presence of MTC should be enough to get
    -- the DAW started, provided it's in external sync mode.
    --
    -- DAWs tend to spend a long time synchronizing, presumably because
    -- hardware devices take time to spin up.  That's unnecessary in software,
    -- so in Cubase you can set \"lock frames\" to 2, and in Reaper you can set
    -- \"synchronize by seeking ahead\" to 67ms.
    , sync_mtc :: !Bool
    , sync_frame_rate :: !Midi.FrameRate
    } deriving (Show)

instance Pretty SyncConfig where
    format (SyncConfig dev dev_id mtc rate) = Pretty.record "SyncConfig"
        [ ("device", Pretty.format dev)
        , ("device_id", Pretty.format dev_id)
        , ("mtc", Pretty.format mtc)
        , ("frame_rate", Pretty.text (showt rate))
        ]

-- ** hooks

-- | Hooks are Cmds that run after some event.
newtype Hooks = Hooks {
    -- | Run when the selection changes.
    hooks_selection :: [[(ViewId, Maybe TrackSelection)] -> CmdId ()]
    }

-- | Just a 'Sel.Selection' annotated with its BlockId and TrackId.  There's
-- no deep reason for it, it just saves a bit of work for selection hooks.
type TrackSelection = (Sel.Selection, BlockId, Maybe TrackId)

instance Show Hooks where
    show (Hooks sel) = "((Hooks " ++ show (length sel) ++ "))"

instance Semigroup Hooks where
    Hooks sel1 <> Hooks sel2 = Hooks (sel1 <> sel2)
instance Monoid Hooks where
    mempty = Hooks []
    mappend = (<>)

-- ** EditState

-- | Editing state, modified in the course of editing.
data EditState = EditState {
    -- | Edit mode enables various commands that write to tracks.
    state_edit_mode :: !EditMode
    -- | True if the floating input edit is open.
    , state_floating_input :: !Bool
    -- | Whether or not to advance the insertion point after a note is
    -- entered.
    , state_advance :: Bool
    -- | Chord mode means the note is considered entered when all NoteOffs
    -- have been received.  While a note is held down, the insertion point will
    -- move to the next note track with the same instrument so you can
    -- enter chords.
    --
    -- When chord mode is off, the note is considered entered as soon as
    -- its NoteOn is received.
    , state_chord :: Bool
    -- | Try to find or create a 'Controls.dynamic' track for to record
    -- 'InputNote.Input' velocity, similar to how a pitch track is edited and
    -- created.
    , state_record_velocity :: Bool
    -- | Use the alphanumeric keys to enter notes in addition to midi input.
    , state_kbd_entry :: !Bool
    -- | Default time step for cursor movement.
    , state_time_step :: !TimeStep.TimeStep
    -- | Used for note duration.  It's separate from 'state_time_step' to
    -- allow for tracker-style note entry where newly entered notes extend to
    -- the next note or the end of the block.
    , state_note_duration :: !TimeStep.TimeStep
    -- | If this is Rewind, create notes with negative durations.
    , state_note_orientation :: !Types.Orientation
    -- | New notes get this text by default.  This way, you can enter a series
    -- of notes with the same attributes, or whatever.
    , state_note_text :: !Text
    -- | Transpose note entry on the keyboard by this many octaves.  It's by
    -- octave instead of scale degree since scales may have different numbers
    -- of notes per octave.
    , state_kbd_entry_octave :: !Pitch.Octave
    , state_recorded_actions :: !RecordedActions
    , state_instrument_attributes :: !(Map ScoreT.Instrument Attrs.Attributes)
    -- | See 'set_edit_box'.
    , state_edit_box :: !(Block.Box, Block.Box)
    } deriving (Eq, Show)

initial_edit_state :: EditState
initial_edit_state = EditState {
    state_edit_mode = NoEdit
    , state_floating_input = False
    , state_kbd_entry = False
    , state_advance = True
    , state_chord = False
    , state_record_velocity = False
    , state_time_step =
        TimeStep.time_step $ TimeStep.AbsoluteMark TimeStep.AllMarklists 0
    , state_note_duration = TimeStep.event_edge
    , state_note_orientation = Types.Positive
    , state_note_text = ""
    -- This should put middle C in the center of the kbd entry keys.
    , state_kbd_entry_octave = 3
    , state_recorded_actions = mempty
    , state_instrument_attributes = mempty
    , state_edit_box = (box, box)
    } where box = uncurry Block.Box Config.bconfig_box

-- | These enable various commands to edit event text.  What exactly val
-- and method mean are dependent on the track.
data EditMode = NoEdit | ValEdit | MethodEdit deriving (Eq, Show)
instance Pretty EditMode where pretty = showt

type RecordedActions = Map Char Action

-- | Repeat a recorded action.
--
-- Select event and duration and hit shift-1 to record InsertEvent.
-- Text edits record ReplaceText, PrependText, or AppendText in the last
-- action field (bound to '.'), which you can then save.
data Action =
    -- | If a duration is given, the event has that duration, otherwise
    -- it gets the current time step.
    InsertEvent !(Maybe TrackTime) !Text
    | ReplaceText !Text | PrependText !Text | AppendText !Text
    deriving (Show, Eq)

instance Pretty Action where
    pretty act = case act of
        InsertEvent maybe_dur text ->
            pretty text <> " (" <> pretty maybe_dur <> ")"
        ReplaceText text -> "=" <> pretty text
        PrependText text -> pretty text <> "+"
        AppendText text -> "+" <> pretty text

-- *** midi devices

data WriteDeviceState = WriteDeviceState {
    -- Used by Cmd.MidiThru:
    -- | NoteId currently playing in each Addr.  An Addr may have >1 NoteId.
    wdev_note_addr :: !(Map InputNote.NoteId Patch.Addr)
    -- | The note id is not guaranteed to have any relationship to the key,
    -- so the MIDI NoteOff needs to know what key the MIDI NoteOn used.
    , wdev_note_key :: !(Map InputNote.NoteId Midi.Key)
    -- | Map an addr to a number that increases when it's assigned a note.
    -- This is used along with 'wdev_serial' to implement addr round-robin.
    , wdev_addr_serial :: !(Map Patch.Addr Serial)
    -- | Next serial number for 'wdev_addr_serial'.
    , wdev_serial :: !Serial
    -- | Last NoteId seen.  This is needed to emit controls (rather than just
    -- mapping them from MIDI input) because otherwise there's no way to know
    -- to which note they should be assigned.
    , wdev_last_note_id :: !(Maybe InputNote.NoteId)

    -- Used by Cmd.PitchTrack:
    -- | NoteIds being entered into which pitch tracks.  When entering a chord,
    -- a PitchChange uses this to know which pitch track to update.
    , wdev_pitch_track :: !(Map InputNote.NoteId (BlockId, TrackNum))

    -- Used by no one, yet:  (TODO should someone use this?)
    -- | Remember the current patch of each addr.  More than one patch or
    -- keyswitch can share the same addr, so I need to keep track which one is
    -- active to minimize switches.
    , wdev_addr_inst :: !(Map Patch.Addr Midi.Types.Patch)
    } deriving (Eq, Show)

type Serial = Int

empty_wdev_state :: WriteDeviceState
empty_wdev_state = WriteDeviceState
    { wdev_note_addr = Map.empty
    , wdev_note_key = Map.empty
    , wdev_addr_serial = Map.empty
    , wdev_serial = 0
    , wdev_last_note_id = Nothing
    , wdev_pitch_track = Map.empty
    , wdev_addr_inst = Map.empty
    }

-- *** performance

perf_tempo :: Performance -> Transport.TempoFunction
perf_tempo = TrackWarp.tempo_func . perf_warps

perf_inv_tempo :: Performance -> Transport.InverseTempoFunction
perf_inv_tempo = TrackWarp.inverse_tempo_func . perf_warps

perf_closest_warp :: Performance -> Transport.ClosestWarpFunction
perf_closest_warp = TrackWarp.closest_warp . perf_warps

-- *** instrument

-- | The code part of an instrument, i.e. the calls and cmds it brings into
-- scope.
--
-- This has to be in Cmd.Cmd for circular import reasons.
data InstrumentCode = InstrumentCode {
    inst_calls :: !Derive.InstrumentCalls
    , inst_postproc :: !InstrumentPostproc
    , inst_cmds :: ![HandlerId]
    -- | An optional specialized cmd to write Thru.  This is separate from
    -- 'inst_cmds' so it can be run wherever the instrument needs special thru,
    -- not just in the instrument's note track.  This way custom thru works in
    -- the pitch track too.
    , inst_thru :: !(Maybe ThruFunction)
    }

type ThruFunction =
    Scale.Scale -> Attrs.Attributes -> InputNote.Input -> CmdId [Thru]

-- | Process each event before conversion.  This is like a postproc call,
-- but it can only map events 1:1 and you don't have to explicitly call it.
--
-- This can change the duration, but should not change 'Score.event_start',
-- because the events are not resorted afterwards.  Also, it's applied during
-- conversion, so it only makes sense to modify 'Score.event_duration',
-- 'Score.event_controls', 'Score.event_pitch', and 'Score.event_environ'.
-- TODO so I could have it return just those?  But then it has to return Maybe
-- to not modify and needs a record type.
type InstrumentPostproc = Score.Event -> (Score.Event, [Log.Msg])

instance Show InstrumentCode where show _ = "((InstrumentCode))"
instance Pretty InstrumentCode where
    format (InstrumentCode calls _ cmds thru) = Pretty.record "InstrumentCode"
        [ ("calls", Pretty.format calls)
        , ("cmds", Pretty.format cmds)
        , ("thru", Pretty.format thru)
        ]

make_derive_instrument :: ResolvedInstrument -> Derive.Instrument
make_derive_instrument resolved = Derive.Instrument
    { inst_calls = inst_calls $ Common.common_code $ Inst.inst_common $
        inst_instrument resolved
    , inst_environ = maybe mempty RestrictedEnviron.convert $
        Common.config_environ $ inst_common_config resolved
    , inst_controls = Common.config_controls (inst_common_config resolved)
    , inst_attributes = Inst.inst_attributes (inst_instrument resolved)
    , inst_elements = case Inst.inst_backend (inst_instrument resolved) of
        Inst.Im patch -> Im.Patch.patch_elements patch
        _ -> mempty
    }

empty_code :: InstrumentCode
empty_code = InstrumentCode
    { inst_calls = mempty
    , inst_postproc = (,[])
    , inst_cmds = []
    , inst_thru = Nothing
    }

-- | Instantiate 'Inst.Db' with the code type.  The only reason the Db has the
-- type parameter is so I can define it in its own module without a circular
-- import.
type InstrumentDb = Inst.Db InstrumentCode
-- | Like 'InstrumentDb'.
type Inst = Inst.Inst InstrumentCode


-- *** history

-- | Ghosts of state past, present, and future.
data History = History {
    hist_past :: ![HistoryEntry]
    -- | The present is actually the immediate past.  When you undo, the
    -- undo itself is actually in the future of the state you want to undo.
    -- So another way of looking at it is that you undo from the past to
    -- a point further in the past.  But since you always require a \"recent
    -- past\" to exist, it's more convenient to break it out and call it the
    -- \"present\".  Isn't time travel confusing?
    , hist_present :: !HistoryEntry
    , hist_future :: ![HistoryEntry]
    , hist_last_cmd :: !(Maybe LastCmd)
    } deriving (Show)

initial_history :: HistoryEntry -> History
initial_history present = History [] present [] Nothing

-- | Record some information about the last cmd for the benefit of
-- 'Cmd.Undo.maintain_history'.
data LastCmd =
    -- | This cmd set the state because it was an undo or redo.  Otherwise undo
    -- and redo themselves would be recorded and multiple undo would be
    -- impossible!
    UndoRedo
    -- | This cmd set the state because of a load.  This should reset all the
    -- history so I can start loading from the new state's history.
    | Load (Maybe GitTypes.Commit) [Text]
    deriving (Show)

data HistoryConfig = HistoryConfig {
    -- | Keep this many previous history entries in memory.
    hist_keep :: !Int
    -- | Checkpoints are saved relative to the state at the next checkpoint.
    -- So it's important to keep the commit of that checkpoint up to date,
    -- otherwise the state and the checkpoints will get out of sync.
    , hist_last_commit :: !(Maybe GitTypes.Commit)
    } deriving (Show)

empty_history_config :: HistoryConfig
empty_history_config = HistoryConfig Config.default_keep_history Nothing

data HistoryCollect = HistoryCollect {
    -- | This is cleared after each cmd.  A cmd can cons its name on, and
    -- the cmd is recorded with the (optional) set of names it returns.
    -- Hopefully each cmd has at least one name, since this makes the history
    -- more readable.  There can be more than one name if the history records
    -- several cmds or if one cmd calls another.
    state_cmd_names :: ![Text]
    -- | Suppress history record until the EditMode changes from the given one.
    -- This is a bit of a hack so that every keystroke in a raw edit isn't
    -- recorded separately.
    , state_suppress_edit :: !(Maybe EditMode)
    -- | The Git.Commit in the SaveHistory should definitely be Nothing.
    , state_suppressed :: !(Maybe SaveGitTypes.SaveHistory)
    } deriving (Show)

empty_history_collect :: HistoryCollect
empty_history_collect = HistoryCollect
    { state_cmd_names = []
    , state_suppress_edit = Nothing
    , state_suppressed = Nothing
    }

data HistoryEntry = HistoryEntry {
    hist_state :: !Ui.State
    -- | Since track event updates are not caught by diff but recorded by
    -- Ui.State, I have to save those too, or else an undo or redo will miss
    -- the event changes.  TODO ugly, can I avoid this?
    --
    -- If this HistoryEntry is in the past, these are the updates that took it
    -- to the future, not the updates emitted by the cmd itself.  If the
    -- HistoryEntry is in the future, the updates take it to the past, which
    -- are the updated emitted by the cmd.  So don't be confused if it looks
    -- like a HistoryEntry has the wrong updates.
    , hist_damage :: !Update.UiDamage
    -- | Cmds involved creating this entry.
    , hist_names :: ![Text]
    -- | The Commit where this entry was saved.  Nothing if the entry is
    -- unsaved.
    , hist_commit :: !(Maybe GitTypes.Commit)
    } deriving (Show)

empty_history_entry :: Ui.State -> HistoryEntry
empty_history_entry state = HistoryEntry
    { hist_state = state
    , hist_damage = mempty
    , hist_names = []
    , hist_commit = Nothing
    }

instance Pretty History where
    format (History past present future last_cmd) = Pretty.record "History"
        [ ("past", Pretty.format past)
        , ("present", Pretty.format present)
        , ("future", Pretty.format future)
        , ("last_cmd", Pretty.text (showt last_cmd))
        ]

instance Pretty HistoryEntry where
    format (HistoryEntry _state damage commands commit) =
        Pretty.format commit Pretty.<+> Pretty.textList commands
        Pretty.<+> Pretty.format damage

instance Pretty HistoryConfig where
    format (HistoryConfig keep last_commit) = Pretty.record "HistoryConfig"
        [ ("keep", Pretty.format keep)
        , ("last_commit", Pretty.format last_commit)
        ]

instance Pretty HistoryCollect where
    format (HistoryCollect names edit suppressed) =
        Pretty.record "HistoryCollect"
            [ ("names", Pretty.format names)
            , ("suppress_edit", Pretty.format edit)
            , ("suppressed", Pretty.format suppressed)
            ]

-- *** SelectionHistory

-- | Remember previous selections.  This should be updated only by significant
-- movements, so clicks and cmd-a, but not hjkl stuff.
data SelectionHistory = SelectionHistory {
    sel_past :: [(ViewId, Sel.Selection)]
    , sel_future :: [(ViewId, Sel.Selection)]
    } deriving (Eq, Show)

empty_selection_history :: SelectionHistory
empty_selection_history = SelectionHistory [] []

instance Pretty SelectionHistory where
    format (SelectionHistory past future) = Pretty.record "SelectionHistory"
        [ ("past", Pretty.format past)
        , ("future", Pretty.format future)
        ]

-- *** modifier

data Modifier = KeyMod Key.Modifier
    -- | Mouse button, and track it went down at, if any.  The block is not
    -- recorded.  You can't drag across blocks so you know any click must
    -- apply to the focused block.
    | MouseMod Types.MouseButton (Maybe (TrackNum, UiMsg.Track))
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the device, this code doesn't know which devices are available.
    -- Block or track level handlers can query the device themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)

instance Pretty Modifier where
    pretty = \case
        KeyMod mod -> pretty mod
        MouseMod button mb_track -> "MouseMod" <> pretty (button, mb_track)
        MidiMod chan key -> "MidiMod" <> pretty (chan, key)

mouse_mod_btn :: Modifier -> Maybe Types.MouseButton
mouse_mod_btn (MouseMod btn _) = Just btn
mouse_mod_btn _ = Nothing


-- | Take a modifier to its key in the modifier map which has extra info like
-- mouse down position stripped.
strip_modifier :: Modifier -> Modifier
strip_modifier (MouseMod btn _) = MouseMod btn Nothing
strip_modifier mod = mod


-- ** state access

gets :: M m => (State -> a) -> m a
gets f = do
    state <- get
    return $! f state

modify :: M m => (State -> State) -> m ()
modify f = do
    st <- get
    put $! f st

modify_play_state :: M m => (PlayState -> PlayState) -> m ()
modify_play_state f = modify $ \st ->
    st { state_play = f (state_play st) }

-- | Return the rect of the screen closest to the given point, or the default.
get_screen :: M m => Maybe (Int, Int) -> m Rect.Rect
get_screen mb_point = do
    screens <- gets state_screens
    -- There are no screens yet during setup, so pick something somewhat
    -- reasonable so windows don't all try to crunch themselves down to
    -- nothing.
    return $ fromMaybe (Rect.xywh 0 0 800 600) $ case mb_point of
        Nothing -> Seq.head screens
        Just point -> Seq.minimum_on (Rect.distance point) screens

lookup_performance :: M m => BlockId -> m (Maybe Performance)
lookup_performance block_id =
    gets $ Map.lookup block_id . state_performance . state_play

get_performance :: M m => BlockId -> m Performance
get_performance block_id = abort_unless =<< lookup_performance block_id

-- | Clear all performances, which will cause them to be rederived.
-- It's in IO because it wants to kill any threads still deriving.
--
-- TODO I'm not actually sure if this is safe.  A stale DeriveComplete
-- coming in should be ignored, right?  Why not Ui.update_all?
invalidate_performances :: CmdT IO ()
invalidate_performances = do
    liftIO . kill_performance_threads =<< get
    modify_play_state $ \state -> state
        { state_performance = mempty
        , state_performance_threads = mempty
        }

clear_im_cache :: BlockId -> CmdT IO ()
clear_im_cache block_id = do
    path <- gets score_path
    liftIO $ do
        config <- Shared.Config.getConfig
        let imDir = Shared.Config.imDir config
        File.ignoreEnoent $ Directory.removeDirectoryRecursive $
            Shared.Config.notesDirectory imDir path block_id
        File.ignoreEnoent $ Directory.removeDirectoryRecursive $
            Shared.Config.outputDirectory imDir path block_id
    return ()

-- | Keys currently held down, as in 'state_keys_down'.
keys_down :: M m => m (Map Modifier Modifier)
keys_down = gets state_keys_down

get_focused_view :: M m => m ViewId
get_focused_view = gets state_focused_view >>= abort_unless

get_focused_block :: M m => m BlockId
get_focused_block = Block.view_block <$> (Ui.get_view =<< get_focused_view)

lookup_focused_view :: M m => m (Maybe ViewId)
lookup_focused_view = gets state_focused_view

-- | Request focus.  'state_focused_view' will be updated once fltk reports the
-- focus change.
focus :: Ui.M m => ViewId -> m ()
focus view_id = do
    view <- Ui.lookup_view view_id
    case view of
        Nothing ->
            Ui.throw $ "Cmd.focus on non-existent view: " <> showt view_id
        _ -> return ()
    Ui.damage $ mempty { Update._bring_to_front = Set.singleton view_id }

-- | In some circumstances I don't want to abort if there's no focused block.
lookup_focused_block :: M m => m (Maybe BlockId)
lookup_focused_block = do
    maybe_view_id <- lookup_focused_view
    case maybe_view_id of
        -- It's still an error if the view id doesn't exist.
        Just view_id -> fmap (Just . Block.view_block) (Ui.get_view view_id)
        Nothing -> return Nothing

get_current_step :: M m => m TimeStep.TimeStep
get_current_step = gets (state_time_step . state_edit)

-- | Get the leftmost track covered by the insert selection, which is
-- considered the "focused" track by convention.
get_insert_tracknum :: M m => m (Maybe TrackNum)
get_insert_tracknum = do
    view_id <- get_focused_view
    sel <- Ui.get_selection view_id Config.insert_selnum
    return (fmap Sel.start_track sel)

-- | This just calls 'Ui.set_view_status', but all status setting should
-- go through here so they can be uniformly filtered or logged or something.
set_view_status :: M m => ViewId -> (Int, Text) -> Maybe Text -> m ()
set_view_status = Ui.set_view_status

-- | Emit a special log msg that will cause log view to put this key and value
-- in its status bar.  A value of \"\" will cause logview to delete that key.
set_global_status :: M m => Text -> Text -> m ()
set_global_status key val = do
    status_map <- gets state_global_status
    when (Map.lookup key status_map /= Just val) $ do
        modify $ \st ->
            st { state_global_status = Map.insert key val status_map }
        Log.debug $ "global status: " <> key <> " -- " <> val

-- | Set a status variable on all views.
set_status :: M m => (Int, Text) -> Maybe Text -> m ()
set_status key val = do
    view_ids <- Ui.gets (Map.keys . Ui.state_views)
    forM_ view_ids $ \view_id -> set_view_status view_id key val

-- ** lookup instrument

-- | This is an instrument as looked up by 'lookup_instrument' or
-- 'get_lookup_instrument'.  This merges compiled-in and runtime instrument
-- data.
data ResolvedInstrument = ResolvedInstrument {
    inst_instrument :: !Inst
    , inst_qualified :: !InstTypes.Qualified
    , inst_common_config :: !Common.Config
    , inst_backend :: !Backend
    } deriving (Show)

inst_synth :: ResolvedInstrument -> InstTypes.SynthName
inst_synth  = InstTypes.synth . inst_qualified

inst_common :: ResolvedInstrument -> Common.Common InstrumentCode
inst_common = Inst.inst_common . inst_instrument

instance Pretty ResolvedInstrument where
    format (ResolvedInstrument instrument qualified common_config backend) =
        Pretty.record "ResolvedInstrument"
            [ ("instrument", Pretty.format instrument)
            , ("qualified", Pretty.format qualified)
            , ("common_config", Pretty.format common_config)
            , ("backend", Pretty.format backend)
            ]

-- | This merges the compiled-id 'Inst.Backend' and the per-score
-- 'UiConfig.Backend'.
data Backend =
    Midi !Midi.Patch.Patch !Midi.Patch.Config
    | Im !Im.Patch.Patch
    | Dummy
    deriving (Show)

instance Pretty Backend where
    format (Midi patch config) = Pretty.format (patch, config)
    format (Im patch) = Pretty.format patch
    format Dummy = "Dummy"

midi_instrument :: ResolvedInstrument -> Maybe (Patch.Patch, Patch.Config)
midi_instrument inst = case inst_backend inst of
    Midi patch config -> Just (patch, config)
    _ -> Nothing

get_midi_instrument :: (CallStack.Stack, M m) => ScoreT.Instrument
    -> m (Patch.Patch, Patch.Config)
get_midi_instrument inst =
    require ("not a midi instrument: " <> pretty inst) . midi_instrument
        =<< get_instrument inst

lookup_midi_config :: M m => ScoreT.Instrument -> m (Maybe Patch.Config)
lookup_midi_config inst = justm (lookup_backend inst) $ \case
    Midi _ config -> return $ Just config
    _ -> return Nothing

lookup_backend :: M m => ScoreT.Instrument -> m (Maybe Backend)
lookup_backend inst = justm (lookup_instrument inst) $
    return . Just . inst_backend

lookup_instrument :: M m => ScoreT.Instrument -> m (Maybe ResolvedInstrument)
lookup_instrument inst = do
    ui_state <- Ui.get
    db <- gets $ config_instrument_db . state_config
    case Ui.allocation inst #$ ui_state of
        Nothing -> return Nothing
        Just alloc -> case resolve_instrument db alloc of
            Left err -> do
                -- This is a broken allocation, so I should log it at least.
                Log.warn $ "lookup " <> pretty inst <> ": " <> err
                return Nothing
            Right val -> return (Just val)

get_instrument :: (CallStack.Stack, M m) => ScoreT.Instrument
    -> m ResolvedInstrument
get_instrument inst = require ("instrument not found: " <> pretty inst)
    =<< lookup_instrument inst

get_lookup_instrument :: M m
    => m (ScoreT.Instrument -> Maybe ResolvedInstrument)
get_lookup_instrument = fmap (either (const Nothing) Just .) $
    state_lookup_instrument <$> Ui.get <*> get
    -- This throws away the Left error just because that's what its callers all
    -- happen to want.

-- | This memoizes instrument resolution in case you're going to do it a lot.
-- 'resolve_instrument' has to merge some things so it's not exactly free.
-- The spine-strict Map makes this less efficient for one-off lookups, but so
-- far all uses are mapping the lookup across many events.
--
-- Of course, the memoization only works as long as the memo table persists,
-- which should happen if you use 'get_lookup_instrument' and reuse the
-- function it returns.
state_lookup_instrument :: Ui.State -> State -> ScoreT.Instrument
    -> Either Text ResolvedInstrument
state_lookup_instrument ui_state cmd_state = \inst ->
    fromMaybe (Left $ "no alloc for " <> pretty inst) $ Map.lookup inst memo
    where
    memo = resolve <$> (Ui.config#Ui.allocations_map #$ ui_state)
    resolve alloc = resolve_instrument db alloc
        where db = config_instrument_db (state_config cmd_state)

-- | See 'ResolvedInstrument'.
resolve_instrument :: InstrumentDb -> UiConfig.Allocation
    -> Either Text ResolvedInstrument
resolve_instrument db alloc = do
    let qualified = UiConfig.alloc_qualified alloc
    inst <- case (Inst.lookup qualified db, UiConfig.alloc_backend alloc) of
        (Just inst, _) -> Right inst
        -- Dummy instruments don't need a db entry.
        (Nothing, UiConfig.Dummy) ->
            Right $ Inst.Inst Inst.Dummy (Common.common empty_code)
        (Nothing, _) -> Left $ "patch not in db: " <> pretty qualified
    backend <- case (Inst.inst_backend inst, UiConfig.alloc_backend alloc) of
        (Inst.Midi patch, UiConfig.Midi config) ->
            return $ Midi patch (Patch.merge_defaults patch config)
        (Inst.Im patch, UiConfig.Im) -> return $ Im patch
        (_, UiConfig.Dummy) -> return Dummy

        -- TODO I'd like to do this instead, but unfortunately I use non-Dummy
        -- dummy instruments like sc-pemade-pasang, for API convenience.  I
        -- should fix that maybe.
        -- (Inst.Dummy, UiConfig.Dummy) -> return Dummy

        -- 'UiConfig.verify_allocation' should have prevented this.
        (inst_backend, alloc_backend) -> Left $
            "inconsistent backends: " <> pretty (inst_backend, alloc_backend)
    return $ ResolvedInstrument
        { inst_instrument = merge_call_map backend inst
        , inst_qualified = qualified
        , inst_common_config =
            merge_environ (Inst.inst_common inst) (UiConfig.alloc_config alloc)
        , inst_backend = backend
        }
    where
    -- If the Common.config_environ is Nothing, inherit from
    -- Common.common_environ.  This means the config_environ of
    -- a ResolvedInstrument is always Just, but I can't be bothered to make
    -- a whole new type.
    merge_environ :: Common.Common InstrumentCode -> Common.Config
        -> Common.Config
    merge_environ common =
        Common.cenviron %= Just . fromMaybe (Common.common_environ common)
    -- Put the attrs the instrument understands in the CallMap as +attr calls.
    -- If there isn't already a higher level call in there, then at least we
    -- don't lose the attrs entirely.
    merge_call_map backend =
        Inst.common#Common.call_map %= (<> attr_calls (inst_attrs backend))
    attr_calls attrs = Map.fromList
        [ (attr, Expr.Symbol $ ShowVal.show_val attr)
        | attr <- attrs, attr /= mempty
        ]
    inst_attrs = \case
        Midi patch _ ->
            Common.mapped_attributes $ Midi.Patch.patch_attribute_map patch
        Im patch ->
            Common.mapped_attributes $ Im.Patch.patch_attribute_map patch
        Dummy -> mempty

-- ** lookup qualified name

get_qualified :: M m => InstTypes.Qualified -> m Inst
get_qualified qualified =
    require ("instrument not in db: " <> pretty qualified)
        =<< lookup_qualified qualified

get_alloc_qualified :: M m => UiConfig.Allocation -> m Inst
get_alloc_qualified alloc =
    require ("instrument not in db: "
            <> pretty (UiConfig.alloc_qualified alloc))
        =<< lookup_alloc_qualified alloc

-- | Lookup an 'InstTypes.Qualified' in the context of its Allocation.  This is
-- because UiConfig.Dummy instruments can inherit a 'Common.Common' from any
-- backend.
lookup_alloc_qualified :: M m => UiConfig.Allocation -> m (Maybe Inst)
lookup_alloc_qualified alloc =
    fmap inherit <$> lookup_qualified (UiConfig.alloc_qualified alloc)
    where
    inherit inst = case UiConfig.alloc_backend alloc of
        UiConfig.Dummy -> (Inst.backend #= Inst.Dummy) inst
        _ -> inst

-- | Look up an instrument that might not be allocated.
lookup_qualified :: M m => InstTypes.Qualified -> m (Maybe Inst)
lookup_qualified qualified = do
    state <- get
    return $ state_lookup_qualified state qualified

state_lookup_qualified :: State -> InstTypes.Qualified -> Maybe Inst
state_lookup_qualified state qualified =
    Inst.lookup qualified $ config_instrument_db (state_config state)

-- ** misc

get_wdev_state :: M m => m WriteDeviceState
get_wdev_state = gets state_wdev_state

modify_wdev_state :: M m => (WriteDeviceState -> WriteDeviceState) -> m ()
modify_wdev_state f = modify $ \st ->
    st { state_wdev_state = f (state_wdev_state st) }

derive_immediately :: M m => [BlockId] -> m ()
derive_immediately block_ids = modify $ \st -> st { state_derive_immediately =
    Set.fromList block_ids <> state_derive_immediately st }

inflict_damage :: M m => Derive.ScoreDamage -> m ()
inflict_damage damage = modify_play_state $ \st -> st
    { state_current_performance = Map.map inflict (state_current_performance st)
    }
    where inflict perf = perf { perf_damage = damage <> perf_damage perf }

-- | Cause a block to rederive even if there haven't been any edits on it.
inflict_block_damage :: M m => BlockId -> m ()
inflict_block_damage block_id = inflict_damage $ mempty
    { Derive.sdamage_blocks = Set.singleton block_id }

-- | Cause a track to rederive even if there haven't been any edits on it.
inflict_track_damage :: M m => BlockId -> TrackId -> m ()
inflict_track_damage block_id track_id = inflict_damage $ mempty
    { Derive.sdamage_tracks = Map.singleton track_id Ranges.everything
    , Derive.sdamage_track_blocks = Set.singleton block_id
    }

-- ** EditState

modify_edit_state :: M m => (EditState -> EditState) -> m ()
modify_edit_state f = modify $ \st -> st { state_edit = f (state_edit st) }

-- | At the Ui level, the edit box is per-block, but I use it to indicate edit
-- mode, which is global.  So it gets stored in Cmd.State and must be synced
-- with new blocks.
set_edit_box :: M m => Block.Box -> Block.Box -> m ()
set_edit_box skel track = do
    modify_edit_state $ \st -> st { state_edit_box = (skel, track) }
    block_ids <- Ui.all_block_ids
    forM_ block_ids $ \bid -> Ui.set_edit_box bid skel track

is_val_edit :: M m => m Bool
is_val_edit = (== ValEdit) <$> gets (state_edit_mode . state_edit)

is_kbd_entry :: M m => m Bool
is_kbd_entry = gets (state_kbd_entry . state_edit)

set_note_text :: M m => Text -> m ()
set_note_text text = do
    modify_edit_state $ \st -> st { state_note_text = text }
    set_status Config.status_note_text $
        if Text.null text then Nothing else Just text

get_instrument_attributes :: M m => ScoreT.Instrument -> m Attrs.Attributes
get_instrument_attributes inst = fromMaybe mempty <$>
    gets (Map.lookup inst . state_instrument_attributes . state_edit)

set_instrument_attributes :: M m => ScoreT.Instrument -> Attrs.Attributes
    -> m ()
set_instrument_attributes inst attrs = modify_edit_state $ \st -> st
    { state_instrument_attributes =
        Map.insert inst attrs (state_instrument_attributes st)
    }


-- * util

-- | Give a name to a Cmd.  The name is applied when the cmd returns so the
-- names come out in call order, and it doesn't incur overhead for cmds that
-- abort.
name :: M m => Text -> m a -> m a
name s cmd = cmd <* modify (\st -> st
    { state_history_collect = (state_history_collect st)
        { state_cmd_names = s : state_cmd_names (state_history_collect st) }
    })

-- | Like 'name', but also set the 'state_suppress_edit'.  This will suppress
-- history recording until the edit mode changes from the given one.
suppress_history :: M m => EditMode -> Text -> m a -> m a
suppress_history mode name cmd = cmd <* modify (\st -> st
    { state_history_collect = (state_history_collect st)
        { state_cmd_names = name : state_cmd_names (state_history_collect st)
        , state_suppress_edit = Just mode
        }
    })

-- | Log an event so that it can be clicked on in logview.
log_event :: BlockId -> TrackId -> Event.Event -> Text
log_event block_id track_id event =
    Stack.log_ui_frame (Just block_id, Just track_id, Just (Event.range event))

-- | Turn off all sounding notes, reset controls.
-- TODO clear out WriteDeviceState?
all_notes_off :: M m => m ()
all_notes_off = mapM_ write_thru
    [ MidiThru $ Midi.Interface.AllNotesOff 0
    , MidiThru $ Interface.reset_controls 0
    , ImThru Shared.Osc.stop
    ]
