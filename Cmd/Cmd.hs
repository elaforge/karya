-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
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

import           GHC.Stack (HasCallStack)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Vivid.OSC as OSC

import qualified Util.Exceptions as Exceptions
import qualified Util.GitT as GitT
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import           Cmd.Msg (Performance(..))
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.REnv as REnv
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Interface
import qualified Midi.Interface as Interface
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
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Config as Shared.Config
import qualified Synth.Shared.Thru as Thru
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.KeycapsT as KeycapsT
import qualified Ui.Meter.Meter as Meter
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
data Handler m
    = Keymap (Keymap m)
    | Handler (Maybe (NoteEntryMap KeycapsT.KeyDoc)) (NamedCmd m)
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
            bindable <- abortUnless (msgToBindable msg)
            mods <- modsDown
            maybe (pure Continue) run $
                Map.lookup (KeySpec mods bindable) keymap

-- | Return the mods currently down, stripping out non-modifier keys and notes,
-- so that overlapping keys will still match.  Mouse mods are not filtered, so
-- each mouse chord can be bound individually.
modsDown :: M m => m (Set Modifier)
modsDown = Set.fromList <$> fmap (filter isMod . Map.keys) keysDown
    where
    isMod (KeyMod {}) = True
    isMod (MidiMod {}) = False
    isMod (MouseMod {}) = True

-- | Pair a Cmd with a Doc that can be used for logging, undo, etc.
data NamedCmd m = NamedCmd
    { cmdName :: Text
    , cmdCall :: Msg.Msg -> m Status
    }

-- | NoteEntry might depend on base octave, and might have different
-- mappings for unshifted or shifted.
data NoteEntryMap a
    = WithOctave (Map Pitch.Octave (Map Char a))
    | WithoutOctave (Map Char a)
    deriving (Show, Functor)

noteEntryLookup :: Pitch.Octave -> Char -> NoteEntryMap a -> Maybe a
noteEntryLookup octave char = \case
    WithOctave m -> Map.lookup char =<< Map.lookup octave m
    WithoutOctave m -> Map.lookup char m

-- ** Keymap

type Keymap m = Map KeySpec (NamedCmd m)

data KeySpec = KeySpec (Set Modifier) Bindable
    deriving (Eq, Ord, Show)

data Bindable
    -- | Key IsRepeat Key
    = Key Bool Key.Key
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

msgToBindable :: Msg.Msg -> Maybe Bindable
msgToBindable msg = case msg of
    (getKey -> Just (isRepeat, key)) -> Just $ Key isRepeat key
    (Msg.mouse -> Just mouse) -> case UiMsg.mouseState mouse of
        UiMsg.MouseDown btn ->
            Just $ Click btn on (UiMsg.mouseClicks mouse)
        UiMsg.MouseDrag btn -> Just $ Drag btn on
        UiMsg.MouseUp btn -> Just $ Release btn on
        _ -> Nothing
    (Msg.midi -> Just (Midi.ChannelMessage chan (Midi.NoteOn key _))) ->
        Just $ Note chan key
    _ -> Nothing
    where
    on = maybe Elsewhere mouseOn (Msg.context msg)
    getKey msg = case Msg.key msg of
        Just (UiMsg.KeyDown, k) -> Just (False, k)
        Just (UiMsg.KeyRepeat, k) -> Just (True, k)
        _ -> Nothing

mouseOn :: UiMsg.Context -> MouseOn
mouseOn = maybe Elsewhere on . UiMsg.ctxTrack
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
        Texts.join2 " " (showMods mods) (showBindable True bindable)
        where showMods = Text.intercalate " + " . map showMod . Set.toList

showMod :: Modifier -> Text
showMod m = case m of
    -- TODO this is only true on OS X
    KeyMod mod -> Key.showMacMod mod
    MouseMod button _ -> "mouse " <> showt button
    MidiMod chan key -> "midi " <> showt key <> " chan " <> showt chan

instance Pretty Bindable where
    pretty = showBindable True

showBindable :: Bool -> Bindable -> Text
showBindable showRepeatable b = case b of
    Key isRepeat key -> pretty key
        <> if showRepeatable && isRepeat then " (repeatable)" else ""
    Click button on times -> clickTimes times <> "click "
        <> showt button <> " on " <> pretty on
    Drag button on -> "drag " <> showt button <> " on " <> pretty on
    Release button on -> "release " <> showt button <> " on " <> pretty on
    Note chan key -> "midi " <> showt key <> " channel " <> showt chan
    where
    clickTimes 0 = ""
    clickTimes 1 = "double-"
    clickTimes 2 = "triple-"
    clickTimes n = showt n <> "-"

instance Pretty MouseOn where
    pretty OnTrack = "track"
    pretty OnDivider = "divider"
    pretty OnSkeleton = "skeleton"
    pretty Elsewhere = "elsewhere"


-- * run CmdT

type CmdId = CmdT Identity.Identity

-- | Cmds used by the REPL, which all run in IO.
type CmdL a = CmdT IO a

data Status
    -- | Continue processing, so another Cmd will have an opportunity to see
    -- the Msg.
    = Continue
    -- | Stop further cmd processing, \"consuming\" the Msg.
    | Done
    -- | Hack to control import dependencies, see "Cmd.PlayC".
    | Play PlayArgs
    -- | Open a FloatingInput box.
    | FloatingInput FloatingInput
    -- | Pack it up and go home.
    | Quit
    deriving (Show)

-- | Combine two Statuses by keeping the one with higher priority.
-- Yes, Status could be a Monoid but merge should be a rare operation.
mergeStatus :: Status -> Status -> Status
mergeStatus s1 s2 = if prio s1 >= prio s2 then s1 else s2
    where
    prio status = case status of
        Continue -> 0
        Done -> 1
        Play {} -> 2
        FloatingInput {} -> 3
        Quit -> 4

-- | Arguments for 'Cmd.PlayC.play'.  This is a special return value to trigger
-- a play, see "Cmd.PlayC" for details.
data PlayArgs = PlayArgs
    { playSync :: Maybe SyncConfig
    -- | Description of what is being played for logging.
    , playName :: Text
    , playMidi :: Midi.Perform.MidiEvents
    , playSc :: Sc.Note.PlayNotes
    , playInvTempo :: Maybe Transport.InverseTempoFunction
    , playRepeatAt :: Maybe RealTime
    -- | Since im playback is done by the VST, I don't directly control it as I
    -- do with the MIDI player, effectively all the sound is "in the decay." So
    -- I have an explicit end, and the MIDI thread will pretend it's still
    -- playing until it crosses this boundary.
    , playImEnd :: Maybe RealTime
    , playImDirect :: Maybe PlayDirectArgs
    }
instance Show PlayArgs where show _ = "((PlayArgs))"

-- | Arguments for 'Cmd.PlayC.play_im_direct_thread'.
data PlayDirectArgs = PlayDirectArgs
    { playScorePath :: FilePath
    , playBlockId :: BlockId
    , playMuted :: Set ScoreT.Instrument
    , playStart :: RealTime
    } deriving (Eq, Show)

data FloatingInput
    -- | Open a new floating text input.
    -- View, track, pos, (select start, select end).
    = FloatingOpen ViewId TrackNum ScoreTime Text (Int, Int)
    -- | Insert the given text into an already open edit box.
    | FloatingInsert Text
    deriving (Show)

-- | Cmds can run in either Identity or IO, but are generally returned in IO,
-- just to make things uniform.
type RunCmd cmdM valM a =
    Ui.State -> State -> CmdT cmdM a -> valM (Result a)

-- | The result of running a Cmd.
type Result a =
    ( State
    , [Thru]
    , [Log.Msg]
    , Either Ui.Error (a, Ui.State, Update.UiDamage)
    )

run :: Monad m => a -> RunCmd m m a
run abortVal ustate cstate cmd = do
    (((uiResult, cstate2), midi), logs) <-
        (Log.run . Logger.run . flip MonadState.runStateT cstate
            . Ui.run ustate . (\(CmdT m) -> m))
        cmd
    -- Any kind of error rolls back state and discards midi, but not log msgs.
    -- Normally 'abort_val' will be Continue, but obviously if 'cmd' doesn't
    -- return Status it can't be.
    pure $ case uiResult of
        Left Ui.Abort -> (cstate, [], logs, Right (abortVal, ustate, mempty))
        Left _ -> (cstate, [], logs, uiResult)
        _ -> (cstate2, midi, logs, uiResult)

-- | Like 'run', but discard MIDI thru and updates.
run_ :: Monad m => Ui.State -> State -> CmdT m a
    -> m (Either String (a, State, Ui.State), [Log.Msg])
run_ uiState cmdState cmd = do
    (cmdState, _thru, logs, result) <-
        run Nothing uiState cmdState (liftM Just cmd)
    pure $ (, logs) $ case result of
        Left err -> Left $ prettys err
        Right (val, uiState, _damage) -> case val of
            Nothing -> Left "aborted"
            Just v -> Right (v, cmdState, uiState)

-- | Like 'run_', but discard all the final states.
eval :: Monad m => Ui.State -> State -> CmdT m a
    -> m (Either String a, [Log.Msg])
eval uiState cmdState = fmap (first (second valOf)) . run_ uiState cmdState
    where
    valOf (a, _, _) = a

-- | Run the Cmd in Identity, returning Nothing if it aborted.
runId :: Ui.State -> State -> CmdT Identity.Identity a -> Result (Maybe a)
runId uiState cmdState cmd =
    Identity.runIdentity (run Nothing uiState cmdState (fmap Just cmd))

-- | Run the given command in Identity, but return it in IO, just as
-- a convenient way to have a uniform return type with 'run' (provided it is
-- run in IO).
runIdIo :: RunCmd Identity.Identity IO Status
runIdIo uiState cmdState cmd =
    pure $ Identity.runIdentity (run Continue uiState cmdState cmd)

runIo :: RunCmd IO IO Status
runIo = run Continue

-- | Promote a CmdId to a generic cmd, which can also run as a CmdT IO.
-- TODO: shouldn't it be possible to do this for free?
liftId :: M m => CmdId a -> m a
liftId cmd = do
    (cmdState, thru, logs, result) <- runId <$> Ui.get <*> get <*> pure cmd
    mapM_ Log.write logs
    case result of
        Left err -> Ui.throwError err
        Right (val, uiState, damage) -> case val of
            Nothing -> abort
            Just val -> do
                put cmdState
                mapM_ writeThru thru
                Ui.damage damage
                Ui.unsafePut uiState
                pure val

-- | Run a set of Cmds as a single Cmd.  The first one to return non-Continue
-- will return.  Cmds can use this to dispatch to other Cmds.
sequenceCmds :: M m => [a -> m Status] -> a -> m Status
sequenceCmds [] _ = pure Continue
sequenceCmds (cmd:cmds) msg = do
    status <- catchAbort (cmd msg)
    case status of
        Nothing -> sequenceCmds cmds msg
        Just Continue -> sequenceCmds cmds msg
        Just status -> pure status

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
    writeThru :: Thru -> m ()
    -- | An abort is an exception to get out of CmdT, but it's considered the
    -- same as returning Continue.  It's so a command can back out if e.g. it's
    -- selected by the 'Keymap' but has an additional prerequisite such as
    -- having an active block.
    abort :: m a
    catchAbort :: m a -> m (Maybe a)

instance Monad m => M (CmdT m) where
    get = (CmdT . lift) MonadState.get
    put st = (CmdT . lift) (MonadState.put st)
    writeThru msg = (CmdT . lift . lift) (Logger.log msg)
    abort = Except.throwError Ui.Abort
    catchAbort m = Except.catchError (fmap Just m) catch
        where
        catch Ui.Abort = pure Nothing
        catch err = Except.throwError err

data Thru
    -- | Send MIDI thru.  You can give it a timestamp, but it should be 0 for
    -- thru, which will cause it to go straight to the front of the queue.  Use
    -- 'midi' for normal midi thru.
    = MidiThru Midi.Interface.Message
    | ImThru Thru.Message
    | OscThru [OSC.OSC]
    deriving (Show)

midiThru :: Midi.WriteDevice -> Midi.Message -> Thru
midiThru dev msg =
    MidiThru $ Midi.Interface.Midi $ Midi.WriteMessage dev 0 msg

midi :: M m => Midi.WriteDevice -> Midi.Message -> m ()
midi dev msg = writeThru $ midiThru dev msg

osc :: M m => [OSC.OSC] -> m ()
osc = writeThru . OscThru

-- | For some reason, newtype deriving doesn't work on MonadTrans.
instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift -- whee!!

-- | Give CmdT unlifted access to all the logging functions.
instance Monad m => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . Log.write

-- | And to the UI state operations.
instance Monad m => Ui.M (CmdT m) where
    get = CmdT Ui.get
    unsafePut st = CmdT (Ui.unsafePut st)
    damage upd = CmdT (Ui.damage upd)
    getDamage = CmdT Ui.getDamage
    throwError msg = CmdT (Ui.throwError msg)

-- ** exceptions

-- | This is the same as Ui.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (HasCallStack, M m) => Text -> m a
throw = Ui.throw

-- | Run a subcomputation that is allowed to abort.
ignoreAbort :: M m => m a -> m ()
ignoreAbort m = void $ catchAbort m

-- | Run an IO action, rethrowing any IO exception as a Cmd exception.
rethrowIo :: IO a -> CmdT IO a
rethrowIo =
    either throw pure <=< liftIO . Exception.handle handle . (Right <$>)
    where
    handle :: Exception.SomeException -> IO (Either Text a)
    handle = pure . Left . ("io exception: "<>) . showt

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
abortUnless :: M m => Maybe a -> m a
abortUnless = maybe abort pure

-- | Throw an exception with the given msg on Nothing.
require :: (HasCallStack, M m) => Text -> Maybe a -> m a
require msg = maybe (throw msg) pure

requireRight :: (HasCallStack, M m) => (err -> Text) -> Either err a -> m a
requireRight fmtErr = either (throw . fmtErr) pure

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
data State = State
    { stateConfig :: Config
    -- | If set, the current 'Ui.State' was loaded from this file.
    -- This is so save can keep saving to the same file.
    , stateSaveFile :: Maybe (Writable, SaveFile)
    , stateSaved :: Saved
    , stateKyCache :: Maybe KyCache
    -- | Omit the usual derive delay for these blocks, and trigger a derive.
    -- This is set by integration, which modifies a block in response to
    -- another block being derived.  Blocks set to derive immediately are also
    -- considered to have block damage, if they didn't already.  This is
    -- cleared after every cmd.
    , stateDeriveImmediately :: Set BlockId
    -- | History.
    , stateHistory :: History
    , stateHistoryConfig :: HistoryConfig
    , stateHistoryCollect :: HistoryCollect
    , stateSelectionHistory :: SelectionHistory

    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position.  The value has complete info.
    , stateKeysDown :: Map Modifier Modifier
    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , stateFocusedView :: Maybe ViewId
    -- | This contains a Rect for each screen.  The first one is the default
    -- one, if a default is needed, though normally views should show up next
    -- to other views.
    , stateScreens :: [Rect.Rect]
    -- | Just indicates that the keycaps window is open.  The window is global,
    -- stored in "Ui.PtrMap", so I don't need to store it here.
    , stateKeycaps :: Maybe KeycapsState
    , stateKeycapsUpdate :: Maybe KeycapsUpdate

    -- | This is similar to 'Ui.Block.view_status', except that it's global
    -- instead of per-view.  So changes are logged with a special prefix so
    -- logview can catch them.  Really I only need this map to suppress log
    -- spam.
    , stateGlobalStatus :: Map Text Text
    , statePlay :: PlayState
    , stateHooks :: Hooks

    -- External device tracking.
    -- | MIDI state of WriteDevices.
    , stateWdevState :: WriteDeviceState

    -- | MIDI state of ReadDevices, including configuration like pitch bend
    -- range.
    , stateRdevState :: InputNote.ReadDeviceState
    , stateEdit :: EditState
    -- | The status return for this Cmd.  This is used only by the REPL, since
    -- non-REPL cmds simply return Status as their return value.  REPL cmds
    -- can't do that because they commonly use the return value to return
    -- an interesting String back to the REPL.
    , stateReplStatus :: Status
    -- | Enable various debug printing to stdout.
    , stateDebug :: Bool
    } deriving (Show)

data KeycapsUpdate
    = KeycapsUpdate KeycapsState (Maybe ((Int, Int), KeycapsT.Layout))
        KeycapsT.RawBindings
    | KeycapsClose
    deriving (Show)

-- | The set of things that can affect a keycaps window.  So when this changes,
-- the window has to be updated.  I assume the KeycapsT.Layout is constant, so
-- it's not in here, which allows me to cache global keymaps in CAFs.
data KeycapsState = KeycapsState
    { kcMods :: Set Modifier
    , kcOctave :: Pitch.Octave
    , kcIsKbdEntry :: Bool
    , kcTrackType :: Maybe ParseTitle.Type
    , kcInstrument :: Maybe ScoreT.Instrument
    , kcScaleId :: Maybe Pitch.ScaleId
    } deriving (Show, Eq)

-- ** SavedState

data SaveFile = SaveState Path.Canonical | SaveRepo Path.Canonical
    deriving (Show, Eq)
data Writable = ReadWrite | ReadOnly deriving (Show, Eq)

-- | This tracks how much the score has been saved to disk.
data Saved = Saved
    { _savedState :: SavedState
    , _editorOpen :: Bool
    } deriving (Eq, Show)

-- True if state is synced to disk, either because it was just saved and
-- not significantly changed, or because it was just loaded.
data SavedState
    -- | Just loaded from a file.  This is almost like SavedChanges, except
    -- that it's required so 'Cmd.Internal.sync_status' can tell the difference
    -- between the state changing because it was just loaded (set to
    -- SavedChanges) and changing due to an edit (set to UnsavedChanges).
    = JustLoaded
    | UnsavedChanges
    | SavedChanges
    deriving (Eq, Show)

-- | Absolute directory of the save file.
stateSaveDir :: State -> Maybe FilePath
stateSaveDir state = case stateSaveFile state of
    Nothing -> Nothing
    Just (_, SaveState fn) -> Just $ FilePath.takeDirectory $ Path.toPath fn
    Just (_, SaveRepo repo) -> Just $ FilePath.takeDirectory $ Path.toPath repo

-- | Given a saved state complete path, the path to its accompanying .ky file.
kyPath :: FilePath -> FilePath
kyPath fname = fname <> ".ky"

-- | Like 'kyPath' for a repo, which is foo.git -> foo.ky.
repoKyPath :: FilePath -> FilePath
repoKyPath = kyPath . fst . Lists.dropSuffix ".git"

saveFileKyPath :: SaveFile -> FilePath
saveFileKyPath (SaveState fn) = kyPath $ Path.toPath fn
saveFileKyPath (SaveRepo repo) = repoKyPath $ Path.toPath repo

-- | Unique name for this score, for the global im cache.
scorePath :: State -> FilePath
scorePath state = case stateSaveFile state of
    -- #untitled so it's clear where it came from and unlikely to conflict with
    -- a real filename.
    Nothing -> "#untitled"
    Just (_, SaveState fn) -> strip fn
    Just (_, SaveRepo repo) -> strip repo
    where
    strip = Path.dropPrefix (configSaveDir (stateConfig state))

type Error = Text

-- | A loaded and parsed ky file, or an error string.  This also has the files
-- loaded and their timestamps, to detect when one has changed.
data KyCache
    -- | Error marks a broken ky file.  Error reporting is delayed
    -- until you try to play.  This is
    = KyCache (Either Error (Derive.Builtins, Derive.InstrumentAliases))
        Fingerprint
    -- | This disables the cache mechanism.  Tests use this to avoid having
    -- to set SaveFile.
    | PermanentKy (Derive.Builtins, Derive.InstrumentAliases)
    deriving (Show)

-- | Keep track of loaded files and a fingerprint for their contents.  This is
-- used to detect when they should be reloaded.
data Fingerprint = Fingerprint [FilePath] Int
    deriving (Eq, Show)

instance Semigroup Fingerprint where
    Fingerprint fnames1 fprint1 <> Fingerprint fnames2 fprint2 =
        Fingerprint (fnames1<>fnames2) (Hashable.hashWithSalt fprint1 fprint2)
instance Monoid Fingerprint where
    mempty = Fingerprint [] 0
    mappend = (<>)
instance Pretty Fingerprint where
    pretty (Fingerprint files word) = pretty files <> ":" <> pretty word

fingerprint :: [Ky.Loaded] -> Fingerprint
fingerprint imports =
    -- The code in 'Ui.ky' gets "" for its filename.
    Fingerprint (filter (not . null) fnames)
        (foldl' Hashable.hashWithSalt 0 contents)
    where
    (fnames, contents) = unzip
        [(fname, content) | Ky.Loaded fname content <- imports]

initialState :: Config -> State
initialState config = State
    { stateConfig = config
    , stateSaveFile = Nothing
    , stateSaved = Saved JustLoaded False
    , stateKyCache = Nothing
    , stateDeriveImmediately = Set.empty
    -- This is a dummy entry needed to bootstrap a Cmd.State.  Normally
    -- 'hist_present' should always have the current state, but the initial
    -- setup cmd needs a State too.
    , stateHistory = initialHistory (emptyHistoryEntry Ui.empty)
    , stateHistoryConfig = emptyHistoryConfig
    , stateHistoryCollect = emptyHistoryCollect
    , stateSelectionHistory = emptySelectionHistory
    , stateKeysDown = Map.empty
    , stateFocusedView = Nothing
    , stateScreens = []
    , stateKeycaps = Nothing
    , stateKeycapsUpdate = Nothing
    , stateGlobalStatus = Map.empty
    , statePlay = initialPlayState
    , stateHooks = mempty

    , stateWdevState = emptyWdevState
    , stateRdevState = InputNote.emptyRdevState
    , stateEdit = initialEditState
    , stateReplStatus = Continue
    , stateDebug = False
    }

-- | Reset the parts of the State which are specific to a \"session\".  This
-- should be called whenever an entirely new state is loaded.
reinitState :: HistoryEntry -> State -> State
reinitState present cstate = cstate
    { stateHistory = initialHistory present
    -- Performance threads should have been killed by the caller.
    , statePlay = initialPlayState
        { statePlayStep = statePlayStep (statePlay cstate) }
    -- This is essential, otherwise lots of cmds break on the bad reference.
    , stateFocusedView = Nothing
    , stateEdit = initialEditState
        { stateTimeStep = stateTimeStep (stateEdit cstate) }
    }

-- ** Config

-- | Config type variables that change never or rarely.  These mostly come from
-- the "App.StaticConfig".
data Config = Config
    -- | App root, initialized from 'Config.get_app_dir'.
    { configAppDir :: Path.AppDir
    , configSaveDir :: Path.Canonical
    , configMidiInterface :: Midi.Interface.Interface
    -- | Search path for local definition files, from 'Config.kyPaths'.
    , configKyPaths :: [FilePath]
    -- | Reroute MIDI inputs and outputs.  These come from
    -- 'App.StaticConfig.rdev_map' and 'App.StaticConfig.wdev_map' and probably
    -- shouldn't be changed at runtime.
    , configRdevMap :: Map Midi.ReadDevice Midi.ReadDevice
    -- | WriteDevices can be score-specific, though, so another map is kept in
    -- 'Ui.State', which may override the one here.
    , configWdevMap :: Map Midi.WriteDevice Midi.WriteDevice
    , configInstrumentDb :: InstrumentDb
    -- | Library of calls for the deriver.
    , configBuiltins :: Derive.Builtins
    , configHighlightColors :: Map Color.Highlight Color.Color
    , configIm :: Shared.Config.Config
    -- | If True, play im audio directly instead of via the play_cache VST.
    -- This means you don't need a DAW, but if you have MIDI instruments
    -- they probably won't be very in sync.
    , configImPlayDirect :: Bool
    , configGitUser :: SaveGitT.User
    -- | If set, and there are im instruments, there is an implicit automatic
    -- allocation for play-cache/ on this addr.
    , configPlayCache :: Maybe (Midi.WriteDevice, Midi.Channel)
    } deriving (Show)

-- | Get a midi writer that takes the 'config_wdev_map' into account.
stateMidiWriter :: State -> Midi.Interface.Message -> IO ()
stateMidiWriter state imsg = do
    let out = case imsg of
            Midi.Interface.Midi wmsg -> Midi.Interface.Midi $ mapWdev wmsg
            _ -> imsg
    mbErr <- Midi.Interface.writeMessage
        (configMidiInterface (stateConfig state)) out
    whenJust mbErr $ \err ->
        Log.warn $ "error writing " <> pretty out <> ": " <> err
    where
    mapWdev (Midi.WriteMessage wdev time msg) =
        Midi.WriteMessage (lookupWdev wdev) time msg
    lookupWdev wdev = Map.findWithDefault wdev wdev
        (configWdevMap (stateConfig state))

-- | Convert a relative path to place it in the app dir.
toAbsolute :: State -> Path.Relative -> FilePath
toAbsolute state = Path.toAbsolute (configAppDir (stateConfig state))

-- | This was previously in 'Config', and configured via StaticConfig.  But it
-- turns out I don't really use StaticConfig.  It has a name here just so
-- I don't get references to 'Scale.All.lookup_scale' everywhere.
lookupScale :: Derive.LookupScale
lookupScale = Scale.All.lookupScale

-- ** PlayState

-- | State concerning derivation, performance, and playing the performance.
data PlayState = PlayState
    -- | Transport control channel for the player, if one is running.
    -- This is a list even though I only expect one to run at a time.  The
    -- reason is that if starting a new one stops the old one, there will
    -- be a small window where the old one has not exited yet.  It's probably
    -- better to wait for the previous one to exit, but I've also had bugs
    -- where play gets stuck on, that makes me think with the concurrency I'm
    -- going to wind up with overlapping anyway, so I may as well handle it
    -- when I see it.
    { statePlayControl :: [Transport.PlayControl]
    -- | When changes are made to a block, its performance will be
    -- recalculated in the background.  When the Performance is forced, it will
    -- replace the existing performance in 'state_performance', if any.  This
    -- means there will be a window in which the performance is out of date,
    -- but this is better than hanging the responder every time it touches an
    -- insufficiently lazy part of the performance.
    , statePerformance :: Map BlockId Performance
    -- | However, some cmds, like play, want the most up to date performance
    -- even if they have to wait for it.  This map will be updated
    -- immediately.
    , stateCurrentPerformance :: Map BlockId Performance
    -- | Keep track of current thread working on each performance.  If a
    -- new performance is needed before the old one is complete, it can be
    -- killed off.
    , statePerformanceThreads :: Map BlockId Thread
    -- | Some play commands start playing from a short distance before the
    -- cursor.
    , statePlayStep :: TimeStep.TimeStep
    -- | Contain a StepState if step play is active.  Managed in
    -- "Cmd.StepPlay".
    , stateStep :: Maybe StepState
    -- | Globally speed up or slow down performance.  It mutiplies the
    -- timestamps by the reciprocal of this amount, so 2 will play double
    -- speed, and 0.5 will play half speed.
    , statePlayMultiplier :: RealTime
    -- | If set, synchronize with a DAW when the selection is set, and on play
    -- and stop.
    , stateSync :: Maybe SyncConfig
    -- | Track im progress as updated by 'Msg.ImRenderingRange'.  This is
    -- ultimately displayed on the GUI, but I keep track here so I can take
    -- minimum and maximum when multiple instruments live on one track.
    , stateImProgress :: Map BlockId
        (Map TrackId (Map ScoreT.Instrument (RealTime, RealTime)))
    , statePreviousPlay :: Maybe PlayCmd
    } deriving (Show)

-- | Wrapper around CmdId PlayArgs to make it Showable.
data PlayCmd = PlayCmd Text (CmdId PlayArgs)
instance Show PlayCmd where
    show (PlayCmd name _) = "PlayCmd " <> show name

-- | Wrap Async to make it showable.  I use Async instead of ThreadId because
-- I want to make sure they can run their finalizers when the app quits, and
-- I can wait on an Async, but not on a ThreadId.  At the moment, the
-- finalizers are killing im subprocesses.  'kill_thread' uses Async.cancel,
-- which synchronously waits for the thread to complete.
newtype Thread = Thread (Async.Async ())

instance Show Thread where
    show = show . Async.asyncThreadId . u
        where u (Thread t) = t

killThread :: Thread -> IO ()
killThread (Thread async) = Async.cancel async

killPerformanceThreads :: State -> IO ()
killPerformanceThreads =
    mapM_ killThread . Map.elems . statePerformanceThreads . statePlay

-- | Get currently evaluating root BlockIds.
runningThreads :: CmdT IO [BlockId]
runningThreads = do
    threads <- gets $ Map.toAscList . statePerformanceThreads . statePlay
    alive <- liftIO $ filterM (isAlive . snd) threads
    pure $ map fst alive
    where
    isAlive (Thread async) = Maybe.isNothing <$> Async.poll async

initialPlayState :: PlayState
initialPlayState = PlayState
    { statePlayControl = []
    , statePerformance = Map.empty
    , stateCurrentPerformance = Map.empty
    , statePerformanceThreads = Map.empty
    , statePlayStep = TimeStep.timeStep $
        TimeStep.RelativeMark TimeStep.AllMarklists Meter.Section
    , stateStep = Nothing
    , statePlayMultiplier = RealTime.seconds 1
    , stateSync = Nothing
    , stateImProgress = mempty
    , statePreviousPlay = Nothing
    }

-- | Step play is a way of playing back the performance in non-realtime.
data StepState = StepState
    -- - constant
    -- | Keep track of the view step play was started in, so I know where to
    -- display the selection.
    { stepViewId :: ViewId
    -- | If step play only applies to a few tracks, list them.  If null,
    -- step play applies to all tracks.
    , stepTracknums :: [TrackNum]

    -- - modified
    -- | MIDI states before the step play position, in descending order.
    , stepBefore :: [(ScoreTime, Midi.State.State)]
    -- | MIDI states after the step play position, in asceding order.
    , stepAfter :: [(ScoreTime, Midi.State.State)]
    } deriving (Show)

-- | Configure synchronization.  MMC is used to set the play position and MTC
-- is used to start and stop playing.
--
-- MMC has start and stop msgs, but they seem useless, since they're sysexes,
-- which are not delivered precisely.
data SyncConfig = SyncConfig
    { syncDevice :: Midi.WriteDevice
    -- | Send MMC to this device.
    , syncDeviceId :: Mmc.DeviceId
    -- | If true, send MTC on the 'sync_device'.  If this is set, MMC play and
    -- stop will be omitted, since the presence of MTC should be enough to get
    -- the DAW started, provided it's in external sync mode.
    --
    -- DAWs tend to spend a long time synchronizing, presumably because
    -- hardware devices take time to spin up.  That's unnecessary in software,
    -- so in Cubase you can set \"lock frames\" to 2, and in Reaper you can set
    -- \"synchronize by seeking ahead\" to 67ms.
    , syncMtc :: Bool
    , syncFrameRate :: Midi.FrameRate
    } deriving (Show)

instance Pretty SyncConfig where
    format (SyncConfig dev devId mtc rate) = Pretty.record "SyncConfig"
        [ ("device", Pretty.format dev)
        , ("device_id", Pretty.format devId)
        , ("mtc", Pretty.format mtc)
        , ("frame_rate", Pretty.text (showt rate))
        ]

-- ** hooks

-- | Hooks are Cmds that run after some event.
newtype Hooks = Hooks {
    -- | Run when the selection changes.
    hooksSelection :: [[(ViewId, Maybe TrackSelection)] -> CmdId ()]
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
data EditState = EditState
    -- | Edit mode enables various commands that write to tracks.
    { stateEditMode :: EditMode
    -- | True if the floating input edit is open.
    , stateFloatingInput :: Bool
    -- | Whether or not to advance the insertion point after a note is
    -- entered.
    , stateAdvance :: Bool
    -- | Chord mode means the note is considered entered when all NoteOffs
    -- have been received.  While a note is held down, the insertion point will
    -- move to the next note track with the same instrument so you can
    -- enter chords.
    --
    -- When chord mode is off, the note is considered entered as soon as
    -- its NoteOn is received.
    , stateChord :: Bool
    -- | Try to find or create a 'Controls.dynamic' track for to record
    -- 'InputNote.Input' velocity, similar to how a pitch track is edited and
    -- created.
    , stateRecordVelocity :: Bool
    -- | Use the alphanumeric keys to enter notes in addition to midi input.
    , stateKbdEntry :: Bool
    -- | Default time step for cursor movement.
    , stateTimeStep :: TimeStep.TimeStep
    -- | Used for note duration.  It's separate from 'state_time_step' to
    -- allow for tracker-style note entry where newly entered notes extend to
    -- the next note or the end of the block.
    , stateNoteDuration :: TimeStep.TimeStep
    -- | If this is Negative, create notes with negative durations.
    , stateNoteOrientation :: Types.Orientation
    -- | New notes get this text by default.  This way, you can enter a series
    -- of notes with the same attributes, or whatever.
    , stateNoteText :: Text
    -- | Transpose note entry on the keyboard by this many octaves.  It's by
    -- octave instead of scale degree since scales may have different numbers
    -- of notes per octave.
    , stateKbdEntryOctave :: Pitch.Octave
    , stateRecordedActions :: RecordedActions
    , stateInstrumentAttributes :: Map ScoreT.Instrument Attrs.Attributes
    -- | See 'set_edit_box'.
    , stateEditBox :: (Block.Box, Block.Box)
    } deriving (Eq, Show)

initialEditState :: EditState
initialEditState = EditState {
    stateEditMode = NoEdit
    , stateFloatingInput = False
    , stateKbdEntry = False
    , stateAdvance = True
    , stateChord = False
    , stateRecordVelocity = False
    , stateTimeStep = statePlayStep initialPlayState
    , stateNoteDuration = TimeStep.eventEdge
    , stateNoteOrientation = Types.Positive
    , stateNoteText = ""
    -- This should put middle C in the center of the kbd entry keys.
    , stateKbdEntryOctave = 3
    , stateRecordedActions = mempty
    , stateInstrumentAttributes = mempty
    , stateEditBox = (box, box)
    } where box = uncurry Block.Box Config.bconfigBox

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
data Action
    -- | If a duration is given, the event has that duration, otherwise
    -- it gets the current time step.
    = InsertEvent (Maybe TrackTime) Text
    | ReplaceText Text | PrependText Text | AppendText Text
    deriving (Show, Eq)

instance Pretty Action where
    pretty act = case act of
        InsertEvent maybeDur text ->
            pretty text <> " (" <> pretty maybeDur <> ")"
        ReplaceText text -> "=" <> pretty text
        PrependText text -> pretty text <> "+"
        AppendText text -> "+" <> pretty text

-- *** midi devices

data WriteDeviceState = WriteDeviceState
    -- Used by Cmd.MidiThru:
    -- | NoteId currently playing in each Addr.  An Addr may have >1 NoteId.
    { wdevNoteAddr :: Map InputNote.NoteId Patch.Addr
    -- | The note id is not guaranteed to have any relationship to the key,
    -- so the MIDI NoteOff needs to know what key the MIDI NoteOn used.
    , wdevNoteKey :: Map InputNote.NoteId Midi.Key
    -- | Map an addr to a number that increases when it's assigned a note.
    -- This is used along with 'wdev_serial' to implement addr round-robin.
    , wdevAddrSerial :: Map Patch.Addr Serial
    -- | Next serial number for 'wdev_addr_serial'.
    , wdevSerial :: Serial
    -- | Last NoteId seen.  This is needed to emit controls (rather than just
    -- mapping them from MIDI input) because otherwise there's no way to know
    -- to which note they should be assigned.
    , wdevLastNoteId :: Maybe InputNote.NoteId

    -- Used by Cmd.PitchTrack:
    -- | NoteIds being entered into which pitch tracks.  When entering a chord,
    -- a PitchChange uses this to know which pitch track to update.
    , wdevPitchTrack :: Map InputNote.NoteId (BlockId, TrackNum)

    -- Used by no one, yet:  (TODO should someone use this?)
    -- | Remember the current patch of each addr.  More than one patch or
    -- keyswitch can share the same addr, so I need to keep track which one is
    -- active to minimize switches.
    , wdevAddrInst :: Map Patch.Addr Midi.Types.Patch
    } deriving (Eq, Show)

type Serial = Int

emptyWdevState :: WriteDeviceState
emptyWdevState = WriteDeviceState
    { wdevNoteAddr = Map.empty
    , wdevNoteKey = Map.empty
    , wdevAddrSerial = Map.empty
    , wdevSerial = 0
    , wdevLastNoteId = Nothing
    , wdevPitchTrack = Map.empty
    , wdevAddrInst = Map.empty
    }

-- *** performance

perfTempo :: Performance -> Transport.TempoFunction
perfTempo = TrackWarp.tempoFunc . perfWarps

perfInvTempo :: Performance -> Transport.InverseTempoFunction
perfInvTempo = TrackWarp.inverseTempoFunc . perfWarps

perfClosestWarp :: Performance -> Transport.ClosestWarpFunction
perfClosestWarp = TrackWarp.closestWarp . perfWarps

-- *** instrument

-- | The code part of an instrument, i.e. the calls and cmds it brings into
-- scope.
--
-- This has to be in Cmd.Cmd for circular import reasons.
data InstrumentCode = InstrumentCode
    { instCalls :: Derive.InstrumentCalls
    , instPostproc :: InstrumentPostproc
    , instCmds :: [HandlerId]
    -- | An optional specialized cmd to write Thru.  This is separate from
    -- 'inst_cmds' so it can be run wherever the instrument needs special thru,
    -- not just in the instrument's note track.  This way custom thru works in
    -- the pitch track too.
    , instThru :: Maybe ThruFunction
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

makeDeriveInstrument :: ResolvedInstrument -> Derive.Instrument
makeDeriveInstrument resolved = Derive.Instrument
    { instCalls = instCalls $ Common.commonCode $ Inst.instCommon $
        instInstrument resolved
    , instEnviron = REnv.convert $
        Common.configEnviron $ instCommonConfig resolved
    , instControls = Common.configControls (instCommonConfig resolved)
    , instAttributes = Inst.instAttributes (instInstrument resolved)
    , instElements = case Inst.instBackend (instInstrument resolved) of
        Inst.Im patch -> Im.Patch.patchElements patch
        _ -> mempty
    }

emptyCode :: InstrumentCode
emptyCode = InstrumentCode
    { instCalls = mempty
    , instPostproc = (,[])
    , instCmds = []
    , instThru = Nothing
    }

-- | Instantiate 'Inst.Db' with the code type.  The only reason the Db has the
-- type parameter is so I can define it in its own module without a circular
-- import.
type InstrumentDb = Inst.Db InstrumentCode
-- | Like 'InstrumentDb'.
type Inst = Inst.Inst InstrumentCode


-- *** history

-- | Ghosts of state past, present, and future.
data History = History
    { histPast :: [HistoryEntry]
    -- | The present is actually the immediate past.  When you undo, the
    -- undo itself is actually in the future of the state you want to undo.
    -- So another way of looking at it is that you undo from the past to
    -- a point further in the past.  But since you always require a \"recent
    -- past\" to exist, it's more convenient to break it out and call it the
    -- \"present\".  Isn't time travel confusing?
    , histPresent :: HistoryEntry
    , histFuture :: [HistoryEntry]
    , histLastCmd :: Maybe LastCmd
    } deriving (Show)

initialHistory :: HistoryEntry -> History
initialHistory present = History [] present [] Nothing

-- | Record some information about the last cmd for the benefit of
-- 'Cmd.Undo.maintain_history'.
data LastCmd
    -- | This cmd set the state because it was an undo or redo.  Otherwise undo
    -- and redo themselves would be recorded and multiple undo would be
    -- impossible!
    = UndoRedo
    -- | This cmd set the state because of a load.  This should reset all the
    -- history so I can start loading from the new state's history.
    | Load (Maybe GitT.Commit) [Text]
    deriving (Show)

data HistoryConfig = HistoryConfig
    -- | Keep this many previous history entries in memory.
    { histKeep :: Int
    -- | Checkpoints are saved relative to the state at the next checkpoint.
    -- So it's important to keep the commit of that checkpoint up to date,
    -- otherwise the state and the checkpoints will get out of sync.
    , histLastCommit :: Maybe GitT.Commit
    } deriving (Show)

emptyHistoryConfig :: HistoryConfig
emptyHistoryConfig = HistoryConfig Config.defaultKeepHistory Nothing

data HistoryCollect = HistoryCollect
    -- | This is cleared after each cmd.  A cmd can cons its name on, and
    -- the cmd is recorded with the (optional) set of names it returns.
    -- Hopefully each cmd has at least one name, since this makes the history
    -- more readable.  There can be more than one name if the history records
    -- several cmds or if one cmd calls another.
    { stateCmdNames :: [Text]
    -- | Suppress history record until the EditMode changes from the given one.
    -- This is a bit of a hack so that every keystroke in a raw edit isn't
    -- recorded separately.
    , stateSuppressEdit :: Maybe EditMode
    -- | The Git.Commit in the SaveHistory should definitely be Nothing.
    , stateSuppressed :: Maybe SaveGitT.SaveHistory
    } deriving (Show)

emptyHistoryCollect :: HistoryCollect
emptyHistoryCollect = HistoryCollect
    { stateCmdNames = []
    , stateSuppressEdit = Nothing
    , stateSuppressed = Nothing
    }

data HistoryEntry = HistoryEntry
    { histState :: Ui.State
    -- | Since track event updates are not caught by diff but recorded by
    -- Ui.State, I have to save those too, or else an undo or redo will miss
    -- the event changes.  TODO ugly, can I avoid this?
    --
    -- If this HistoryEntry is in the past, these are the updates that took it
    -- to the future, not the updates emitted by the cmd itself.  If the
    -- HistoryEntry is in the future, the updates take it to the past, which
    -- are the updated emitted by the cmd.  So don't be confused if it looks
    -- like a HistoryEntry has the wrong updates.
    , histDamage :: Update.UiDamage
    -- | Cmds involved creating this entry.
    , histNames :: [Text]
    -- | The Commit where this entry was saved.  Nothing if the entry is
    -- unsaved.
    , histCommit :: Maybe GitT.Commit
    } deriving (Show)

emptyHistoryEntry :: Ui.State -> HistoryEntry
emptyHistoryEntry state = HistoryEntry
    { histState = state
    , histDamage = mempty
    , histNames = []
    , histCommit = Nothing
    }

instance Pretty History where
    format (History past present future lastCmd) = Pretty.record "History"
        [ ("past", Pretty.format past)
        , ("present", Pretty.format present)
        , ("future", Pretty.format future)
        , ("last_cmd", Pretty.text (showt lastCmd))
        ]

instance Pretty HistoryEntry where
    format (HistoryEntry _state damage commands commit) =
        Pretty.format commit Pretty.<+> Pretty.textList commands
        Pretty.<+> Pretty.format damage

instance Pretty HistoryConfig where
    format (HistoryConfig keep lastCommit) = Pretty.record "HistoryConfig"
        [ ("keep", Pretty.format keep)
        , ("last_commit", Pretty.format lastCommit)
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
data SelectionHistory = SelectionHistory
    { selPast :: [(ViewId, Sel.Selection)]
    , selFuture :: [(ViewId, Sel.Selection)]
    } deriving (Eq, Show)

emptySelectionHistory :: SelectionHistory
emptySelectionHistory = SelectionHistory [] []

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
        MouseMod button mbTrack -> "MouseMod" <> pretty (button, mbTrack)
        MidiMod chan key -> "MidiMod" <> pretty (chan, key)

mouseModBtn :: Modifier -> Maybe Types.MouseButton
mouseModBtn (MouseMod btn _) = Just btn
mouseModBtn _ = Nothing

-- | Take a modifier to its key in the modifier map which has extra info like
-- mouse down position stripped.
stripModifier :: Modifier -> Modifier
stripModifier (MouseMod btn _) = MouseMod btn Nothing
stripModifier mod = mod


-- ** state access

gets :: M m => (State -> a) -> m a
gets f = do
    state <- get
    pure $! f state

modify :: M m => (State -> State) -> m ()
modify f = do
    st <- get
    put $! f st

modifyPlayState :: M m => (PlayState -> PlayState) -> m ()
modifyPlayState f = modify $ \st -> st { statePlay = f (statePlay st) }

-- | Return the rect of the screen closest to the given point, or the default.
getScreen :: M m => Maybe (Int, Int) -> m Rect.Rect
getScreen mbPoint = do
    screens <- gets stateScreens
    -- There are no screens yet during setup, so pick something somewhat
    -- reasonable so windows don't all try to crunch themselves down to
    -- nothing.
    pure $ fromMaybe (Rect.xywh 0 0 800 600) $ case mbPoint of
        Nothing -> Lists.head screens
        Just point -> Lists.minimumOn (Rect.distance point) screens

lookupPerformance :: M m => BlockId -> m (Maybe Performance)
lookupPerformance blockId =
    gets $ Map.lookup blockId . statePerformance . statePlay

getPerformance :: M m => BlockId -> m Performance
getPerformance blockId = abortUnless =<< lookupPerformance blockId

-- | Clear all performances, which will cause them to be rederived.
-- This is stronger than 'Ui.update_all', because Ui.update_all will simply
-- cause diff to look at the blocks for diffs, while this will force a
-- re-derivation even if there are no apparent diffs.
invalidatePerformances :: M m => m ()
invalidatePerformances = modifyPlayState $ \state -> state
    { statePerformance = mempty
    , stateCurrentPerformance = mempty
    }

clearImCache :: BlockId -> CmdT IO ()
clearImCache blockId = do
    path <- gets scorePath
    liftIO $ do
        config <- Shared.Config.getConfig
        let imDir = Shared.Config.imDir config
        Exceptions.ignoreEnoent $ Directory.removeDirectoryRecursive $
            Shared.Config.notesDirectory imDir path blockId
        Exceptions.ignoreEnoent $ Directory.removeDirectoryRecursive $
            Shared.Config.outputDirectory imDir path blockId
    pure ()

-- | Keys currently held down, as in 'state_keys_down'.
keysDown :: M m => m (Map Modifier Modifier)
keysDown = gets stateKeysDown

getFocusedView :: M m => m ViewId
getFocusedView = abortUnless =<< gets stateFocusedView

getFocusedBlock :: M m => m BlockId
getFocusedBlock = Block.viewBlock <$> (Ui.getView =<< getFocusedView)

lookupFocusedView :: M m => m (Maybe ViewId)
lookupFocusedView = gets stateFocusedView

-- | In some circumstances I don't want to abort if there's no focused block.
lookupFocusedBlock :: M m => m (Maybe BlockId)
lookupFocusedBlock = do
    maybeViewId <- lookupFocusedView
    case maybeViewId of
        -- It's still an error if the view id doesn't exist.
        Just viewId -> fmap (Just . Block.viewBlock) (Ui.getView viewId)
        Nothing -> pure Nothing

-- | Request focus.  'state_focused_view' will be updated once fltk reports the
-- focus change.
focus :: Ui.M m => ViewId -> m ()
focus viewId = do
    view <- Ui.lookupView viewId
    case view of
        Nothing ->
            Ui.throw $ "Cmd.focus on non-existent view: " <> showt viewId
        _ -> pure ()
    Ui.damage $ mempty { Update._bringToFront = Set.singleton viewId }

getCurrentStep :: M m => m TimeStep.TimeStep
getCurrentStep = gets (stateTimeStep . stateEdit)

-- | Get the leftmost track covered by the insert selection, which is
-- considered the "focused" track by convention.
getInsertTracknum :: M m => m (Maybe TrackNum)
getInsertTracknum = do
    viewId <- getFocusedView
    sel <- Ui.getSelection viewId Config.insertSelnum
    -- Selection.shift can put the selection on or before the ruler track, so
    -- it doesn't get squished by going off the edge, but I don't want to ever
    -- act like <=0 is selected.
    pure $ max 1 . Sel.startTrack <$> sel

-- *** status

-- | This just calls 'Ui.set_view_status', but all status setting should
-- go through here so they can be uniformly filtered or logged or something.
setViewStatus :: M m => ViewId -> (Int, Text) -> Maybe Text -> m ()
setViewStatus = Ui.setViewStatus

-- | Emit a special log msg that will cause log view to put this key and value
-- in its status bar.  A value of \"\" will cause logview to delete that key.
setGlobalStatus :: M m => Text -> Text -> m ()
setGlobalStatus key val = do
    statusMap <- gets stateGlobalStatus
    when (Map.lookup key statusMap /= Just val) $ do
        modify $ \st ->
            st { stateGlobalStatus = Map.insert key val statusMap }
        Log.debug $ "global status: " <> key <> " -- " <> val

-- | Set a status variable on all views.
setStatus :: M m => (Int, Text) -> Maybe Text -> m ()
setStatus key val = do
    viewIds <- Ui.gets (Map.keys . Ui.stateViews)
    forM_ viewIds $ \viewId -> setViewStatus viewId key val

-- ** lookup instrument

-- | This is an instrument as looked up by 'lookup_instrument' or
-- 'get_lookup_instrument'.  This merges compiled-in and runtime instrument
-- data.
data ResolvedInstrument = ResolvedInstrument
    { instInstrument :: Inst
    , instQualified :: InstT.Qualified
    , instCommonConfig :: Common.Config
    , instBackend :: Backend
    } deriving (Show)

instSynth :: ResolvedInstrument -> InstT.SynthName
instSynth  = InstT.synth . instQualified

instCommon :: ResolvedInstrument -> Common.Common InstrumentCode
instCommon = Inst.instCommon . instInstrument

instance Pretty ResolvedInstrument where
    format (ResolvedInstrument instrument qualified commonConfig backend) =
        Pretty.record "ResolvedInstrument"
            [ ("instrument", Pretty.format instrument)
            , ("qualified", Pretty.format qualified)
            , ("common_config", Pretty.format commonConfig)
            , ("backend", Pretty.format backend)
            ]

-- | This merges the compiled-id 'Inst.Backend' and the per-score
-- 'UiConfig.Backend'.
data Backend
    = Midi Midi.Patch.Patch Midi.Patch.Config
    | Im Im.Patch.Patch
    | Sc Sc.Patch.Patch
    | Dummy Text
    deriving (Show)

instance Pretty Backend where
    format (Midi patch config) = Pretty.format (patch, config)
    format (Im patch) = Pretty.format patch
    format (Sc patch) = Pretty.format patch
    format (Dummy msg) = "Dummy \"" <> Pretty.text msg <> "\""

midiPatch :: ResolvedInstrument -> Maybe (Patch.Patch, Patch.Config)
midiPatch inst = case instBackend inst of
    Midi patch config -> Just (patch, config)
    _ -> Nothing

scPatch :: ResolvedInstrument -> Maybe Sc.Patch.Patch
scPatch inst = case instBackend inst of
    Sc patch -> Just patch
    _ -> Nothing

getMidiInstrument :: (HasCallStack, M m) => ScoreT.Instrument
    -> m (Patch.Patch, Patch.Config)
getMidiInstrument inst =
    require ("not a midi instrument: " <> pretty inst) . midiPatch
        =<< getInstrument inst

lookupMidiConfig :: M m => ScoreT.Instrument -> m (Maybe Patch.Config)
lookupMidiConfig inst = justm (lookupBackend inst) $ \case
    Midi _ config -> pure $ Just config
    _ -> pure Nothing

lookupBackend :: M m => ScoreT.Instrument -> m (Maybe Backend)
lookupBackend inst = justm (lookupInstrument inst) $
    pure . Just . instBackend

lookupInstrument :: M m => ScoreT.Instrument -> m (Maybe ResolvedInstrument)
lookupInstrument inst = do
    uiState <- Ui.get
    db <- gets $ configInstrumentDb . stateConfig
    case Ui.allocation inst #$ uiState of
        Nothing -> pure Nothing
        Just alloc -> case resolveInstrument db alloc of
            Left err -> do
                -- This is a broken allocation, so I should log it at least.
                Log.warn $ "lookup " <> pretty inst <> ": " <> err
                pure Nothing
            Right val -> pure (Just val)

getInstrument :: (HasCallStack, M m) => ScoreT.Instrument
    -> m ResolvedInstrument
getInstrument inst = require ("instrument not found: " <> pretty inst)
    =<< lookupInstrument inst

getLookupInstrument :: M m
    => m (ScoreT.Instrument -> Maybe ResolvedInstrument)
getLookupInstrument = fmap (either (const Nothing) Just .) $
    stateLookupInstrument <$> Ui.get <*> get
    -- This throws away the Left error just because that's what its callers all
    -- happen to want.

stateLookupInstrument :: Ui.State -> State -> ScoreT.Instrument
    -> Either Error ResolvedInstrument
stateLookupInstrument uiState cmdState = memoizedInstrument
    (UiConfig.configAllocations (Ui.stateConfig uiState))
    (configInstrumentDb (stateConfig cmdState))

-- | This memoizes instrument resolution in case you're going to do it a lot.
-- 'resolve_instrument' has to merge some things so it's not exactly free.
-- The spine-strict Map makes this less efficient for one-off lookups, but so
-- far all uses are mapping the lookup across many events.
--
-- Of course, the memoization only works as long as the memo table persists,
-- which should happen if you use 'get_lookup_instrument' and reuse the
-- function it returns.
memoizedInstrument :: UiConfig.Allocations -> InstrumentDb
    -> ScoreT.Instrument -> Either Error ResolvedInstrument
memoizedInstrument (UiConfig.Allocations allocs) db = \inst ->
    fromMaybe (Left $ "no alloc for " <> pretty inst) $ Map.lookup inst memo
    where
    memo = resolve <$> allocs
    resolve alloc = resolveInstrument db alloc

-- | See 'ResolvedInstrument'.
resolveInstrument :: InstrumentDb -> UiConfig.Allocation
    -> Either Error ResolvedInstrument
resolveInstrument db alloc = do
    let qualified = UiConfig.allocQualified alloc
    inst <- tryJust ("patch not in db: " <> pretty qualified) $
        instLookup qualified db
    backend <- case (Inst.instBackend inst, UiConfig.allocBackend alloc) of
        (Inst.Midi patch, UiConfig.Midi config) ->
            pure $ Midi patch (Patch.mergeDefaults patch config)
        (Inst.Im patch, UiConfig.Im) -> pure $ Im patch
        (Inst.Sc patch, UiConfig.Sc) -> pure $ Sc patch
        (Inst.Dummy msg1, UiConfig.Dummy msg2) ->
            -- If it's a dummy allocation of a dummy patch, then I can use ""
            -- to use the patch's msg.
            pure $ Dummy $ if msg2 /= "" then msg2 else msg1
        -- 'UiConfig.verify_allocation' should have prevented this.
        (instBackend, allocBackend) -> Left $
            "inconsistent backends: " <> pretty (instBackend, allocBackend)
    pure $ ResolvedInstrument
        { instInstrument = inst -- merge_attr_calls backend inst
        , instQualified = qualified
        , instCommonConfig =
            mergeEnviron (Inst.instCommon inst) (UiConfig.allocConfig alloc)
        , instBackend = backend
        }
    where
    -- Merge instrument default environ with the local Allocation.
    -- I used to use Maybe and replace when it was Just, but it turns out
    -- I definitely want to merge things like inst-top and inst-bottom.
    -- This means I can't delete keys, but if I ever need that I could
    -- do something like map to NotGiven.
    mergeEnviron :: Common.Common InstrumentCode -> Common.Config
        -> Common.Config
    mergeEnviron common = Common.cenviron %= (Common.commonEnviron common <>)

    -- TODO: I'm not sure if I want this or not... now Convert converts all
    -- attrs, while this had the effect of only converting supported ones.
    -- Which seems generally nice, so I'll leave the code here.
    -- Put the attrs the instrument understands in the CallMap as +attr calls.
    -- If there isn't already a higher level call in there, then at least we
    -- don't lose the attrs entirely.
    mergeAttrCalls backend =
        Inst.common#Common.callMap %= (<> attrCalls (instAttrs backend))
        where
        attrCalls attrs = Map.fromList
            [ (attr, Expr.Symbol $ ShowVal.showVal attr)
            | attr <- attrs, attr /= mempty
            ]
        instAttrs = \case
            Midi patch _ ->
                Common.mappedAttributes $ Midi.Patch.patchAttributeMap patch
            Im patch ->
                Common.mappedAttributes $ Im.Patch.patchAttributeMap patch
            Sc _patch -> mempty -- TODO attrs for sc?
            Dummy {} -> mempty

-- ** lookup qualified name

getQualified :: M m => InstT.Qualified -> m Inst
getQualified qualified =
    require ("patch not in db: " <> pretty qualified)
        =<< lookupQualified qualified

getAllocQualified :: M m => UiConfig.Allocation -> m Inst
getAllocQualified = getQualified . UiConfig.allocQualified

-- | Look up an instrument that might not be allocated.
lookupQualified :: M m => InstT.Qualified -> m (Maybe Inst)
lookupQualified qualified = do
    db <- gets (configInstrumentDb . stateConfig)
    pure $ instLookup qualified db

getLookupInst :: State -> InstT.Qualified -> Maybe Inst
getLookupInst state qualified =
    instLookup qualified (configInstrumentDb (stateConfig state))

getLookupInstEnv :: State -> InstT.Qualified -> Maybe REnv.Environ
getLookupInstEnv state =
    fmap (Common.commonEnviron . Inst.instCommon) . getLookupInst state

instLookup :: InstT.Qualified -> InstrumentDb -> Maybe Inst
instLookup qualified db
    | qualified == InstT.dummy =
        Just $ Inst.Inst (Inst.Dummy "") (Common.common emptyCode)
    | otherwise = Inst.lookup qualified db

-- ** misc

getWdevState :: M m => m WriteDeviceState
getWdevState = gets stateWdevState

modifyWdevState :: M m => (WriteDeviceState -> WriteDeviceState) -> m ()
modifyWdevState f = modify $ \st ->
    st { stateWdevState = f (stateWdevState st) }

deriveImmediately :: M m => [BlockId] -> m ()
deriveImmediately blockIds = modify $ \st -> st { stateDeriveImmediately =
    Set.fromList blockIds <> stateDeriveImmediately st }

inflictDamage :: M m => Derive.ScoreDamage -> m ()
inflictDamage damage = modifyPlayState $ \st -> st
    { stateCurrentPerformance = Map.map inflict (stateCurrentPerformance st)
    }
    where inflict perf = perf { perfDamage = damage <> perfDamage perf }

-- | Cause a block to rederive even if there haven't been any edits on it.
inflictBlockDamage :: M m => BlockId -> m ()
inflictBlockDamage blockId = inflictDamage $ mempty
    { Derive.sdamageBlocks = Set.singleton blockId }

-- | Cause a track to rederive even if there haven't been any edits on it.
inflictTrackDamage :: M m => BlockId -> TrackId -> m ()
inflictTrackDamage blockId trackId = inflictDamage $ mempty
    { Derive.sdamageTracks = Map.singleton trackId Ranges.everything
    , Derive.sdamageTrackBlocks = Set.singleton blockId
    }

-- ** EditState

modifyEditState :: M m => (EditState -> EditState) -> m ()
modifyEditState f = modify $ \st -> st { stateEdit = f (stateEdit st) }

-- | At the Ui level, the edit box is per-block, but I use it to indicate edit
-- mode, which is global.  So it gets stored in Cmd.State and must be synced
-- with new blocks.
setEditBox :: M m => Block.Box -> Block.Box -> m ()
setEditBox skel track = do
    modifyEditState $ \st -> st { stateEditBox = (skel, track) }
    blockIds <- Ui.allBlockIds
    forM_ blockIds $ \bid -> Ui.setEditBox bid skel track

isValEdit :: M m => m Bool
isValEdit = (== ValEdit) <$> gets (stateEditMode . stateEdit)

isKbdEntry :: M m => m Bool
isKbdEntry = gets (stateKbdEntry . stateEdit)

setNoteText :: M m => Text -> m ()
setNoteText text = do
    modifyEditState $ \st -> st { stateNoteText = text }
    setStatus Config.statusNoteText $
        if Text.null text then Nothing else Just text

getInstrumentAttributes :: M m => ScoreT.Instrument -> m Attrs.Attributes
getInstrumentAttributes inst = fromMaybe mempty <$>
    gets (Map.lookup inst . stateInstrumentAttributes . stateEdit)

setInstrumentAttributes :: M m => ScoreT.Instrument -> Attrs.Attributes
    -> m ()
setInstrumentAttributes inst attrs = modifyEditState $ \st -> st
    { stateInstrumentAttributes =
        Map.insert inst attrs (stateInstrumentAttributes st)
    }


-- * util

-- | Give a name to a Cmd.  The name is applied when the cmd returns so the
-- names come out in call order, and it doesn't incur overhead for cmds that
-- abort.
name :: M m => Text -> m a -> m a
name s cmd = cmd <* modify (\st -> st
    { stateHistoryCollect = (stateHistoryCollect st)
        { stateCmdNames = s : stateCmdNames (stateHistoryCollect st) }
    })

-- | Like 'name', but also set the 'state_suppress_edit'.  This will suppress
-- history recording until the edit mode changes from the given one.
suppressHistory :: M m => EditMode -> Text -> m a -> m a
suppressHistory mode name cmd = cmd <* modify (\st -> st
    { stateHistoryCollect = (stateHistoryCollect st)
        { stateCmdNames = name : stateCmdNames (stateHistoryCollect st)
        , stateSuppressEdit = Just mode
        }
    })

-- | Log an event so that it can be clicked on in logview.
logEvent :: BlockId -> TrackId -> Event.Event -> Text
logEvent blockId trackId event =
    Stack.logUiFrame (Just blockId, Just trackId, Just (Event.range event))

-- | Turn off all sounding notes, reset controls.
-- TODO clear out WriteDeviceState?
allNotesOff :: M m => m ()
allNotesOff = mapM_ writeThru
    [ MidiThru $ Midi.Interface.AllNotesOff 0
    , MidiThru $ Interface.resetControls 0
    , ImThru Thru.Stop
    ]
