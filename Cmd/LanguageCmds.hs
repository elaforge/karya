{- | Helper functions to be imported into LanguageEnviron.  LanguageEnviron
must be interpreted since it's the "top level" module, so I put the library
of commands in here.

Of course, lang commands can use anything in scope in LanguageEnviron, not just
these helpers.  That includes all the various cmd_* functions used by the
keybindings.  Also, keybindings can be invoked directly with the 'keybinding'
helper.

The difference is that the functions here generally take simpler types like
strings, if they will just be wrapped up in a newtype, like BlockId.  Also,
there are sometimes *_d variants that will default some of the values.

The various show_* functions print out state generally in a 'show' format, but
not necessarily.  It's designed to be for human reading and may drop
"uninteresting" data.

    --- old notes ---
    Use cases:

        send a simple cmd, "create block"
    I should be able to just type the command + args with minimum fuss.

        create a one-off compound command, "create 3 copies of this block"
    Same as above, but it runs in the Cmd monad, and saves as single undo.

        create a compound command and save it, doesn't get run
    I should just edit a plain module, and tell the UI to include it in its
    language namespace.  This way I use any editor, and commands use the module
    namespace, and are easily sharable and distributable by normal means.

        add a custom keybinding
    Modify the keymap, globally, per block, or per track.

        show blocks without views

        attach a derivation to a track, or set of tracks, or block

        reorder tracks, without messing up the derivation rules


    For saved cmds to work, all cmds need to run in a namespace that includes
    them.  The UI keeps a list of directories, and when a one shot command is
    compiled, it imports all modules from those directories.  It can also take
    a signal to recompile and rebind the keymaps.


        Block / derivation management

    Because each step in derivation may be exposed as a Block, there needs to be
    some kind of framework for organizing them.  Also, the order in which they
    happen is relevant.

    tempo track -> multiply times of everything in the block
    note track -> substitute sub-blocks, realize note structures (tuplets),
        realize scale

    contorller1
    controller2
        -> combine with controller2, apply static transformation

    ----
    derived notes
    controller1 + controller2
        -> NoteList that gets rendered to Midi / score




        Ways to display a derivation

    A neighboring track in the same block.

    Another block view.

    "Hidden" behind the current block / track, press a key to "descend" one
    level.

    Not displayed at all, but played of course.

-}
module Cmd.LanguageCmds where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import Text.Printf

-- import Ui.Types

import qualified Util.Seq as Seq
-- import qualified Util.Log as Log
import Util.Pretty as Pretty
import qualified Util.PPrint as PPrint

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
-- import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
-- import qualified Cmd.Save as Save
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.InstrumentDb as InstrumentDb

import qualified App.Config as Config


type Cmd = Cmd.CmdT Identity.Identity String

-- * util

-- | Showable, with a short name for convenience.
-- data S = forall a. Show a => S a

-- | pprint does ok for records, but it doesn't work so well if I want to print
-- part of the record, or change the types.  A real record system for haskell
-- would probably fix this.
show_record :: [(String, String)] -> String
show_record = concatMap (\(k, v) ->
    printf "%s:\n%s\n" k (indent_lines (Seq.strip v)))
indent_lines = Seq.rstrip . unlines . map (indent++) . lines
indent = "  "

show_list :: [String] -> String
show_list xs = concatMap (\(i, x) -> printf "%d. %s\n" i x) (Seq.enumerate xs)


_cmd_state :: (Cmd.State -> a) -> Cmd.CmdT Identity.Identity a
_cmd_state = flip fmap Cmd.get_state
-- | Various Cmd return types: nothing, pprinted val, and showed val.
r_ = (>> return "")
rp m = fmap PPrint.pshow m
rs m = fmap show m

-- * show / modify cmd state

show_save_file :: Cmd
show_save_file = _cmd_state (show . Cmd.state_default_save_file)
set_save_file :: String -> Cmd
set_save_file s = r_ $
    Cmd.modify_state $ \st -> st { Cmd.state_default_save_file = s }

show_midi_config :: Cmd
show_midi_config = rp $
    _cmd_state (Instrument.config_alloc . Cmd.state_midi_instrument_config)

assign_instrument :: String -> Instrument.Addr -> Cmd
assign_instrument inst_name addr = r_ $ do
    inst <- case InstrumentDb.lookup (Score.Instrument inst_name) of
        Just (InstrumentDb.Midi midi_inst) -> return midi_inst
        Just inst -> State.throw $
            "you can't allocate a midi address to non-midi instrument "
            ++ show inst
        Nothing ->
            State.throw $ "instrument " ++ show inst_name ++ " not found"
    alloc <- fmap (Instrument.config_alloc . Cmd.state_midi_instrument_config)
        Cmd.get_state
    let alloc2 = Map.insert addr inst alloc
    Cmd.modify_state $ \st ->
        st { Cmd.state_midi_instrument_config = Instrument.Config alloc2 }

show_step :: Cmd
show_step = rs $ _cmd_state Cmd.state_step

set_step :: TimeStep.TimeStep -> Cmd
set_step step = r_ $ Cmd.modify_state $ \st -> st { Cmd.state_step = step }

show_octave :: Cmd
show_octave = rs $ _cmd_state Cmd.state_kbd_entry_octave
set_octave :: Int -> Cmd
set_octave n = r_ $ Cmd.modify_state $ \st ->
    st { Cmd.state_kbd_entry_octave = n }

-- * load / save

-- Need to run in IO, not Identity for this.
-- I think I could have lang run in IO, but is it worth it just for this?
-- I don't see why not.  TODO
-- load, save :: Maybe String -> Cmd
-- load fn = r_ $ Save.cmd_load fn
-- save fn = r_ $ Save.cmd_save fn

-- undo, redo

-- * show / modify UI state

vid = Block.ViewId
bid = Block.BlockId
_bid (Block.BlockId s) = s
rid = Ruler.RulerId
tid = Track.TrackId

show_state :: Cmd
show_state = do
    (State.State views blocks tracks rulers) <- State.get
    let f m = PPrint.pshow (Map.keys m)
    return $ show_record [("views", f views), ("blocks", f blocks),
        ("tracks", f tracks), ("rulers", f rulers)]

-- ** views

show_views :: Cmd
show_views = rp $ fmap (Map.keys . State.state_views) State.get

create_view :: String -> Block.View -> Cmd
create_view view_id view = r_ $ State.create_view view_id view

create_view_d :: String -> String -> Cmd
create_view_d view_id block_id = r_ $ do
    rect <- fmap (find_rect Config.view_size . map Block.view_rect . Map.elems
        . State.state_views) State.get
    State.create_view view_id
        (Block.view (bid block_id) rect Config.view_config)

-- TODO I also need the screen dimensions to do this right.  Before I go
-- too far here, though, I'll want to think about proper window manager stuff.
-- If I just allow the placement function to be passed as an arg...
find_rect (w, h) rects = Block.Rect (right, bottom) (w, h)
    where
    right = maximum $ 0 : map Block.rect_right rects
    bottom = 10

destroy_view :: String -> Cmd
destroy_view view_id = r_ $ State.destroy_view (vid view_id)

-- ** blocks

get_active_block = fmap Block.view_block
    (State.get_view =<< Cmd.get_active_view)

show_block :: Maybe String -> Cmd
show_block maybe_block_id = do
    block_id <- maybe get_active_block return
        (fmap Block.BlockId maybe_block_id)
    Block.Block { Block.block_title = title, Block.block_ruler_track = ruler,
        Block.block_tracks = tracks, Block.block_deriver = deriver }
            <- State.get_block block_id
    return $ show_record
        [ ("title", title)
        , ("ruler", show ruler)
        , ("tracks", show_list (map (show . fst) tracks))
        , ("deriver", show deriver)
        ]

create_block block_id block = r_ $ State.create_block block_id block
create_block_d block_id ruler_id = create_block
    block_id (Block.block "" Config.block_config (ruler ruler_id) [])

set_deriver :: String -> Maybe String -> Cmd
set_deriver block_id maybe_deriver_id = r_ $ do
    State.modify_block (bid block_id) $ \block ->
        block { Block.block_deriver = fmap Block.DeriverId maybe_deriver_id }

set_deriver_d :: Maybe String -> Cmd
set_deriver_d maybe_deriver_id = do
    block_id <- get_active_block
    set_deriver (_bid block_id) maybe_deriver_id

-- ** tracks

-- | Some helpers to make it easier to make TracklikeIds.
track track_id ruler_id = Block.TId (tid track_id) (rid ruler_id)
ruler ruler_id = Block.RId (rid ruler_id)
divider color = Block.DId (Block.Divider color)

insert_track block_id tracknum track width = r_ $ do
    State.insert_track (bid block_id) tracknum track width
remove_track block_id tracknum = State.remove_track (bid block_id) tracknum

-- ** rulers

-- TODO decide on strings or IDs.  IDs are more composable, but strings are
-- more convenient when called directly.

show_ruler :: Ruler.RulerId -> Cmd
show_ruler ruler_id = do
    (Ruler.Ruler mlists bg show_names use_alpha full_width) <-
        State.get_ruler ruler_id
    return $ show_record
        [ ("bg", show bg)
        , ("show_names", show show_names), ("use_alpha", show use_alpha)
        , ("full_width", show full_width)
        , ("marklists", show_list (map fst mlists))
        ]

show_marklist :: Ruler.RulerId -> Ruler.MarklistName -> Cmd
show_marklist ruler_id marklist_name = do
    ruler <- State.get_ruler ruler_id
    mlist <- case lookup marklist_name (Ruler.ruler_marklists ruler) of
        Nothing -> State.throw $
            "no marklist " ++ show marklist_name ++ " in " ++ show ruler_id
        Just mlist -> return mlist
    return $ show_list $
        map (\(pos, m) -> printf "%s - %s" (show pos) (pretty m))
            (Ruler.forward mlist (TrackPos 0))

-- * show / modify keymap

-- | Run the Cmd that is bound to the given KeySpec, if there is one.
-- keymap :: Keymap.KeySpec -> Cmd
