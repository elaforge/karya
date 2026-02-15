-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The instrument browser is a standalone program to browse the instrument
    database.

    Instruments are in the left pane, and the right pane has information on the
    selected instrument.  A search box above the instrument list accepts
    a simple query language, documneted at 'Search.Query'.

    If you double click on an instrument name, 'choose_instrument' is called on
    the instrument.

    The instrument info is basically just a pretty-printed version of the
    contents of 'Patch.Patch'.

    Some parts of the instrument db may be generated offline, by
    "Instrument.MakeDb".
-}
module Instrument.Browser (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad.State as State

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment
import qualified System.Exit

import qualified Util.Fltk as Fltk
import qualified Util.FltkUtil as FltkUtil
import qualified Util.Lists as Lists
import qualified Util.Network as Network

import qualified App.Config as Config
import qualified App.LoadInstruments as LoadInstruments
import qualified App.Path as Path
import qualified App.ReplProtocol as ReplProtocol

import qualified Cmd.Cmd as Cmd
import qualified Instrument.BrowserC as BrowserC
import qualified Instrument.Inst as Inst
import qualified Instrument.InstDoc as InstDoc
import qualified Instrument.InstT as InstT
import qualified Instrument.Search as Search

import           Global


-- | Send this to the REPL when on a double-click on an instrument.
select_command :: Text
select_command = "LInst.set_instrument"

data Flag = Help | Geometry FltkUtil.Geometry
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , FltkUtil.option Geometry
    ]

default_geometry :: Maybe FltkUtil.Geometry -> (Int, Int, Int, Int)
default_geometry = FltkUtil.xywh 50 50 550 600

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    unless (null args) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")

    db <- LoadInstruments.load =<< Path.get_app_dir
    putStrLn $ "Loaded " ++ show (Inst.size db) ++ " instruments."
    let geometry = Lists.head [g | Geometry g <- flags]
        (x, y, w, h) = default_geometry geometry
    win <- Fltk.run_action $ BrowserC.create x y w h
    let index_db = Db db (Search.make_index db)
    chan <- Fltk.new_channel
    Concurrent.forkFinally (handle_msgs chan win index_db) $ \result -> do
        putStrLn $ "handler thread died: "
            ++ either show (const "no exception")
                (result :: Either Exception.SomeException ())
        Fltk.quit chan
    Fltk.event_loop chan

usage :: String -> IO a
usage msg = do
    putStrLn $ "ERROR: " ++ msg
    putStrLn "usage: browser [ flags ]"
    putStr (GetOpt.usageInfo "" options)
    System.Exit.exitFailure

-- | Bundle a Db along with its search index.
data Db = Db {
    db_db :: Cmd.InstrumentDb
    , db_index :: Search.Index
    }

data State = State {
    state_displayed :: [InstT.Qualified]
    } deriving (Show)

handle_msgs :: Fltk.Channel -> BrowserC.Window -> Db -> IO ()
handle_msgs chan win db = do
    displayed <- liftIO $ process_query chan win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        Fltk.Msg typ text <- liftIO $ STM.atomically $ Fltk.read_msg win
        let qualified = InstT.parse_qualified text
        case typ of
            BrowserC.Select -> liftIO $ show_info chan win db qualified
            BrowserC.Choose -> liftIO $ choose_instrument qualified
            BrowserC.Query -> do
                state <- State.get
                displayed <- liftIO $
                    process_query chan win db (state_displayed state) text
                State.put (state { state_displayed = displayed })
            BrowserC.Unknown c -> liftIO $
                putStrLn $ "unknown msg type: " ++ show c

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
show_info :: Fltk.Channel -> BrowserC.Window -> Db -> InstT.Qualified
    -> IO ()
show_info chan win db qualified = Fltk.action chan $ BrowserC.set_info win info
    where
    info = fromMaybe ("not found: " <> InstT.show_qualified qualified) $ do
        let InstT.Qualified synth_name inst_name = qualified
        synth <- Inst.lookup_synth synth_name (db_db db)
        inst <- Map.lookup inst_name (Inst.synth_insts synth)
        return $ InstDoc.info_of qualified (Inst.synth_doc synth) inst tags
    tags = fromMaybe [] $ Search.tags_of (db_index db) qualified

-- | Send the chosen instrument to the sequencer.  This will send
-- @change_instrument \"synth/inst\"@ to the REPL port.
choose_instrument :: InstT.Qualified -> IO ()
choose_instrument qualified = do
    let cmd = select_command <> " "
            <> showt (InstT.show_qualified qualified)
    Text.IO.putStrLn $ "send: " <> cmd
    response <- query cmd
    unless (Text.null response) $
        Text.IO.putStrLn $ "response: " <> response

query :: Text -> IO Text
query = fmap ReplProtocol.format_result
    . ReplProtocol.query_cmd (Network.Unix Config.repl_socket_name)

-- | Find instruments that match the query, and update the UI incrementally.
process_query :: Fltk.Channel -> BrowserC.Window -> Db -> [InstT.Qualified]
    -> Text -> IO [InstT.Qualified]
process_query chan win db displayed query = do
    let matches = Search.search (db_index db) (Search.parse query)
        diff = Lists.diffIndex (==) displayed matches
    forM_ diff $ \(i, paired) -> case paired of
        Lists.Second inst -> Fltk.action chan $
            BrowserC.insert_line win (i+1) (InstT.show_qualified inst)
        Lists.First _inst -> Fltk.action chan $
            BrowserC.remove_line win (i+1)
        _ -> return ()
    return matches
