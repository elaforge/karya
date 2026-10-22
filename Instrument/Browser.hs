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
selectCommand :: Text
selectCommand = "LInst.set_instrument"

data Flag = Help | Geometry FltkUtil.Geometry
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , FltkUtil.option Geometry
    ]

defaultGeometry :: Maybe FltkUtil.Geometry -> (Int, Int, Int, Int)
defaultGeometry = FltkUtil.xywh 50 50 550 600

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> pure (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    unless (null args) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")

    db <- LoadInstruments.load =<< Path.getAppDir
    putStrLn $ "Loaded " ++ show (Inst.size db) ++ " instruments."
    let geometry = Lists.head [g | Geometry g <- flags]
        (x, y, w, h) = defaultGeometry geometry
    win <- Fltk.runAction $ BrowserC.create x y w h
    let indexDb = Db db (Search.makeIndex db)
    chan <- Fltk.newChannel
    Concurrent.forkFinally (handleMsgs chan win indexDb) $ \result -> do
        putStrLn $ "handler thread died: "
            ++ either show (const "no exception")
                (result :: Either Exception.SomeException ())
        Fltk.quit chan
    Fltk.eventLoop chan

usage :: String -> IO a
usage msg = do
    putStrLn $ "ERROR: " ++ msg
    putStrLn "usage: browser [ flags ]"
    putStr (GetOpt.usageInfo "" options)
    System.Exit.exitFailure

-- | Bundle a Db along with its search index.
data Db = Db
    { dbDb :: Cmd.InstrumentDb
    , dbIndex :: Search.Index
    }

data State = State
    { stateDisplayed :: [InstT.Qualified]
    } deriving (Show)

handleMsgs :: Fltk.Channel -> BrowserC.Window -> Db -> IO ()
handleMsgs chan win db = do
    displayed <- liftIO $ processQuery chan win db [] ""
    flip State.evalStateT (State displayed) $ forever $ do
        Fltk.Msg typ text <- liftIO $ STM.atomically $ Fltk.readMsg win
        let qualified = InstT.parseQualified text
        case typ of
            BrowserC.Select -> liftIO $ showInfo chan win db qualified
            BrowserC.Choose -> liftIO $ chooseInstrument qualified
            BrowserC.Query -> do
                state <- State.get
                displayed <- liftIO $
                    processQuery chan win db (stateDisplayed state) text
                State.put (state { stateDisplayed = displayed })
            BrowserC.Unknown c -> liftIO $
                putStrLn $ "unknown msg type: " ++ show c

-- | Look up the instrument, generate a info sheet on it, and send to the UI.
showInfo :: Fltk.Channel -> BrowserC.Window -> Db -> InstT.Qualified
    -> IO ()
showInfo chan win db qualified = Fltk.action chan $ BrowserC.setInfo win info
    where
    info = fromMaybe ("not found: " <> InstT.showQualified qualified) $ do
        let InstT.Qualified synthName instName = qualified
        synth <- Inst.lookupSynth synthName (dbDb db)
        inst <- Map.lookup instName (Inst.synthInsts synth)
        pure $ InstDoc.infoOf qualified (Inst.synthDoc synth) inst tags
    tags = fromMaybe [] $ Search.tagsOf (dbIndex db) qualified

-- | Send the chosen instrument to the sequencer.  This will send
-- @change_instrument \"synth/inst\"@ to the REPL port.
chooseInstrument :: InstT.Qualified -> IO ()
chooseInstrument qualified = do
    let cmd = selectCommand <> " "
            <> showt (InstT.showQualified qualified)
    Text.IO.putStrLn $ "send: " <> cmd
    response <- query cmd
    unless (Text.null response) $
        Text.IO.putStrLn $ "response: " <> response

query :: Text -> IO Text
query = fmap ReplProtocol.formatResult
    . ReplProtocol.queryCmd True (Network.Unix Config.replSocketName)

-- | Find instruments that match the query, and update the UI incrementally.
processQuery :: Fltk.Channel -> BrowserC.Window -> Db -> [InstT.Qualified]
    -> Text -> IO [InstT.Qualified]
processQuery chan win db displayed query = do
    let matches = Search.search (dbIndex db) (Search.parse query)
        diff = Lists.diffIndex (==) displayed matches
    forM_ diff $ \(i, paired) -> case paired of
        Lists.Second inst -> Fltk.action chan $
            BrowserC.insertLine win (i+1) (InstT.showQualified inst)
        Lists.First _inst -> Fltk.action chan $
            BrowserC.removeLine win (i+1)
        _ -> pure ()
    pure matches
