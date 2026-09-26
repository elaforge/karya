-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond (
    -- * derive
    deriveBlock, derive, lookupKey
    -- * compile
    , Movement, extractMovements, explicitMovements
    , compileLy, convert, lyFilename
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy.IO

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Processes
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.C.Prelude.Block as C.Block
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.Symbols as Symbols

import qualified Perform.Lilypond as Lilypond
import qualified Perform.Lilypond.Constants as Lilypond.Constants
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Pitch as Pitch

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * derive

deriveBlock :: Cmd.M m => BlockId -> m Derive.Result
deriveBlock blockId = do
    -- Make sure a bad block id will fail right away.
    _ <- Ui.getBlock blockId
    derive $ C.Block.evalRootBlock blockId

-- | Run a derivation in lilypond context, which will cause certain calls to
-- behave differently.
derive :: Cmd.M m => Derive.NoteDeriver -> m Derive.Result
derive deriver = do
    config <- Ui.config#UiConfig.lilypond <#> Ui.get
    uiState <- Ui.get
    (constant, aliases) <-
        PlayUtil.getConstant (addLyGlobal uiState) mempty mempty
    pure $ Derive.extractResult $
        Derive.derive (setTempo constant)
            (setMode config (PlayUtil.initialDynamic aliases))
            (Derive.withScopes lilypondScope deriver)
    where
    setTempo state = state
        { Derive.stateUi = Ui.config#UiConfig.default_#UiConfig.tempo #= 1 $
            Derive.stateUi state
        }
    setMode config state = state { Derive.stateMode = Derive.Lilypond config }
    addLyGlobal = Ui.config#UiConfig.allocationsMap
        %= Map.insert Lilypond.Constants.lyGlobal allocation
    allocation = UiConfig.allocation Lilypond.Constants.lyQualified
        (UiConfig.Dummy "")

-- | Override a few calls with lilypond versions.
lilypondScope :: Derive.Scopes -> Derive.Scopes
lilypondScope =
    Derive.sGenerator#Derive.sNote
        %= Derive.addPriority Derive.PrioOverride cmap
    where
    cmap = mempty
        { Derive.callMap = Map.fromList
            [(Symbols.nullNote, note), (Symbols.defaultNote, note)]
        }
    -- Turn off the behaviour where staccato shortens the note, since that's
    -- already implicit when you see the dot.
    note = Note.noteCall "" "" mempty
        (Note.defaultNote Note.noDurationAttributes)

lookupKey :: Cmd.Performance -> Pitch.Key
lookupKey perf =
    fromMaybe Twelve.defaultKey $ msum $ map (lookup . Derive.stateEnviron) $
        Map.elems (Msg.perfTrackDynamic perf)
    where
    lookup environ = case Env.getVal EnvKey.key environ of
        Right key -> Just (Pitch.Key key)
        Left _ -> Nothing

-- * compile

type Movement = (Lilypond.Title, [Score.Event])

-- | Generate lilypond code.  If there are movement divisions, they will
-- be extracted from the events.
extractMovements :: Lilypond.Config -> Lilypond.Title -> [Score.Event]
    -> (Either Log.Msg Lazy.Text, [Log.Msg])
extractMovements config title scoreEvents = (output, logs)
    where
    (lyEvents, logs) = convert config scoreEvents
    output = Lilypond.lyFile config title <$>
        Lilypond.extractMovements config lyEvents

-- | Generate lilypond from an explicit list of movements.
explicitMovements :: Lilypond.Config -> Lilypond.Title -> [Movement]
    -> (Either Log.Msg Lazy.Text, [Log.Msg])
explicitMovements config title movements = (output, logs)
    where
    (result, logs) = convertMovements config movements
    output = Lilypond.lyFile config title <$> result

convertMovements :: Lilypond.Config -> [Movement]
    -> (Either Log.Msg [Lilypond.Movement], [Log.Msg])
convertMovements config movements =
    (Lilypond.explicitMovements config (zip (map fst movements) mvtEvents),
        concat logs)
    where (mvtEvents, logs) = unzip (map (convert config . snd) movements)

compileLy :: FilePath -> Lazy.Text -> IO ()
compileLy filename text = do
    Directory.createDirectoryIfMissing True (FilePath.takeDirectory filename)
    Lazy.IO.writeFile filename text
    void $ Thread.start $ Util.Processes.call
        "lilypond" ["-o", FilePath.dropExtension filename, filename]

convert :: Lilypond.Config -> [Score.Event] -> ([Lilypond.Event], [Log.Msg])
convert config scoreEvents =
    (Convert.quantize (Lilypond.configQuantize config) events, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert config scoreEvents

lyFilename :: Cmd.M m => Lilypond.Title -> m FilePath
lyFilename title = do
    dir <- Cmd.require "ly_filename: no save dir"
        =<< Cmd.gets Cmd.stateSaveDir
    pure $ dir </> "ly" </> untxt (clean title) ++ ".ly"
    where
    clean = Text.map replace . Text.toLower
    replace '/' = '-'
    replace ' ' = '-'
    replace c = c
