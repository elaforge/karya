-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond (
    -- * derive
    derive_block, derive, lookup_key
    -- * compile
    , Movement, extract_movements, explicit_movements
    , compile_ly, convert, ly_filename
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

derive_block :: Cmd.M m => BlockId -> m Derive.Result
derive_block block_id = do
    -- Make sure a bad block id will fail right away.
    _ <- Ui.get_block block_id
    derive $ C.Block.eval_root_block block_id

-- | Run a derivation in lilypond context, which will cause certain calls to
-- behave differently.
derive :: Cmd.M m => Derive.NoteDeriver -> m Derive.Result
derive deriver = do
    config <- Ui.config#UiConfig.lilypond <#> Ui.get
    ui_state <- Ui.get
    (constant, aliases) <-
        PlayUtil.get_constant (add_ly_global ui_state) mempty mempty
    return $ Derive.extract_result $
        Derive.derive (set_tempo constant)
            (set_mode config (PlayUtil.initial_dynamic aliases))
            (Derive.with_scopes lilypond_scope deriver)
    where
    set_tempo state = state
        { Derive.state_ui = Ui.config#UiConfig.default_#UiConfig.tempo #= 1 $
            Derive.state_ui state
        }
    set_mode config state = state { Derive.state_mode = Derive.Lilypond config }
    add_ly_global = Ui.config#UiConfig.allocations_map
        %= Map.insert Lilypond.Constants.ly_global allocation
    allocation = UiConfig.allocation Lilypond.Constants.ly_qualified
        (UiConfig.Dummy "")

-- | Override a few calls with lilypond versions.
lilypond_scope :: Derive.Scopes -> Derive.Scopes
lilypond_scope =
    Derive.s_generator#Derive.s_note
        %= Derive.add_priority Derive.PrioOverride cmap
    where
    cmap = mempty
        { Derive.call_map = Map.fromList
            [(Symbols.null_note, note), (Symbols.default_note, note)]
        }
    -- Turn off the behaviour where staccato shortens the note, since that's
    -- already implicit when you see the dot.
    note = Note.note_call "" "" mempty
        (Note.default_note Note.no_duration_attributes)

lookup_key :: Cmd.Performance -> Pitch.Key
lookup_key perf =
    fromMaybe Twelve.default_key $ msum $ map (lookup . Derive.state_environ) $
        Map.elems (Msg.perf_track_dynamic perf)
    where
    lookup environ = case Env.get_val EnvKey.key environ of
        Right key -> Just (Pitch.Key key)
        Left _ -> Nothing

-- * compile

type Movement = (Lilypond.Title, [Score.Event])

-- | Generate lilypond code.  If there are movement divisions, they will
-- be extracted from the events.
extract_movements :: Lilypond.Config -> Lilypond.Title -> [Score.Event]
    -> (Either Log.Msg Lazy.Text, [Log.Msg])
extract_movements config title score_events = (output, logs)
    where
    (ly_events, logs) = convert config score_events
    output = Lilypond.ly_file config title <$>
        Lilypond.extract_movements config ly_events

-- | Generate lilypond from an explicit list of movements.
explicit_movements :: Lilypond.Config -> Lilypond.Title -> [Movement]
    -> (Either Log.Msg Lazy.Text, [Log.Msg])
explicit_movements config title movements = (output, logs)
    where
    (result, logs) = convert_movements config movements
    output = Lilypond.ly_file config title <$> result

convert_movements :: Lilypond.Config -> [Movement]
    -> (Either Log.Msg [Lilypond.Movement], [Log.Msg])
convert_movements config movements =
    (Lilypond.explicit_movements config (zip (map fst movements) mvt_events),
        concat logs)
    where (mvt_events, logs) = unzip (map (convert config . snd) movements)

compile_ly :: FilePath -> Lazy.Text -> IO ()
compile_ly filename text = do
    Directory.createDirectoryIfMissing True (FilePath.takeDirectory filename)
    Lazy.IO.writeFile filename text
    void $ Thread.start $ Util.Processes.call
        "lilypond" ["-o", FilePath.dropExtension filename, filename]

convert :: Lilypond.Config -> [Score.Event] -> ([Lilypond.Event], [Log.Msg])
convert config score_events =
    (Convert.quantize (Lilypond.config_quantize config) events, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert config score_events

ly_filename :: Cmd.M m => Lilypond.Title -> m FilePath
ly_filename title = do
    dir <- Cmd.require "ly_filename: no save dir"
        =<< Cmd.gets Cmd.state_save_dir
    return $ dir </> "ly" </> untxt (clean title) ++ ".ly"
    where
    clean = Text.map replace . Text.toLower
    replace '/' = '-'
    replace ' ' = '-'
    replace c = c
