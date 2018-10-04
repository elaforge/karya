{-# LANGUAGE ExistentialQuantification #-}
module Synth.Sampler.Patch2 where
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Note as Note

import Global


type Error = Text

db :: FilePath -> [Patch] -> Db
db rootDir patches = Db
    { _rootDir = rootDir
    , _patches = Map.fromList $ Seq.key_on _name patches
    }

data Db = Db {
    -- | Base directory for samples.  '_load' gets rootDir </> patchName.
    _rootDir :: !FilePath
    , _patches :: !(Map Note.PatchName Patch)
    }

data Patch = forall loaded. Patch {
    _name :: Note.PatchName
    , _load :: FilePath -> IO (Either Error loaded)
    , _convert :: loaded -> Note.Note -> Either Error Sample.Sample
    , _karyaPatch :: ImInst.Patch
    }

load :: FilePath -> Patch
    -> IO (Either Error (Note.Note -> Either Error Sample.Sample))
load rootDir patch@(Patch { _load = load, _convert = convert })  =
    load (rootDir </> untxt (_name patch)) >>= \case
        Left err -> return $ Left err
        Right loaded -> return $ Right $ convert loaded
