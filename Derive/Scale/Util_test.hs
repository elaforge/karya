module Derive.Scale.Util_test where
import Control.Monad

import Util.Test
import qualified Perform.Pitch as Pitch

import qualified Derive.Scale.Util as Util


test_split_join_note = do
    let pairs =
            [ ("4", Just ("4", 0))
            , ("4-1", Just ("4", -0.01))
            , ("4+126", Just ("4", 1.26))
            , ("-1+1", Just ("-1", 0.01))
            , ("-1c#-1", Just ("-1c#", -0.01))
            , ("+10", Just ("+10", 0))
            , ("a+b", Nothing)
            , ("c-", Nothing)
            ]
    forM_ pairs $ \(note, split) -> do
        let n = Pitch.Note note
        equal (Util.split_note n) split
        case split of
            Just (d, frac) -> equal (Util.join_note d frac) n
            _ -> return ()
