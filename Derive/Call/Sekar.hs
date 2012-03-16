-- | Pattern based derivation in the Javanese sekaran tradition.
--
-- TODO This is merely a proof of concept.  Sekaran could be implemented a lot
-- of ways, but I'll have to wait to see which ones are most compositionally
-- useful.  It would definitely be more natural, though, if the notation marked
-- the seleh instead of the first note.  But that would require either
-- a preproc pass or deriving the notes backwards.
module Derive.Call.Sekar where
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Note as Note
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required)
import qualified Derive.Derive as Derive


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("sekar", c_sekar)
    ]

-- | Plain sekaran derivation.
--
-- [pattern /String/] Apply this pattern to the encompassed notes.  The pattern
-- is documented by 'make_pattern'.
c_sekar :: Derive.NoteCall
c_sekar = Derive.stream_generator "sekar" $ \args ->
    CallSig.call1 args (required "pattern") $ \pattern -> do
        pattern <- make_pattern pattern
        Note.place_at (Args.range args) $
            sekar (concat (Note.sub_events args)) pattern

-- | [(index of note, is rest)]
type Pattern = [(Int, Bool)]

-- | Lowercase letters represent a deriver starting from @a@, uppercase ones
-- represent a rest of the duration of that deriver.  E.g. @abAb@.
make_pattern :: String -> Derive.Deriver Pattern
make_pattern pattern = do
    when (null pattern || any (not . a_to_z . Char.toLower) pattern) $
        Derive.throw $ "pattern chars must be a-z: " ++ show pattern
    return [(fromEnum (Char.toLower c) - fromEnum 'a', Char.isUpper c)
        | c <- pattern]
    where a_to_z c = 'a' <= c && c <= 'z'

sekar :: [Note.Event] -> Pattern -> [Note.Event]
sekar notes = Maybe.mapMaybe place . add_starts . Maybe.mapMaybe resolve
    where
    place (start, (dur, Just note)) = Just $ Note.Event start dur note
    place (_, (_, Nothing)) = Nothing
    add_starts notes = zip (scanl (+) 0 (map fst notes)) notes
    resolve (i, is_rest) = case Seq.at notes i of
        Nothing -> Nothing
        Just (Note.Event _ dur note) ->
            Just (dur, if is_rest then Nothing else Just note)
