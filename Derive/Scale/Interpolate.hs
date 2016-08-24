-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Interpolate between two different scales.
module Derive.Scale.Interpolate where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = scale_make $ \env (Scale.LookupScale lookup) -> do
    (from_id, to_id) <- environ_from_to env
    let find msg scale_id = fromMaybe (Left $ BaseTypes.PitchError msg) $
            lookup env2 (Scale.LookupScale lookup) scale_id
        -- This should avoid an infinite loop if from_id is itself
        -- interpolate.
        env2 = Env.delete scale_from $ Env.delete scale_to env
    from <- find ("from scale " <> pretty from_id) from_id
    to <- find ("to scale " <> pretty to_id) to_id
    return $ make from to
    where
    scale_make = (:[]) . Scale.Make scale_id ("same as from scale", doc)

make :: Scale.Scale -> Scale.Scale -> Scale.Scale
make from to = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = "same as `from` scale"
    , scale_symbols = []
    , scale_transposers =
        Scale.scale_transposers from <> Scale.scale_transposers to
    , scale_read = Scale.scale_read from
    , scale_show = Scale.scale_show from
    , scale_bottom = Scale.scale_bottom from
    , scale_layout = Scale.scale_layout from
    , scale_transpose = Scale.scale_transpose from
    , scale_enharmonics = Scale.scale_enharmonics from
    , scale_note_to_call = to_call
    , scale_input_to_note = to_note
    , scale_input_to_nn = Scales.computed_input_to_nn to_note to_call
    , scale_call_doc = doc
    }
    where
    to_call = note_to_call from to
    to_note = Scale.scale_input_to_note from

doc :: Derive.DocumentedCall
doc = Derive.extract_val_doc $ interpolated_degree dummy dummy
    where
    dummy = Derive.val_call Module.scale "dummy" mempty "" $
        Sig.call0 $ \_args -> return (0 :: Double)

scale_id :: Pitch.ScaleId
scale_id = "interpolate"

scale_at :: Score.Control
scale_at = "scale-at"

note_to_call :: Scale.Scale -> Scale.Scale -> Pitch.Note -> Maybe Derive.ValCall
note_to_call from to note =
    interpolated_degree <$> Scale.scale_note_to_call from note
        <*> Scale.scale_note_to_call to note

interpolated_degree :: Derive.ValCall -> Derive.ValCall -> Derive.ValCall
interpolated_degree from to = Derive.val_call Module.scale "pitch" mempty
    ("Emit a pitch between two scales. The scales are in the "
    <> doc scale_from <> " and " <> doc scale_to
    <> " environ values, and keys are from " <> doc key_from <> " and "
    <> doc key_to <> ". If " <> doc scale_to <> " isn't set, it defaults to "
    <> doc scale_from <> ". The " <> doc scale_at
    <> " control ranges from 0 to 1\
    \ and controls the interpolation between the scales. For this to work, the\
    \ scales must have the same degree names, since there's no way to manually\
    \ specify a correspondence between scale degrees."
    ) $ Sig.call (Sig.many_vals "arg" "passed to `from` and `to` scales") $
    \_vals args -> do
        start <- Args.real_start args
        n <- fromMaybe 0 <$> Derive.untyped_control_at scale_at start
        let apply key = rename_environ key EnvKey.key
                . Eval.apply_pitch (Args.start args)
        let typecheck = Typecheck.typecheck "interpolated_degree"
                (Args.start args)
        if n <= 0 then apply key_from from
            else if n >= 1 then apply key_to to
            else do
                p1 <- typecheck =<< apply key_from from
                p2 <- typecheck =<< apply key_to to
                return $ Typecheck.to_val $ Pitches.interpolated p1 p2 n
    where
    doc :: ShowVal.ShowVal a => a -> Doc.Doc
    doc = ShowVal.doc

rename_environ :: Env.Key -> Env.Key -> Derive.Deriver a -> Derive.Deriver a
rename_environ from to deriver = do
    maybe_val :: Maybe BaseTypes.Val <- Derive.lookup_val from
    maybe id (Derive.with_val to) maybe_val deriver


-- * util

environ_from_to :: Env.Environ
    -> Either BaseTypes.PitchError (Pitch.ScaleId, Pitch.ScaleId)
environ_from_to env = do
    from <- Scales.read_environ (Just . BaseTypes.sym_to_scale_id) Nothing
        scale_from env
    to <- Scales.read_environ (Just . BaseTypes.sym_to_scale_id) (Just from)
        scale_to env
    return (from, to)

key_from, key_to :: Env.Key
key_from = "key-from"
key_to = "key-to"

scale_from, scale_to :: Env.Key
scale_from = "scale-from"
scale_to = "scale-to"

{- Notes from the implementation:

    Scale with arguments: *interpolate scale-from=from scale-to=to

    I don't think I can use environ vals, because all the various scale
    functions don't have access to it.

    If I want a scale with arguments, it winds up being like Environ, except
    that I can rely on it being in the track title instead of having to look up
    an environ.  But I would need to generate the scale with a val call, e.g.
    'scale = (interpolate-scale from to)', and now I can't really write it
    directly '*interpolate ..?'.

    The Key is analogous, and its from the environ.  So perhaps all the
    functions that take a Key should take Environ instead.  Everyone who needs
    a Key already makes you look up in the Environ anyway, why not pass the
    whole thing?

    Not just Environ, it also needs LookupScale.

    type LookupScale = Pitch.ScaleId -> Maybe Scale.  Maybe I could use the
    LookupPattern thing for scales, so *interpolate-from-to will have access to
    those and be able to bake them into the scale creation.  This would also
    solve the problem with scale_transposers and scale_layout.

    What about same scale with different keys?  Environ lends itself to that:
    from-scale, from-key, to-scale, to-key.

    Or, I could do both, and pass Environ to LookupScale.  But then the from
    and to scales are fixed when the scale is created.  Actually it's created
    again every time the scale is looked up.  Maybe that's not so great?  But
    actually all it does is make some closures and I'm constantly doing that
    anyway.

    *interpolate.from.to - Otherwise Cmd.get_scale calls all need environ,
    which means they have to look in the performance, which may not be present.
    Doesn't work for keys, but I need key-from key-to anyway.  It's a pretty
    bogus way to pass arguments though.

    Getting from the Environ is not so bad because it's already doing that for
    the scale itself.

    Scale degree calls create a ValCall, but they don't look in environ.  That
    happens on NN conversion, so the environ always comes out of the
    Score.Event, which means that the interpolate scale degree can't influence
    the environ its callees see.

    Shouldn't calls look in the environ and put those values into their
    closure?  Why pass environ to conversion?  I recall it had to do with the
    tuning var.  So in convert, inst_environ goes before the event environ.
    How exactly should inst environ interact?  It should be added to the
    environ as a default.

    I think this had to do with kotekan calls, because it evaluates the pitch,
    and then sets a new instrument.  If I took environ back out then the tuning
    is set, and can't be changed if you switch the instrument.  But perhaps
    that means kotekan should work on derivers, not as a postproc.  I.e. you
    can't just change the instrument of an event by updating
    Score.event_instrument.

    Postproc, e.g. Gangsa.c_unison, could either create a new instrument, e.g.
    Util.pitched_note (event_pitch e) etc.  Or it could just know about tuning,
    and explicitly apply that to the Score.event_pitch.  But I'd need to
    preserve a way to apply environ to pitches, and then it's unclear whether
    it should take priority, etc.  Seems messy.

    ChromaticScales.smap_semis_to_nn needs a key, where that's scale specific
    (e.g.  Environ.tuning).  The thing is, it uses Environ.key too, it just may
    want additional things.

    So, only pitch_nn needs it.  So pass SemisToNn directly to it.

    Except, rederive_event does not work, because I need to actually
    re-evaluate the pitch in order to capture the new environ.  Just pulling it
    from the old event doesn't do that.

    Perhaps 'unison' wanting to be a postproc is wrong in general.  I could
    just derive the whole thing twice.  But then I don't get the behaviour
    where I can apply it to a whole score and only the pasang instruments are
    split.  Also for kempyung I'd have to rederive with +3 transpose and the
    note call has to know to wrap into range.  That means +3 transpose and then
    +kempyung, so if it sees the pitch is out of range, it knows it can
    transpose -3.  What about nyogcag?  It would have to be a note transformer
    since it needs each note separately.

    Ok, so maybe I do want to go back to being able to retune a note.  But
    I don't want to unconditionally replace the environ with the inst's
    environ, because then I can't override it.  It should be as if the event
    was derived with that instrument in the first place.

    So, put Environ back into PitchConfig.  But it's not applied in Convert
    like control signals, instead applied in ScaleDegree and then manually when
    you change the instrument.

    I suppose this means I can't configure a pitch with a transposition...  no
    wait, I can because it applies it directly.  So do the same for environ.
    So interpolate could also do that.  But I want to make sure to not override
    those values later on.  This isn't a problem for controls because they are
    added, but env vals replace.

    So... capture at pitch creation time, but switching instruments overrides
    with the instrument env vars.

    I apply PitchConfig by composing it on to the functions, but that means
    I can't override existing values.  Actually I still can, but a separate
    field seems clearer.
-}
