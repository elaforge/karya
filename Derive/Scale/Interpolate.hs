-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Interpolate between two different scales.
module Derive.Scale.Interpolate where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = scaleMake $ \env (Scale.LookupScale lookup) -> do
    (fromId, toId) <- environFromTo env
    let find msg scaleId = fromMaybe (Left $ DeriveT.PitchError msg) $
            lookup env2 scaleId
        -- This should avoid an infinite loop if from_id is itself
        -- interpolate.
        env2 = Env.delete scaleFrom $ Env.delete scaleTo env
    from <- find ("from scale " <> pretty fromId) fromId
    to <- find ("to scale " <> pretty toId) toId
    pure $ make from to
    where
    scaleMake = (:[]) . Scale.Make scaleId ("same as from scale", doc)

make :: Scale.Scale -> Scale.Scale -> Scale.Scale
make from to = Scale.Scale
    { scaleId = scaleId
    , scalePattern = "same as `from` scale"
    , scaleSymbols = []
    , scaleTransposers =
        Scale.scaleTransposers from <> Scale.scaleTransposers to
    , scaleRead = Scale.scaleRead from
    , scaleShow = Scale.scaleShow from
    , scaleBottom = Scale.scaleBottom from
    , scaleLayout = Scale.scaleLayout from
    , scaleTranspose = Scale.scaleTranspose from
    , scaleEnharmonics = Scale.scaleEnharmonics from
    , scaleNoteToCall = toCall
    , scaleInputToNote = toNote
    , scaleInputToNn = Scales.computedInputToNn toNote toCall
    , scaleCallDoc = doc
    }
    where
    toCall = noteToCall from to
    toNote = Scale.scaleInputToNote from

doc :: Derive.DocumentedCall
doc = Derive.extractDoc $ interpolatedDegree dummy dummy
    where
    dummy = Derive.valCall Module.scale "dummy" mempty "" $
        Sig.call0 $ \_args -> pure (0 :: Double)

scaleId :: Pitch.ScaleId
scaleId = "interpolate"

scaleAt :: ScoreT.Control
scaleAt = "scale-at"

noteToCall :: Scale.Scale -> Scale.Scale -> Pitch.Note -> Maybe Derive.ValCall
noteToCall from to note =
    interpolatedDegree <$> Scale.scaleNoteToCall from note
        <*> Scale.scaleNoteToCall to note

interpolatedDegree :: Derive.ValCall -> Derive.ValCall -> Derive.ValCall
interpolatedDegree from to = Derive.valCall Module.scale "pitch" mempty
    ("Emit a pitch between two scales. The scales are in the "
    <> doc scaleFrom <> " and " <> doc scaleTo
    <> " environ values, and keys are from " <> doc keyFrom <> " and "
    <> doc keyTo <> ". If " <> doc scaleTo <> " isn't set, it defaults to "
    <> doc scaleFrom <> ". The " <> doc scaleAt
    <> " control ranges from 0 to 1\
    \ and controls the interpolation between the scales. For this to work, the\
    \ scales must have the same degree names, since there's no way to manually\
    \ specify a correspondence between scale degrees."
    ) $ Sig.call (Sig.manyVals "arg" "passed to `from` and `to` scales") $
    \_vals args -> do
        start <- Args.realStart args
        n <- fromMaybe 0 <$> Derive.untypedControlAt scaleAt start
        let apply key = renameEnviron key EnvKey.key
                . Eval.applyPitch (Args.start args)
        if n <= 0 then apply keyFrom from
            else if n >= 1 then apply keyTo to
            else do
                p1 <- apply keyFrom from
                p2 <- apply keyTo to
                pure $ Pitches.interpolated p1 p2 n
    where
    doc :: ShowVal.ShowVal a => a -> Doc.Doc
    doc = ShowVal.doc

renameEnviron :: Env.Key -> Env.Key -> Derive.Deriver a -> Derive.Deriver a
renameEnviron from to deriver = do
    maybeVal :: Maybe DeriveT.Val <- Derive.lookupVal from
    maybe id (Derive.withVal to) maybeVal deriver


-- * util

environFromTo :: Env.Environ
    -> Either DeriveT.PitchError (Pitch.ScaleId, Pitch.ScaleId)
environFromTo env = do
    from <- Scales.readEnviron (Just . Expr.strToScaleId) Nothing
        scaleFrom env
    to <- Scales.readEnviron (Just . Expr.strToScaleId) (Just from)
        scaleTo env
    pure (from, to)

keyFrom, keyTo :: Env.Key
keyFrom = "key-from"
keyTo = "key-to"

scaleFrom, scaleTo :: Env.Key
scaleFrom = "scale-from"
scaleTo = "scale-to"

{- Notes from the implementation: NOTE [make-scale]

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
