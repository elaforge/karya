-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
-- | This contains instrument data in common between different backends.
module Instrument.Common where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Util.Serialize as Serialize

import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.REnv as REnv
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Tag as Tag

import           Global


-- | Attributes common to all instruments.  Unlike 'Config', these are
-- part of the instrument itself and not configurable.
data Common code = Common
    -- | Cmds and Derive calls.  This is abstract so this can be defined
    -- without incurring a dependency on "Cmd.Cmd", which would wind up being
    -- a circular dependency.
    { commonCode :: code
    -- | This environ is merged into the derive environ when the instrument
    -- comes into scope, and also when the pitch of 'Score.Event's with this
    -- instrument is converted.  Typically it sets things like instrument
    -- range, tuning details, etc.
    , commonEnviron :: REnv.Environ
    -- | Key-value pairs used to index the instrument.  A key may appear more
    -- than once with different values.  Tags are free-form, but there is
    -- a list of standard tags in "Instrument.Tag".
    , commonTags :: [Tag.Tag]
    -- | So, instrument, tell me about yourself.
    , commonDoc :: Doc.Doc
    -- | Flags shared with all instruments.
    --
    -- TODO unlike midi flags, these are hardcoded and can't be changed
    -- per-instrument.  I should probably do the same thing as Midi.Patch and
    -- have a Settings which is copied as the default.  But it's a hassle and
    -- I don't need it right now.
    , commonFlags :: Set Flag
    , commonCallMap :: CallMap
    } deriving (Show, Functor)

code = Lens.lens commonCode (\r a -> r { commonCode = a })
environ = Lens.lens commonEnviron (\r a -> r { commonEnviron = a })
tags = Lens.lens commonTags (\r a -> r { commonTags = a })
doc = Lens.lens commonDoc (\r a -> r { commonDoc = a })
flags = Lens.lens commonFlags (\r a -> r { commonFlags = a })
callMap = Lens.lens commonCallMap (\r a -> r { commonCallMap = a })
-- | Map attributes to the names of the calls they should map to.  This
-- is used by the integrator to turn score events into UI events.
type CallMap = Map Attrs.Attributes Expr.Symbol

common :: code -> Common code
common code = Common
    { commonCode = code
    , commonEnviron = mempty
    , commonTags = []
    , commonDoc = ""
    , commonFlags = mempty
    , commonCallMap = mempty
    }

instance Pretty code => Pretty (Common code) where
    format (Common code env tags doc flags callMap) =
        Pretty.record "Instrument"
            [ ("code", Pretty.format code)
            , ("restricted_environ", Pretty.format env)
            , ("tags", Pretty.format tags)
            , ("doc", Pretty.format doc)
            , ("flags", Pretty.format flags)
            , ("call_map", Pretty.format callMap)
            ]

data Flag =
    -- | Patch doesn't pay attention to duration, e.g. percussion.  The UI can
    -- use this to create zero duration events for this instrument.
    Triggered
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty Flag where pretty = showt

addEnviron :: REnv.ToVal a => EnvKey.Key -> a
    -> Common code -> Common code
addEnviron key val =
    environ %= (REnv.fromList [(key, REnv.toVal val)] <>)

-- * AttributeMap

{- | This determines what Attributes the instrument can respond to.  Each
    set of Attributes is mapped to a backend-specific value.  The attributes
    are matched by subset in order, so their order gives a priority.

    For example, if @+pizz@ is before @+nv@, then @+pizz+nv@ will map to
    @+pizz@, unless @+pizz+nv@ exists.  The idea is that more specific or
    more perceptually important attributes go first.  Since pizz vs. arco
    is a much more obvious distinction than vibrato vs. nv, if you say
    everything is nv but some notes are also pizz, chances are you want those
    notes to get pizz even if there isn't a specifically nv pizz variant.

    This also means that if a previous attr is a subset of a later one, the
    later one will never be selected.  'overlapping_attributes' will check for
    that, but normally you use a constructor that calls 'sort_attributes' to
    make sure that can't happen.
-}
newtype AttributeMap a = AttributeMap [(Attrs.Attributes, a)]
    deriving (Eq, Show, Pretty, Functor, Serialize.Serialize)

instance Semigroup (AttributeMap a) where
    AttributeMap as <> AttributeMap bs =
        AttributeMap $ sortAttributes (as <> bs)

instance Monoid (AttributeMap a) where
    mempty = AttributeMap []
    mappend = (<>)

attributeMap :: [(Attrs.Attributes, a)] -> AttributeMap a
attributeMap = AttributeMap . sortAttributes

mappedAttributes :: AttributeMap a -> [Attrs.Attributes]
mappedAttributes (AttributeMap table) = map fst table

attributeVals :: AttributeMap a -> [a]
attributeVals (AttributeMap table) = map snd table

-- | Look up the value as described in 'AttributeMap'.
lookupAttributes :: Attrs.Attributes -> AttributeMap a
    -> Maybe (Attrs.Attributes, a)
lookupAttributes attrs (AttributeMap table) =
    List.find ((attrs `Attrs.contain`) . fst) table

-- | Figured out if any attributes shadow other attributes.  I think this
-- shouldn't happen if you called 'sort_attributes', or used any of the
-- constructors other than 'AttributeMap'.
overlappingAttributes :: AttributeMap a -> [Text]
overlappingAttributes (AttributeMap table) =
    Maybe.catMaybes $ zipWith check (List.inits attrs) attrs
    where
    attrs = map fst table
    check prevs attr = case List.find (Attrs.contain attr) prevs of
        Just otherAttr -> Just $ "attrs "
            <> ShowVal.showVal attr <> " shadowed by "
            <> ShowVal.showVal otherAttr
        Nothing -> Nothing

-- | 'lookup_attributes' looks for the first subset, which means that a smaller
-- set of attributes can shadow a larger set.  Since it's annoying to have to
-- worry about order, sort larger sets to the back.
--
-- The sort is stable, so it shouldn't destroy the priority implicit in the
-- order.
sortAttributes :: [(Attrs.Attributes, a)] -> [(Attrs.Attributes, a)]
sortAttributes = Lists.sortOn (\(a, _) -> - Set.size (Attrs.toSet a))


-- * Config

-- | Configuration for a specific allocation of an instrument in a specific
-- score.
data Config = Config
    -- | This is a local version of 'common_environ'.  Overlayed on the
    -- instrument config 'common_environ'.
    { configEnviron :: REnv.Environ
    -- | This is the control equivalent to 'config_environ'.  These
    -- controls are merged using their default mergers in the note call.
    -- Being in the note call means that the merge should only happen once.
    -- 'config_environ', on the other hand, is applied when the instrument
    -- comes into scope, which should be safe, since merging the environ is
    -- idempotent.
    --
    -- This can be useful to set a per-instrument transposition, or dynamic
    -- level.
    , configControls :: ScoreT.ControlValMap
    -- | If true, this instrument is filtered out prior to playing.
    , configMute :: Bool
    -- | If any instrument is soloed, all instruments except soloed ones are
    -- filtered out prior to playing.
    , configSolo :: Bool
    } deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config
    { configEnviron = mempty
    , configControls = mempty
    , configMute = False
    , configSolo = False
    }

cenviron = Lens.lens configEnviron (\r a -> r { configEnviron = a })
controls = Lens.lens configControls (\r a -> r { configControls = a })
mute = Lens.lens configMute (\r a -> r { configMute = a })
solo = Lens.lens configSolo (\r a -> r { configSolo = a })

instance Pretty Config where
    format (Config environ controls mute solo) = Pretty.record "Config"
        [ ("environ", Pretty.format environ)
        , ("controls", Pretty.format controls)
        , ("mute", Pretty.format mute)
        , ("solo", Pretty.format solo)
        ]

addCenviron :: REnv.ToVal a => EnvKey.Key -> a
    -> Config -> Config
addCenviron key val = cenviron %= (REnv.fromList [(key, v)] <>)
    where v = REnv.toVal val
