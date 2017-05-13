-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This contains instrument data in common between different backends.
module Instrument.Common where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Tag as Tag
import Global


data Common code = Common {
    -- | Cmds and Derive calls.  This is abstract so this can be defined
    -- without incurring a dependency on "Cmd.Cmd", which would wind up being
    -- a circular dependency.
    common_code :: !code
    -- | This environ is merged into the derive environ when the instrument
    -- comes into scope, and also when the pitch of 'Score.Event's with this
    -- instrument is converted.  Typically it sets things like instrument
    -- range, tuning details, etc.
    , common_environ :: !RestrictedEnviron.Environ
    -- | Key-value pairs used to index the instrument.  A key may appear more
    -- than once with different values.  Tags are free-form, but there is
    -- a list of standard tags in "Instrument.Tag".
    , common_tags :: ![Tag.Tag]
    -- | So, instrument, tell me about yourself.
    , common_doc :: !Doc.Doc
    } deriving (Show)

code = Lens.lens common_code (\f r -> r { common_code = f (common_code r) })
environ = Lens.lens common_environ
    (\f r -> r { common_environ = f (common_environ r) })
tags = Lens.lens common_tags (\f r -> r { common_tags = f (common_tags r) })
doc = Lens.lens common_doc (\f r -> r { common_doc = f (common_doc r) })

common :: code -> Common code
common code = Common
    { common_code = code
    , common_environ = mempty
    , common_tags = []
    , common_doc = ""
    }

instance Pretty code => Pretty (Common code) where
    format (Common code env tags doc) = Pretty.record "Instrument"
        [ ("code", Pretty.format code)
        , ("restricted_environ", Pretty.format env)
        , ("tags", Pretty.format tags)
        , ("doc", Pretty.format doc)
        ]

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
    deriving (Eq, Show, Pretty, Serialize.Serialize)

attribute_map :: [(Attrs.Attributes, a)] -> AttributeMap a
attribute_map = sort_attribute_map . AttributeMap

mapped_attributes :: AttributeMap a -> [Attrs.Attributes]
mapped_attributes (AttributeMap table) = map fst table

attribute_vals :: AttributeMap a -> [a]
attribute_vals (AttributeMap table) = map snd table

-- | Look up the value as described in 'AttributeMap'.
lookup_attributes :: Attrs.Attributes -> AttributeMap a -> Maybe a
lookup_attributes attrs (AttributeMap table) =
    snd <$> List.find ((attrs `Attrs.contain`) . fst) table

-- | Figured out if any attributes shadow other attributes.  I think this
-- shouldn't happen if you called 'sort_attribute_map', or used any of the
-- constructors other than 'AttributeMap'.
overlapping_attributes :: AttributeMap a -> [Text]
overlapping_attributes (AttributeMap table) =
    Maybe.catMaybes $ zipWith check (List.inits attrs) attrs
    where
    attrs = map fst table
    check prevs attr = case List.find (Attrs.contain attr) prevs of
        Just other_attr -> Just $ "attrs "
            <> ShowVal.show_val attr <> " shadowed by "
            <> ShowVal.show_val other_attr
        Nothing -> Nothing

-- | 'lookup_attribute' looks for the first subset, which means that a smaller
-- set of attributes can shadow a larger set.  Since it's annoying to have to
-- worry about order, sort larger sets to the back.
--
-- The sort is stable, so it shouldn't destroy the priority implicit in the
-- order.
sort_attribute_map :: AttributeMap a -> AttributeMap a
sort_attribute_map (AttributeMap table) = AttributeMap (sort table)
    where sort = Seq.sort_on (\(a, _) -> - Set.size (Attrs.to_set a))


-- * Config

-- | Configuration for a specific allocation of an instrument in a specific
-- score.
data Config = Config {
    -- | This is a local version of 'common_environ'.
    config_environ :: !RestrictedEnviron.Environ
    -- | This is the control equivalent to 'config_environ'.  These
    -- controls are merged using their default mergers in the note call.
    -- Being in the note call means that the merge should only happen once.
    -- 'config_environ', on the other hand, is applied when the instrument
    -- comes into scope, which should be safe, since merging the environ is
    -- idempotent.
    --
    -- This can be useful to set a per-instrument transposition, or dynamic
    -- level.
    , config_controls :: !ScoreTypes.ControlValMap
    -- | If true, this instrument is filtered out prior to playing.
    , config_mute :: !Bool
    -- | If any instrument is soloed, all instruments except soloed ones are
    -- filtered out prior to playing.
    , config_solo :: !Bool
    } deriving (Eq, Show)

empty_config :: Config
empty_config = Config
    { config_environ = mempty
    , config_controls = mempty
    , config_mute = False
    , config_solo = False
    }

cenviron = Lens.lens config_environ
    (\f r -> r { config_environ = f (config_environ r) })
controls = Lens.lens config_controls
    (\f r -> r { config_controls = f (config_controls r) })
mute = Lens.lens config_mute
    (\f r -> r { config_mute = f (config_mute r) })
solo = Lens.lens config_solo
    (\f r -> r { config_solo = f (config_solo r) })

instance Pretty Config where
    format (Config environ controls mute solo) = Pretty.record "Config"
        [ ("environ", Pretty.format environ)
        , ("controls", Pretty.format controls)
        , ("mute", Pretty.format mute)
        , ("solo", Pretty.format solo)
        ]

add_environ :: RestrictedEnviron.ToVal a => BaseTypes.Key -> a
    -> Config -> Config
add_environ key val = cenviron %= (RestrictedEnviron.make [(key, v)] <>)
    where v = RestrictedEnviron.to_val val

get_environ :: Config -> BaseTypes.Environ
get_environ = RestrictedEnviron.convert . config_environ
