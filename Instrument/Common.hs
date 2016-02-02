-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This contains instrument data in common between different backends.
module Instrument.Common where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

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
    , common_doc :: !Text
    } deriving (Show)

get_environ :: Common code -> BaseTypes.Environ
get_environ = RestrictedEnviron.convert . common_environ

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

instance Pretty.Pretty code => Pretty.Pretty (Common code) where
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
newtype AttributeMap a = AttributeMap [(ScoreTypes.Attributes, a)]
    deriving (Eq, Show, Pretty.Pretty, Serialize.Serialize)

attribute_map :: [(ScoreTypes.Attributes, a)] -> AttributeMap a
attribute_map = sort_attribute_map . AttributeMap

mapped_attributes :: AttributeMap a -> [ScoreTypes.Attributes]
mapped_attributes (AttributeMap table) = map fst table

-- | Look up the value as described in 'AttributeMap'.
lookup_attributes :: ScoreTypes.Attributes -> AttributeMap a -> Maybe a
lookup_attributes attrs (AttributeMap table) =
    snd <$> List.find ((attrs `ScoreTypes.attrs_contain`) . fst) table

-- | Figured out if any attributes shadow other attributes.  I think this
-- shouldn't happen if you called 'sort_attribute_map', or used any of the
-- constructors other than 'AttributeMap'.
overlapping_attributes :: AttributeMap a -> [Text]
overlapping_attributes (AttributeMap table) =
    Maybe.catMaybes $ zipWith check (List.inits attrs) attrs
    where
    attrs = map fst table
    check prevs attr = case List.find (ScoreTypes.attrs_contain attr) prevs of
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
    where sort = Seq.sort_on (\(a, _) -> - Set.size (ScoreTypes.attrs_set a))
