-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Util.Format4 (
    Doc, text
    , (</>), (<+/>), (<+>)
    , newline, indented, _indented_, _indented, indented_
    , Width, render, renderFlat
#ifdef TESTING
    , BreakType(..), Section(..), B(..), bFromText
    , spanLine, findBreak
#endif
) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.String as String
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

-- import qualified Util.Debug as Debug
-- import Util.PPrint (pprint)


data Doc =
    Text !Text -- use 'text' instead of this
    | Doc :+ Doc -- intentionally lazy
    -- | Line break.  If the Indent is non-zero, this increments the indent
    -- level.
    | Break !BreakType !Indent
    deriving (Show)

instance Monoid.Monoid Doc where
    mempty = Text mempty
    mappend = (:+)

instance String.IsString Doc where
    fromString = text . String.fromString

emptyDoc :: Doc -> Bool
emptyDoc (Text t) = Text.null t
emptyDoc _ = False

text :: Text -> Doc
text t = case make t of
    [] -> mempty
    ts -> foldr1 (:+) ts
    where
    make = filter (not . emptyDoc) . List.intersperse newline . map Text
        . Text.split (=='\n')


-- | Space becomes a space when it doesn't break, NoSpace doesn't.
data BreakType = NoSpace | Space | Hard deriving (Eq, Ord, Show)

instance Monoid.Monoid BreakType where
    mempty = NoSpace
    mappend = max

-- | Soft break with a space.
(<+/>) :: Doc -> Doc -> Doc
d1 <+/> d2 = d1 <> Break Space 0 <> d2
infixr 5 <+/> -- less than <>

-- | Soft break with no space.
(</>) :: Doc -> Doc -> Doc
d1 </> d2 = d1 <> Break NoSpace 0 <> d2
infixr 5 </> -- less than <>

newline :: Doc
newline = Break Hard 0

indented :: Doc -> Doc
indented d = Break NoSpace 1 <> d <> Break NoSpace (-1)

_indented_ :: Doc -> Doc
_indented_ d = Break Space 1 <> d <> Break Space (-1)

_indented :: Doc -> Doc
_indented d = Break Space 1 <> d <> Break NoSpace (-1)

indented_ :: Doc -> Doc
indented_ d = Break NoSpace 1 <> d <> Break Space (-1)

-- | Join two docs with a space.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> Text " " <> d2
infixr 6 <+> -- same as <>

-- * render

-- | Width of monospace text, in characters.
type Width = Int

data State = State {
    -- | Collect here for each Section.
    stateCollect :: !B
    -- | If a break has been seen, (prevText, space, indent).
    , stateSections :: ![Section]
    , stateIndent :: !Indent
    } deriving (Show)

data Section = Section {
    sectionIndent :: !Indent
    , sectionB :: !B
    , sectionBreak :: !BreakType
    } deriving (Show)

type Indent = Int

sectionBuilder :: Section -> Builder.Builder
sectionBuilder = bBuilder . sectionB

flatten :: Doc -> [Section]
flatten = collapse . reverse . stateSections . go newline . flip go initialState
    -- TODO use dlist so I don't have to reverse
    where
    initialState = State
        { stateCollect = mempty
        , stateSections = []
        , stateIndent = 0
        }
    go doc state = case doc of
        Text t -> state { stateCollect = stateCollect state <> bFromText t }
        d1 :+ d2 -> go d2 (go d1 state)
        Break btype indent -> state
            { stateCollect = mempty
            , stateSections =
                Section (stateIndent state) (stateCollect state) btype
                    : stateSections state
            , stateIndent = indent + stateIndent state
            }
    -- Empty sections can happen after dedents.  I don't want them, but I do
    -- want to get the break if it's stronger.
    collapse [] = []
    collapse (section : sections) = case span (bNull . sectionB) sections of
        ([], _) -> section : collapse sections
        (nulls, rest) -> section { sectionBreak = break } : collapse rest
            where
            break = sectionBreak section <> mconcat (map sectionBreak nulls)

render :: Text -> Width -> Doc -> Lazy.Text
-- render indent width = renderText indent width . Debug.trace "secs" . flatten
render indent width = renderText indent width . flatten

renderFlat :: Doc -> Lazy.Text
renderFlat = renderTextFlat . flatten

-- Brackets change [break, "}", break] to [Hard, "}", Hard] if there has been
-- a newline since the corresponding "{".
-- I can do that it 'renderText'.  Each time I see a "{", I put a ("{", False).
-- When I emit a newline, all Falses become True.  When I get to the
-- corresponding "}", I check if it has True.  To know it's corresponding, each
-- "{" has a stack, and when I see the "}", pull the front of the stack.
--
-- Or, have another kind of 'sectionB' with nested [Section].  When
-- 'renderText' sees it, it invokes a recursive 'go', which returns True if it
-- emitted a newline, unless it was immediately previous.  If True, the caller
-- emits a newline, drops any following breaks, and continues.


-- | Take sections until they go over the width, or I see a hard newline, or
-- run out.  If they went over, then find a break in the collected, emit before
-- the break, and try again.
renderText :: Text -> Width -> [Section] -> Lazy.Text
renderText indentS width = Builder.toLazyText . flip go mempty
    where
    go [] out = out
    go allSections@(Section indent _ _ : _) out =
        -- out <> case Debug.trace "line" $ spanLine (Text.length indentS) width allSections of
        out <> case spanLine (Text.length indentS) width allSections of
            (_, [], []) -> mempty
            (_, [], section : sections) ->
                go sections (emitLine (sectionBuilder section))
            (False, line, rest) ->
                go rest (emitLine (renderSections line))
            -- (True, line, rest) -> case Debug.trace "break" $ findBreak line of
            (True, line, rest) -> case findBreak line of
                ([], []) -> mempty -- shouldn't be possible
                ([], section : sections) ->
                    go (sections ++ rest) (emitLine (sectionBuilder section))
                (line, rest2) ->
                    go (rest2 ++ rest) (emitLine (renderSections line))
        where
        emitLine b = indentB <> b <> nl
            where
            indentB = mconcat $ replicate indent indent1
            nl = Builder.singleton '\n'
    indent1 = Builder.fromText indentS

renderTextFlat :: [Section] -> Lazy.Text
renderTextFlat = Builder.toLazyText . renderSections

renderSections :: [Section] -> Builder.Builder
renderSections sections =
    mconcat $ interleave (map sectionBuilder sections) spaces
    where
    spaces = map (toSpace . sectionBreak) sections
    toSpace Space = Builder.singleton ' '
    toSpace _ = mempty

-- | Collect a line's worth of Sections.  If break is True, it has one extra.
spanLine :: Width -> Width -> [Section] -> (Bool, [Section], [Section])
spanLine _ _ [] = (False, [], [])
spanLine indentWidth maxWidth sections@(Section indent _ _ : _) =
    go (indentWidth * indent) sections
    where
    go _ [] = (False, [], [])
    go col (section : sections)
        | col + width > maxWidth = (True, [section], sections)
        | sectionBreak section == Hard = (False, [section], sections)
        | otherwise =
            let (break, pre, post) = go (col + space + width) sections
            in (break, section : pre, post)
        where
        space = if sectionBreak section == Space then 1 else 0
        width = bWidth (sectionB section)

-- | Split before the last lowest indent.
findBreak :: [Section] -> ([Section], [Section])
findBreak sections = case lowest of
    Nothing -> ([], [])
    Just (i, _) -> splitAt i sections
    where lowest = minimumOn snd $ zip [0..] (map sectionIndent sections)

{-
    t0 = "Rec" <> indented
        ("{ f1 =" <> indented "valu1"
        </> ", f2 =" <> indented "valu2"
        </> ", f3 =" <> indented "valu3"
        </> "}")

    123456789012345678901234567890
    Rec { f1 = valu1, f2 = valu2, f3 = valu3

    Rec
    Rec { f1 =
       |
    Rec { f1 = valu1
       |      ^
        { f1 = valu1, f2 =
       |             ^
        { f1 = valu1, f2 = valu2
       |             ^
        { f1 = valu1, f2 = valu2, f3 =
       |                         ^
        { f1 = valu1, f2 = valu2, f3 = valu3
       |                         ^

    -- break on the last section 0 where everything before it fits
    Section 0 "Rec" Space *
    Section 1 "{ f1 =" Space
    Section 2 "valu1" NoSpace
    Section 1 ", f2 =" Space
    Section 2 "valu2" NoSpace
    Section 1 ", f3 =" Space
    Section 2 "valu3" NoSpace
    Section 1 "}" NoSpace

    123456789012345678901234567890
    Rec
      { f1 = valu1, f2 = valu2, f3 = valu3

    -- break on the last section 1 where everything before it fits
    Section 1 "{ f1 =" Space
    Section 2 "valu1" NoSpace
    Section 1 ", f2 =" Space
    Section 2 "valu2" NoSpace
    Section 1 ", f3 =" Space *
    Section 2 "valu3" NoSpace
    Section 1 "}" NoSpace

    123456789012345678901234567890
    Rec
      { f1 = valu1, f2 = valu2
      , f3 = valu3

    -- both fit, but I have to break before "}" because I have broken after the
    -- corresponding "{"
    Section 1 ", f3 =" Space
    Section 2 "valu3" NoSpace
    Section 1 "}" NoSpace *

    123456789012345678901234567890
    Rec
      { f1 = valu1, f2 = valu2
      , f3 = valu3
      }

    -- Emit newline after "}" because I broke after the corresponding "{".
    Section 1 "}" NoSpace

-}

-- * B

-- | A 'Builder.Builder' that keeps track of its length.
data B = B {
    bBuilder :: !Builder.Builder
    , bWidth :: !Width
    }

instance Show B where
    show (B b _) = show b

instance Monoid.Monoid B where
    mempty = B mempty 0
    mappend (B b1 len1) (B b2 len2) = B (b1<>b2) (len1+len2)
    mconcat [] = mempty
    mconcat bs = B (mconcat builders) (sum lens)
        where (builders, lens) = unzip [(b, len) | B b len <- bs, len /= 0]

bFromText :: Text -> B
bFromText text = B (Builder.fromText text) (textWidth text)

-- | TODO take double-width characters into account
textWidth :: Text -> Width
textWidth = Text.length

bNull :: B -> Bool
bNull = (==0) . bWidth


-- * misc

minimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x <= key low then x else low

interleave :: [a] -> [a] -> [a]
interleave [x] _ = [x]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs _ = xs
