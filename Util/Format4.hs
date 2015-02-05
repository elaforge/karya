-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Util.Format4 (
    Doc, shortForm, text
    , (</>), (<//>), (<+/>), (<+>)
    , indented
    , Width, render, renderFlat
#ifdef TESTING
    , BreakType(..), Section(..), B(..), bFromText
    , flatten, spanLine, findBreak
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


data Doc =
    Text !Text -- use 'text' instead of this
    | Doc :+ Doc -- intentionally lazy
    -- | The first Doc is the short form, which will be used if it doesn't have
    -- to wrap.
    | ShortForm Doc Doc
    -- | The contained Doc will be indented by a number of steps.
    | Indented !Indent Doc
    -- | Line break.
    | Break !BreakType
    deriving (Show)

{-
    I tried to define (:+) as @Union Doc Break Doc@ to enforce that exactly
    one break is between each Doc.  I also hoped to avoid awkward constructions
    like @"text" <> indented "x"@ or texed with ShortForm by making Doc
    no longer a Monoid.  Unfortunately, I do in fact want to stick text on
    Docs, e.g. @map (","<+>) docs@, and I can no longer write
    @"a" <> "b" </> "c"@ since I'd have to manually wrap the text parts in
    a constructor.  Trying to automatically promote with a typeclass runs into
    ambiguity errors with IsString.

    It's probably still possible if I don't mind some manual promotion, but
    (<>) sticking text inside Indented or ShortForm doesn't seem that bad.
-}

instance Monoid.Monoid Doc where
    mempty = Text mempty
    mappend = (:+)

instance String.IsString Doc where
    fromString = text . String.fromString

-- | The first Doc is the short form, which will be used if it doesn't have
-- to wrap.
shortForm :: Doc -> Doc -> Doc
shortForm = ShortForm

text :: Text -> Doc
text t = case make t of
    [] -> mempty
    ts -> foldr1 (:+) ts
    where
    make = filter (not . isEmpty) . List.intersperse (Break Hard) . map Text
        . Text.split (=='\n')

isEmpty :: Doc -> Bool
isEmpty (Text t) = Text.null t
isEmpty _ = False

-- | Space becomes a space when it doesn't break, NoSpace doesn't.
data BreakType = NoSpace | Space | Hard deriving (Eq, Ord, Show)

instance Monoid.Monoid BreakType where
    mempty = NoSpace
    mappend = max

-- | Soft break with a space.
(<+/>) :: Doc -> Doc -> Doc
d1 <+/> d2 = d1 <> Break Space <> d2
infixr 5 <+/> -- less than <>

-- | Soft break with no space.
(</>) :: Doc -> Doc -> Doc
d1 </> d2 = d1 <> Break NoSpace <> d2
infixr 5 </> -- less than <>

-- | Hard break.
(<//>) :: Doc -> Doc -> Doc
d1 <//> d2 = d1 <> Break Hard <> d2

indented :: Doc -> Doc
indented = Indented 1

-- | Join two docs with a space.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> Text " " <> d2
infixr 6 <+> -- same as <>

-- * render

-- | Width of monospace text, in characters.
type Width = Int
-- | Number of indent levels.  The provided indent text will be replicated this
-- many times.
type Indent = Int

data State = State {
    -- | Collect text each Section.
    stateCollect :: !B
    -- | Collect long form Sections.
    , stateSubs :: ![Section]
    -- | If a break has been seen, (prevText, space, indent).
    , stateSections :: ![Section]
    , stateIndent :: !Indent
    , stateBreakIndent :: !Indent
    } deriving (Show)

data Section = Section {
    sectionIndent :: !Indent
    , sectionB :: !B
    -- | If present, the B is a short version.  If it doesn't fit on a line of
    -- its own, then flatten the subs.  Normally I put down Sections until
    -- I have to break.  A short layout stands in for a list of Sections.  When
    -- I see one, I try to place it, and if it doesn't fit, use the
    -- sub-Sections.
    , sectionSubs :: ![Section]
    , sectionBreak :: !BreakType
    } deriving (Show)

sectionBuilder :: Section -> Builder.Builder
sectionBuilder = bBuilder . sectionB

flatten :: Doc -> [Section]
flatten =
    collapse . reverse . stateSections . go (Break Hard) . flip go initialState
    -- TODO use dlist so I don't have to reverse, but benchmark first
    where
    initialState = State
        { stateCollect = mempty
        , stateSubs = []
        , stateSections = []
        , stateIndent = 0
        , stateBreakIndent = 0
        }
    go doc state = case doc of
        Text t -> state { stateCollect = stateCollect state <> bFromText t }
        d1 :+ d2 -> go d2 (go d1 state)
        ShortForm short long -> state
            { stateCollect =
                stateCollect state <> renderSectionsB (flatten short)
            , stateSubs = map (addIndent (stateIndent state)) (flatten long)
            }
        Indented n doc -> dedent $ go doc $ indent state
            where
            indent state = state
                { stateIndent = stateIndent state + n
                , stateBreakIndent = stateIndent state + n
                }
            dedent state = state { stateIndent = stateIndent state - n }
        Break btype -> state
            { stateCollect = mempty
            , stateSubs = []
            , stateSections = (: stateSections state) $ Section
                { sectionIndent = stateBreakIndent state
                , sectionB = stateCollect state
                , sectionSubs = stateSubs state
                , sectionBreak = btype
                }
            , stateBreakIndent = stateIndent state
            }
    addIndent i section = section { sectionIndent = i + sectionIndent section }
    -- Empty sections can happen after dedents.  I don't want them, but I do
    -- want to get the break if it's stronger.
    collapse [] = []
    collapse (section : sections) = case span (bNull . sectionB) sections of
        ([], _) -> section : collapse sections
        (nulls, rest) -> section { sectionBreak = break } : collapse rest
            where
            break = sectionBreak section <> mconcat (map sectionBreak nulls)

render :: Text -> Width -> Doc -> Lazy.Text
render indent width = renderText indent width . flatten

renderFlat :: Doc -> Lazy.Text
renderFlat = renderTextFlat . flatten


-- | Take sections until they go over the width, or I see a hard newline, or
-- run out.  If they went over, then find a break in the collected, emit before
-- the break, and try again.
renderText :: Text -> Width -> [Section] -> Lazy.Text
renderText indentS maxWidth = Builder.toLazyText . flip renderLine mempty
    where
    renderLine [] out = out
    renderLine (Section indent b subs _ : sections) out
        | not (null subs) && indent * textWidth indentS + bWidth b > maxWidth =
            renderLine (subs ++ sections) out
    renderLine allSections@(Section indent _ _ _ : _) out =
        out <> case spanLine (textWidth indentS) maxWidth allSections of
            (_, [], []) -> mempty
            (_, [], section : sections) ->
                renderLine sections (emitLine (sectionBuilder section))
            (False, line, rest) ->
                renderLine rest (emitLine (renderSections line))
            (True, line, rest) -> case findBreak line of
                ([], []) -> mempty -- shouldn't be possible
                ([], section : sections) -> renderLine (sections ++ rest)
                    (emitLine (sectionBuilder section))
                (line, rest2) ->
                    renderLine (rest2 ++ rest) (emitLine (renderSections line))
        where
        emitLine b = indentB <> b <> nl
            where
            indentB = mconcat $ replicate indent indent1
            nl = Builder.singleton '\n'
    indent1 = Builder.fromText indentS

renderTextFlat :: [Section] -> Lazy.Text
renderTextFlat = Builder.toLazyText . renderSections

renderSectionsB :: [Section] -> B
renderSectionsB sections =
    mconcat $ interleave (map sectionB sections) spaces
    where
    spaces = map (toSpace . sectionBreak) sections
    toSpace Space = B (Builder.singleton ' ') 1
    toSpace _ = B mempty 0

renderSections :: [Section] -> Builder.Builder
renderSections = bBuilder . renderSectionsB

-- | Collect a line's worth of Sections.  If break is True, it has one Section
-- past the break point.  This is so 'findBreak' can know what the indent
-- past the wrap break is, so it can know if it's ok to break there.
spanLine :: Width -> Width -> [Section] -> (Bool, [Section], [Section])
spanLine _ _ [] = (False, [], [])
spanLine indentWidth maxWidth sections@(Section indent _ _ _ : _) =
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

-- | Find the *last* minimum element.
minimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x <= key low then x else low

-- | Interleave so there is a y between each x.
interleave :: [a] -> [a] -> [a]
interleave [x] _ = [x]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs _ = xs
