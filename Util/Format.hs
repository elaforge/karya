-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Format (
    Doc, shortForm, text
    , (</>), (<+/>), (<//>), (<+>)
    , newline, unlines, wrap, wrapWords
    , withIndent, indent, indent_, indentLine
    , Width, render, renderFlat
    , simplify
#ifdef TESTING
    , Doc(..)
    , BreakType(..), Section(..), B(..), bFromText
    , flatten, spanLine, findBreak
#endif
) where
import Prelude hiding (unlines)
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
    -- | The contained Doc will be indented by a number of steps.  The new
    -- indent level only takes effect after the first Break.
    | Indented !Indent Doc
    -- | Line break.
    | Break !BreakType
    deriving (Show)

{-
    I tried to define (:+) as @Union Doc Break Doc@ to enforce that exactly
    one break is between each Doc.  I also hoped to avoid awkward constructions
    like @"text" <> indented "x"@ or text <> ShortForm by making Doc
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
--
-- Prepending text to a shortForm will distribute over both short and long
-- forms.  Otherwise, if you write @"prefix " <> x@, and @x@ happens to be
-- a shortForm, the long form loses the prefix.
shortForm :: Doc -> Doc -> Doc
shortForm = ShortForm

text :: Text -> Doc
text t = case make t of
    [] -> mempty
    ts -> foldr1 (:+) ts
    where
    make = filter (not . isEmpty) . List.intersperse newline . map Text
        . Text.split (=='\n')

isEmpty :: Doc -> Bool
isEmpty (Text t) = Text.null t
isEmpty _ = False

-- | Space becomes a space when it doesn't break, NoSpace doesn't.
data BreakType = NoSpace | Space | Hard deriving (Eq, Ord, Show)

instance Monoid.Monoid BreakType where
    mempty = NoSpace
    mappend = max

-- | Soft break with no space.
(</>) :: Doc -> Doc -> Doc
d1 </> d2 = d1 <> Break NoSpace <> d2
infixr 5 </> -- looser than <>

-- | Soft break with a space.
(<+/>) :: Doc -> Doc -> Doc
d1 <+/> d2 = d1 <> Break Space <> d2
infixr 5 <+/> -- looser than <>

-- | Hard break, see 'newline'.
(<//>) :: Doc -> Doc -> Doc
d1 <//> d2 = d1 <> newline <> d2
infixr 4 <//> -- looser than </>

-- | Increase the indent level for the given Doc.  The indent change only
-- takes effect after the first break, so if you want it to take effect
-- immediately, use one of 'indent', 'indent_', or 'indentLine'.
--
-- The reason indent is delayed is that this way you can do a hanging indent,
-- where the current line is unindented, but it will be indented if it wraps.
-- Otherwise you don't know where to put the indent, since you don't know
-- where the break will happen.
withIndent :: Doc -> Doc
withIndent = Indented 1

indentBreak :: BreakType -> Doc -> Doc
indentBreak break doc = Indented 1 (Break break :+ doc)

-- | Change the indent level and add a no-space break so it takes effect
-- immediately.
indent :: Doc -> Doc
indent = indentBreak NoSpace

-- | Change the indent level and add a spaced break so it takes effect
-- immediately.
indent_ :: Doc -> Doc
indent_ = indentBreak Space

-- | Change the indent level and add a hard break so it takes effect
-- immediately.
indentLine :: Doc -> Doc
indentLine = indentBreak Hard

-- | Join two docs with a space.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> Text " " <> d2
infixr 6 <+> -- same as <>

-- | A hard break will definitely cause a line break.
--
-- Consecutive breaks are merged together, and a hard break always wins.
-- Also, multiple hard breaks are merged into one.  The rationale is that
-- if you are formatting a list of sub-Docs, and you want to put each on its
-- own line, you need a hard break after each one, but if one of them does
-- the same thing, you wind up with two breaks in a row.
newline :: Doc
newline = Break Hard

-- | Analogous to 'Prelude.unlines', terminate each Doc with a newline.
unlines :: [Doc] -> Doc
unlines [] = mempty
unlines docs = mconcat (List.intersperse newline docs) <> newline

wrapWords :: [Doc] -> Doc
wrapWords (d:ds) = List.foldl' (<+/>) d ds
wrapWords [] = mempty

wrap :: [Doc] -> Doc
wrap (d:ds) = List.foldl' (</>) d ds
wrap [] = mempty

-- * render

-- | Width of monospace text, in characters.
type Width = Int
-- | Number of indent levels.  The provided indent text will be replicated this
-- many times.
type Indent = Int

data State = State {
    -- | Collect text for each Section, for 'sectionB'.
    stateCollect :: !B
    -- | Collect long form Sections for 'sectionSubs'.  Like stateSections,
    -- this is in reverse order.
    , stateSubs :: ![Section]
    -- | Collect sections in reverse order.
    , stateSections :: ![Section]
    , stateIndent :: !Indent
    -- | Indent for the next break.  This is different from 'stateIndent',
    -- which is the current indent value because the new indent only applies
    -- *after* the next break.  Also, after a dedent I still need the indented
    -- value to apply to the section.
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
flatten = postprocSections . stateSections . flush . flip go initialState
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
            -- TODO old subs are lost?  What happens with
            -- shortForm <> shortForm?
            , stateSubs = stateSections sub
            }
            where
            -- I need a break to collect the last part of the long form
            -- sub-doc.  But I can only know the break once I see it, after
            -- this ShortForm.  So this break is temporary and will be replaced
            -- by 'replaceBreaks' below.
            sub = goBreak Hard $ go long initial
                where
                initial = initialState
                    -- This causes @a <> ShortForm b c@ to distribute the @a@
                    -- over @b@ and @c@, as documented by 'shortForm'.
                    { stateCollect = stateCollect state
                    , stateIndent = stateIndent state
                    , stateBreakIndent = stateBreakIndent state
                    }
        Indented n doc -> dedent $ go doc $ indent state
            where
            indent state = state
                { stateIndent = stateIndent state + n }
            dedent state = state { stateIndent = stateIndent state - n }
        Break break -> goBreak break state
    goBreak break state = state
        { stateCollect = mempty
        , stateSubs = []
        , stateSections = (: stateSections state) $ Section
            { sectionIndent = stateBreakIndent state
            , sectionB = stateCollect state
            -- If there are subs, then they have been collected by the long
            -- part of a ShortForm.
            , sectionSubs = replaceBreaks $ stateSubs state
            , sectionBreak = break
            }
        , stateBreakIndent = stateIndent state
        }
        where
        -- Recursively replace all the first breaks in the subs.  Since subs
        -- are collected in reverse, this replaces the final break, which was
        -- just a placeholder.
        replaceBreaks [] = []
        replaceBreaks (sub:subs) = sub
            { sectionBreak = break
            , sectionSubs = replaceBreaks $ sectionSubs sub
            } : subs

    -- If there is trailing text, break it with a Hard newline.  Otherwise,
    -- convert the last break to Hard.
    flush state
        | not (bNull (stateCollect state)) = goBreak Hard state
        | final : sections <- stateSections state = state
            { stateSections = final { sectionBreak = Hard } : sections }
        | otherwise = state

-- | Clean up 'stateSections' after 'flatten'.
postprocSections :: [Section] -> [Section]
postprocSections = map subs . mergeBreaks . reverse
    -- TODO use dlist so I don't have to reverse, but benchmark first
    where
    -- sectionSubs are also reversed, and need their breaks merged.
    subs section
        | null (sectionSubs section) = section
        | otherwise = section
            { sectionSubs = postprocSections $ sectionSubs section }

-- | Collapse consecutive breaks into the strongest one.  Empty sections can
-- happen after dedents.  I don't want them, but I do want to get the break if
-- it's stronger.
mergeBreaks :: [Section] -> [Section]
mergeBreaks [] = []
mergeBreaks (section : sections) = case span empty sections of
    ([], _) -> section : mergeBreaks sections
    (nulls, rest) -> section { sectionBreak = break } : mergeBreaks rest
        where break = sectionBreak section <> mconcat (map sectionBreak nulls)
    where empty section = bNull (sectionB section)

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
        -- break is ignored, because the last sub should have the same break.
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

-- | Collect a line's worth of Sections.
-- TODO 'findBreak' should probably be integrated into this.
spanLine :: Width -> Width -> [Section] -> (Bool, [Section], [Section])
    -- ^ (break, pre_break, post_break).  If break is False, then pre_break
    -- can be emitted as-is.  If break is True, then the line went over the
    -- maxWidth and must be broken.  pre_break will then have one Section past
    -- the break point.  This is so 'findBreak' can know what the next indent
    -- break is, so it can know if it's ok to break there.
spanLine _ _ [] = (False, [], [])
spanLine indentWidth maxWidth sections@(Section indent _ _ _ : _) =
    go (indentWidth * indent) sections
    where
    go _ [] = (False, [], [])
    go col (section : sections)
        -- Break as soon as the indent goes below the initial indent.
        | sectionIndent section < indent = (False, [], section : sections)
        | col + width > maxWidth = (True, [section], sections)
        | sectionBreak section == Hard = (False, [section], sections)
        | otherwise =
            let (break, pre, post) = go (col + space + width) sections
            in (break, section : pre, post)
        where
        space = if sectionBreak section == Space then 1 else 0
        width = bWidth (sectionB section)

-- | Given a list of Sections that I know need to be broken, find the best
-- place to break.  Split before the last lowest indent.
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

-- * debug

-- | Merge Texts so the Doc is easier to read.
simplify :: Doc -> Doc
simplify doc = case doc of
    Text t1 :+ d1 -> case simplify d1 of
        Text t2 :+ d2 -> Text (t1<>t2) :+ d2
        Text t2 -> Text (t1<>t2)
        doc -> Text t1 :+ doc
    d1 :+ d2 -> case (simplify d1, simplify d2) of
        (Text t1, Text t2) -> Text (t1 <> t2)
        (d1, d2) -> d1 :+ d2
    ShortForm d1 d2 ->
        ShortForm (Text (Lazy.toStrict (renderFlat d1))) (simplify d2)
    Indented i d -> Indented i (simplify d)
    _ -> doc


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
