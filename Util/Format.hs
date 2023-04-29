-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This is a library to lay out text with line wrapping and indenting.

    The basic theory is that you concatenate text with 'BreakType's.  In
    addition, you can increment the indent level with 'withIndent'.  When
    'render' wraps the text, it will break on the lowest indent level, or as
    soon as the indent level decreases.

    A further wrinkle is that you can mark alternate layouts with 'shortForm'.
-}
module Util.Format (
    Doc, shortForm, text, string
    , (</>), (<+/>), (<//>), (<+>)
    , newline, unlines, paragraphs, wrap, wrapWords
    , withIndent, indent, indent_, indentLine
    , Width, render, renderFlat
    , simplify, denest
#ifdef TESTING
    , module Util.Format
#endif
) where
import           Prelude hiding (unlines)
import qualified Data.Char.WCWidth as WCWidth
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Lists as Lists
import qualified Util.Num as Num


data Doc =
    -- | Use 'text' instead of this constructor to get newlines right.
    Text !Text
    | Doc :+ Doc -- intentionally lazy
    -- | Use 'shortForm'.
    | ShortForm Doc Doc
    -- | Change the indent by the given number of steps.  The new indent level
    -- only takes effect after the first Break.
    | Indent !Indent
    -- | Line break.
    | Break !BreakType
    deriving (Eq, Show)
infixr :+

-- | Number of indent levels.  The provided indent text will be replicated this
-- many times.
type Indent = Int

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
    (<>) sticking text inside Indent or ShortForm doesn't seem that bad.
-}

instance Semigroup Doc where (<>) = (:+)
instance Monoid Doc where
    mempty = Text mempty
    mappend = (<>)

instance String.IsString Doc where
    fromString = text . String.fromString

{- | The first Doc is the short form, which will be used if it doesn't have
    to wrap.  So you can give a compact form which will render if it can fit
    without breaking, and then a form with more fancy layout that will be used
    if it does have to break.  For example, @[1, 2, 3]@ for short lists, and
    @[1\\n, 2\\n, 3\\n]@ for long ones.

    Prepending text to a shortForm will distribute over both short and long
    forms.  Otherwise, if you write @"prefix " <> x@, and @x@ happens to be
    a shortForm, the long form loses the prefix.

    Appending two shortForms will make you lose the long form of the second
    one.  So don't do that.  TODO I'd rather both short and long forms be
    appended, but haven't figured out how to do that yet.
-}
shortForm :: Doc -> Doc -> Doc
shortForm = ShortForm

text :: Text -> Doc
text t = case make t of
    [] -> mempty
    ts -> foldr1 (:+) (merge ts)
    where
    merge [] = []
    merge breaks = case Lists.spanWhile isHard breaks of
        ([], []) -> []
        ([], x : xs) -> x : merge xs
        (hs, rest) -> Break (Hard (Num.sum hs)) : merge rest
    isHard (Break (Hard n)) = Just n
    isHard _ = Nothing
    make = filter (not . isEmpty) . List.intersperse (newline 1) . map Text
        . Text.split (=='\n')

string :: String -> Doc
string = text . Text.pack

isEmpty :: Doc -> Bool
isEmpty (Text t) = Text.null t
isEmpty _ = False

-- | Space becomes a space when it doesn't break, NoSpace doesn't.  Hard
-- breaks can insert >=1 newlines.
data BreakType = NoSpace | Space | Hard !Int deriving (Eq, Ord, Show)

-- | Hard breaks with more newlines win over those with fewer.
instance Semigroup BreakType where (<>) = max

instance Monoid BreakType where
    mempty = NoSpace
    mappend = (<>)

-- | Soft break with no space.
(</>) :: Doc -> Doc -> Doc
d1 </> d2 = d1 <> Break NoSpace <> d2
infixr 5 </> -- looser than <>

-- | Soft break with a space.
(<+/>) :: Doc -> Doc -> Doc
d1 <+/> d2 = d1 <> Break Space <> d2
infixr 5 <+/> -- looser than <>

-- | Hard break with a single 'newline'.
(<//>) :: Doc -> Doc -> Doc
d1 <//> d2 = d1 <> newline 1 <> d2
infixr 4 <//> -- looser than </>

{- | Increase the indent level for the given Doc.  The indent change only
    takes effect after the first break, so if you want it to take effect
    immediately, use one of 'indent', 'indent_', or 'indentLine'.

    The reason indent is delayed is that this way you can do a hanging indent,
    where the current line is unindented, but it will be indented if it wraps.
    Otherwise you don't know where to put the indent, since you don't know
    where the break will happen.
-}
withIndent :: Doc -> Doc
withIndent doc = Indent 1 <> doc <> Indent (-1)

indentBreak :: BreakType -> Doc -> Doc
indentBreak break doc = Indent 1 <> Break break <> doc <> Indent (-1)

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
indentLine = indentBreak (Hard 1)

-- | Join two docs with a space.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> Text " " <> d2
infixr 6 <+> -- same as <>

-- | Insert a number of newlines.
--
-- Consecutive breaks are merged together, and a hard break always wins.
-- Also, multiple hard breaks are merged into one, and ones with greater
-- newlines win other those with fewer.  The rationale is that if you are
-- formatting a list of sub-Docs, and you want to put each on its own line, you
-- need a hard break after each one, but if one of them does the same thing,
-- you wind up with two breaks in a row.
newline :: Int -> Doc
newline n = Break (Hard n)

-- | Analogous to 'Prelude.unlines', terminate each Doc with a newline.
unlines :: [Doc] -> Doc
unlines [] = mempty
unlines docs = mconcat (List.intersperse (newline 1) docs) <> newline 1

-- | This is just like 'unlines', but separate docs with two newlines, and
-- terminate the last with one.
paragraphs :: [Doc] -> Doc
paragraphs [] = mempty
paragraphs docs = mconcat (List.intersperse (newline 2) docs) <> newline 1


wrapWords :: [Doc] -> Doc
wrapWords (d:ds) = List.foldl' (<+/>) d ds
wrapWords [] = mempty

wrap :: [Doc] -> Doc
wrap (d:ds) = List.foldl' (</>) d ds
wrap [] = mempty

-- * render

-- | Width of monospace text, in characters.
type Width = Int

data State = State {
    -- | Collect text for each Section, for 'sectionB'.
    stateCollect :: !B
    -- | Collect long form Sections for 'sectionSubs'.  Like stateSections,
    -- this is in reverse order.
    , stateSubs :: ![Section]
    -- | Collect sections in reverse order.
    , stateSections :: ![Section]

    -- | Track the current indent.  This is updated whenever I see a Indented.
    , stateIndent :: !Indent
    -- | This is the previous value of 'stateIndent'.  It's needed because it
    -- goes into 'sectionEndIndent', details there.
    , statePreviousIndent :: !Indent
    -- | Indent for the next break.  This is different from 'stateIndent',
    -- which is the current indent value because the new indent only applies
    -- *after* the next break.  Also, after a dedent I still need the indented
    -- value to apply to the section.
    , stateBreakIndent :: !Indent
    } deriving (Show)

data Section = Section {
    -- | This is the indent in effect when the section starts, and will be the
    -- physical indent of the section if it gets wrapped.
    sectionStartIndent :: !Indent
    -- | The indent of this section for breaking, see [NOTE end-indent].
    , sectionEndIndent :: !Indent
    -- | Text of the section.
    , sectionB :: !B
    -- | If present, the B is a short version.  If it doesn't fit on a line of
    -- its own, then flatten the subs.  Normally I put down Sections until
    -- I have to break.  A short layout stands in for a list of Sections.  When
    -- I see one, I try to place it, and if it doesn't fit, use the
    -- sub-Sections.
    , sectionSubs :: ![Section]
    , sectionBreak :: !BreakType
    } deriving (Show)

{- [NOTE end-indent]
    If there are multiple 'withIndent's during a section, a Section will start
    with one indent but end with another.  This is important because while the
    physical indent is 'sectionStartIndent', when it wraps it should be counted
    as the sectionEndIndent.  For example, given

    "{ " Indent "[ " Indent "k1" Break ": v1" Dedent Break
        ", " Indent "k2" Break ": v2" Dedent Dedent

    0 1 2 /  1/ 2 /     <- current indent / break
    01    22  11  22    <- section (start indent, end indent)
    { [ k1: v1
              , k2: v2

    by the time I get to the first Break, I'm logically at a syntactic indent
    level of 1, even though the physical indent is 0.  So when I later wrap,
    I need to count "{ [ k1" as a level 1 rather than 0.  Otherwise, it will
    prefer to break right there, and I will get this:

    { [ k1:
        v1
      , k2: v2

    instead of this:

    { [ k1: v1
      , k2: v2
-}

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
        , statePreviousIndent = 0
        }
    go doc state = case doc of
        Text t -> state { stateCollect = stateCollect state <> bFromText t }
        d1 :+ d2 -> go d2 (go d1 state)
        ShortForm short long -> state
            { stateCollect =
                stateCollect state <> renderSectionsB (flatten short)
            -- Whatever is in stateSubs state will be lost.  But if I collect
            -- it into stateSubs I get even more confusing results.  I can't
            -- figure out how to get ShortForm <> ShortForm to yield something
            -- sensible, so I'm giving up for now.
            , stateSubs = stateSections sub
            }
            where
            -- I need a break to collect the last part of the long form
            -- sub-doc.  But I can only know the break once I see it, after
            -- this ShortForm.  So this break is temporary and will be replaced
            -- by 'replaceBreaks' below.  If you see a Hard 0 in the Sections
            -- you know this failed.
            sub = addSection (Hard 0) $ go long subState
                where
                subState = initialState
                    -- This causes @a <> ShortForm b c@ to distribute the @a@
                    -- over @b@ and @c@, as documented by 'shortForm'.
                    { stateCollect = stateCollect state
                    , stateIndent = stateIndent state
                    , statePreviousIndent = statePreviousIndent state
                    , stateBreakIndent = stateBreakIndent state
                    }
        Indent n -> state
            { stateIndent = stateIndent state + n
            , statePreviousIndent = stateIndent state
            }
        Break break -> addSection break state
    -- When I see a Break, I can create a Section for it.
    addSection break state = state
        { stateCollect = mempty
        , stateSubs = []
        , stateSections = (: stateSections state) $ Section
            { sectionStartIndent = stateBreakIndent state
            , sectionEndIndent = statePreviousIndent state
            , sectionB = stateCollect state
            -- If there are subs, then they have been collected by the long
            -- part of a ShortForm.
            , sectionSubs = replaceBreaks $ stateSubs state
            , sectionBreak = break
            }
        , stateBreakIndent = stateIndent state
        , statePreviousIndent = stateIndent state
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
        | not (bNull (stateCollect state)) = addSection (Hard 1) state
        | final : sections <- stateSections state = state
            { stateSections = final { sectionBreak = Hard 1 } : sections }
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

-- | Render a Doc, wrapping after the given Width.
render :: Text -> Width -> Doc -> Lazy.Text
render indent width = renderText indent width . flatten

-- | Render the Doc all on one line, with no newlines.
renderFlat :: Doc -> Lazy.Text
renderFlat = renderTextFlat . flatten

-- | Take sections until they go over the width, or I see a hard newline, or
-- run out.  If they went over, then find a break in the collected, emit before
-- the break, and try again.
renderText :: Text -> Width -> [Section] -> Lazy.Text
renderText indentS maxWidth = Builder.toLazyText . flip renderLine mempty
    where
    renderLine [] out = out
    renderLine (Section indent _ b subs _ : sections) out
        -- break is ignored, because the last sub should have the same break.
        | not (null subs) && indent * textWidth indentS + bWidth b > maxWidth =
            renderLine (subs ++ sections) out
    renderLine allSections@(section1 : _) out =
        out <> case spanLine (textWidth indentS) maxWidth allSections of
            (_, _, [], []) -> mempty
            (_, _, [], section : sections) ->
                renderLine sections (emitLine (sectionBuilder section))
            (_, False, line, rest) ->
                renderLine rest (emitLine (renderSections line))
            (_, True, line, rest) -> case findBreak line of
                ([], []) -> mempty -- shouldn't be possible
                ([], section : sections) -> renderLine (sections ++ rest)
                    (emitLine (sectionBuilder section))
                (line, rest2) ->
                    renderLine (rest2 ++ rest) (emitLine (renderSections line))
        where
        indent = sectionStartIndent section1
        emitLine b = indentB <> b <> nl
            where
            indentB = mconcat $ replicate indent indent1
            nl = Builder.singleton '\n'
    indent1 = Builder.fromText indentS

{- This is for debugging, but I don't want it in the real version, and I don't
know how to make the debugging part free when disabled.

data Output = Output {
    outputLogs :: ![Text]
    , outputText :: !Builder.Builder
    } deriving (Show)

instance Monoid Output where
    mempty = Output mempty mempty
    mappend (Output logs1 text1) (Output logs2 text2) =
        Output (logs1<>logs2) (text1<>text2)

renderLogged :: Text -> Width -> Doc -> (Lazy.Text, [Text])
renderLogged indent width doc = (Builder.toLazyText builder, logs)
    where Output logs builder = renderTextLogged indent width (flatten doc)

renderTextLogged :: Text -> Width -> [Section] -> Output
renderTextLogged indentS maxWidth = flip renderLine mempty
    where
    renderLine [] out = out
    renderLine (Section indent _ b subs _ : sections) out
        -- break is ignored, because the last sub should have the same break.
        | not (null subs) && indent * textWidth indentS + bWidth b > maxWidth =
            log ("short version " <> showt b <> " too wide: "
                    <> showt (indent * textWidth indentS) <> " + "
                    <> showt (bWidth b))
                <> renderLine (subs ++ sections) out
    renderLine allSections@(section1 : _) out =
        out <> case spanLine (textWidth indentS) maxWidth allSections of
            (_, _, [], []) -> mempty
            (msg, _, [], section : sections) -> log msg
                <> log ("single section doesn't fit, it gets its own line: "
                    <> showt section)
                <> renderLine sections (emitLine (sectionBuilder section))
            (msg, False, line, rest) -> log msg
                <> log ("hard break or dedent: "
                    <> showt (map eSection line)
                    <> " rest: " <> showt (map eSection rest))
                <> renderLine rest (emitLine (renderSections line))
            (msg, True, line, rest) -> log msg <> case findBreak line of
                ([], []) -> mempty -- shouldn't be possible
                ([], section : sections) ->
                    log ("break before lowest indent: "
                        <> showt (sectionBuilder section)
                        <> " " <> showt (map eSection line))
                    <> renderLine (sections ++ rest)
                        (emitLine (sectionBuilder section))
                (line, rest2) ->
                    log ("break on lowest indent: "
                        <> showt (renderSections line))
                    <> renderLine (rest2 ++ rest)
                        (emitLine (renderSections line))
        where
        indent = sectionStartIndent section1
        emitLine b = txt (indentB <> b <> nl)
            where
            indentB = mconcat $ replicate indent indent1
            nl = Builder.singleton '\n'
    indent1 = Builder.fromText indentS
    txt t = mempty { outputText = t }
    log t = mempty { outputLogs = [t] }
    eSection s = (sectionStartIndent s, sectionB s)
-}

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
spanLine :: Width -> Width -> [Section]
    -> (Text, Bool, [Section], [Section])
    -- ^ (log, break, pre_break, post_break).  If break is False, then pre_break
    -- can be emitted as-is.  If break is True, then the line went over the
    -- maxWidth and must be broken.  pre_break will then have one Section past
    -- the break point.  This is so 'findBreak' can know what the next indent
    -- break is, so it can know if it's ok to break there.
spanLine _ _ [] = ("out of sections", False, [], [])
spanLine indentWidth maxWidth sections@(section1 : _) =
    go (indentWidth * indent) sections
    where
    indent = sectionStartIndent section1
    go _ [] = ("out of sections", False, [], [])
    go col (section : sections)
        -- Break as soon as the indent goes below the initial indent.
        | sectionStartIndent section < indent =
            ( "indent decreased: " <> showt (sectionStartIndent section)
                <> " < " <> showt indent
            , False, [], section : sections
            )
        | col + width > maxWidth =
            ( "too wide: " <> showt col <> " + "
                <> showt width <> " > " <> showt maxWidth
            , True, [section], sections
            )
        | Hard n <- sectionBreak section, n > 0 =
            ( "hard break"
            , False, [section]
            , if n > 1 then strip (Hard (n-1)) section : sections else sections
            )
        | otherwise =
            let (msg, break, pre, post) = go (col + space + width) sections
            in (msg, break, section : pre, post)
        where
        space = if sectionBreak section == Space then 1 else 0
        width = bWidth (sectionB section)
    strip break section = Section
        { sectionStartIndent = sectionStartIndent section
        , sectionEndIndent = sectionEndIndent section
        , sectionB = mempty
        , sectionSubs = []
        , sectionBreak = break
        }

-- | Given a list of Sections that I know need to be broken, find the best
-- place to break.  Split before the last lowest indent.
findBreak :: [Section] -> ([Section], [Section])
findBreak sections = case lowest of
    Nothing -> ([], [])
    Just (i, _) -> splitAt i sections
    where lowest = lastMinimumOn snd $ zip [0..] (map sectionEndIndent sections)

showt :: Show a => a -> Text
showt = Text.pack . show


-- * B

-- | A 'Builder.Builder' that keeps track of its length.
data B = B {
    bBuilder :: !Builder.Builder
    , bWidth :: !Width
    }

instance Show B where
    show (B b _) = show b

instance Semigroup B where
    B b1 len1 <> B b2 len2 = B (b1<>b2) (len1+len2)

instance Monoid B where
    mempty = B mempty 0
    mappend = (<>)
    mconcat [] = mempty
    mconcat bs = B (mconcat builders) (Num.sum lens)
        where (builders, lens) = unzip [(b, len) | B b len <- bs, len /= 0]

bFromText :: Text -> B
bFromText text = B (Builder.fromText text) (textWidth text)

-- | Number of columns this text should need in a monospace font.
textWidth :: Text -> Width
textWidth = Text.foldl' (\n c -> n + WCWidth.wcwidth c) 0

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
    _ -> doc

-- | Reduce the Doc to a flattened easier to read version.
denest :: Doc -> Text
denest doc = case doc of
    d1 :+ d2 -> denest d1 <> " " <> denest d2
    Text t -> showt t
    Indent n -> if n > 0 then "^" else "v"
    Break b -> case b of
        NoSpace -> "/"
        Space -> "/+"
        Hard _ -> "//"
    ShortForm _ doc -> "(" <> denest doc <> ")"


-- * misc

-- | Find the *last* minimum element.
lastMinimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
lastMinimumOn _ [] = Nothing
lastMinimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x <= key low then x else low

-- | Interleave so there is a y between each x.
interleave :: [a] -> [a] -> [a]
interleave [x] _ = [x]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs _ = xs
