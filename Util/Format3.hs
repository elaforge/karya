-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Simple text formatter.  The pretty print libraries never seem to do what
-- I want and are hard to control.
module Util.Format3 where
import Prelude hiding (words)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.String as String
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder

import qualified Util.Debug as Debug
import Util.PPrint (pprint)


-- * Doc

data Doc =
    Text !Text
    | Union Doc Doc -- intentionally lazy
    -- | Line break.  If the Indent is non-zero, this increments the indent
    -- level.
    | Break !Space !Indent Doc
    deriving (Show)

instance Monoid.Monoid Doc where
    mempty = Text mempty
    mappend = Union

instance String.IsString Doc where
    fromString = Text . String.fromString

-- | Space becomes a space when it doesn't break, NoSpace doesn't.
data Space = Space | NoSpace deriving (Show)

instance Monoid.Monoid Space where
    mempty = NoSpace
    mappend Space _ = Space
    mappend _ Space = Space
    mappend _ _ = NoSpace

type Indent = Int

(</>) :: Doc -> Doc -> Doc
d1 </> d2 = d1 <> Break Space 0 d2
infixr 5 </> -- less than <>

indented :: Doc -> Doc
indented = Break Space 1

break :: Doc -> Doc
break = Break Space 0

-- | Join two docs with a space.
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> Text " " <> d2
infixr 6 <+> -- same as <>


-- * render

-- | Width of monospace text, in characters.
type Width = Int

data State = State {
    stateOutput :: !Builder.Builder
    , stateIndent :: !Indent
    , stateCollect :: !Collect
    , stateCollectIndent :: !Indent
    -- | Breaks keeps track of the possible breaks not yet committed to.
    --
    -- A break is required when the current column goes over the limit.
    , stateBreaks :: !Breaks
    } deriving (Show)

type Breaks = Map.Map Indent Chunk

render :: Text -> Width -> Doc -> Lazy.Text
render indentText_ maxWidth doc =
    Builder.toLazyText $ stateOutput $ flush indentText $
        Debug.trace "final" $ go doc initialState
    where
    initialState = State
        { stateOutput = mempty
        , stateIndent = 0
        , stateCollect = mempty
        , stateCollectIndent = 0
        , stateBreaks = mempty
        }
    go doc state = case doc of -- case Debug.traces "doc" (state, doc) doc of
        Text txt -> goText txt state
        Union d1 d2 -> go d2 (go d1 state)
        Break space indent doc -> goBreak space indent doc state
    goBreak space indent doc state =
        (go doc $ addIndent indent $ flushCollect indentText space state)
        { stateIndent = stateIndent state }
    goText txt state =
        List.foldl' (appendLine indentText maxWidth) state (splitLines txt)
    indentText = bFromText indentText_

flush :: B -> State -> State
flush indentText = flushBreaks . flushCollect indentText NoSpace
    where
    flushBreaks state = state
        { stateOutput = stateOutput state
            <> flatten (Map.toAscList (stateBreaks state))
        }
    flatten [] = mempty
    flatten ((indent, chunk) : rest) =
        mconcat (replicate indent (bToBuilder indentText))
            <> unchunk (mconcat (chunk : map snd rest))
    unchunk (Chunk b _) = bToBuilder b

addIndent :: Indent -> State -> State
addIndent indent state = state { stateIndent = stateIndent state + indent }

-- | Flushes collect and force breaks until there are none left.  This is
-- because I never skip an indent level.
hardBreak :: B -> State -> State
hardBreak indentText = clearBreaks . flushCollect indentText NoSpace
    where
    clearBreaks state = case lowestBreak (stateBreaks state) of
        Nothing -> state
        Just break -> clearBreaks $ breakLine indentText break state

flushCollect :: B -> Space -> State -> State
flushCollect indentText space state
    | bNull (stateCollect state) = state
    | Just break@(indent, _) <- lowestBreak breaks, cindent < indent =
        -- If I reduce the indent then text that was supposed to be indented
        -- more than the collected text, it will lose the indentation.  So
        -- I have to emit it first.
        flushCollect indentText space $ breakLine indentText break state
    | otherwise = state
        { stateBreaks =
            insertBreak cindent (Chunk (stateCollect state) space) breaks
        , stateCollect = mempty
        }
    where
    cindent = stateCollectIndent state
    breaks = stateBreaks state

insertBreak :: Indent -> Chunk -> Breaks -> Breaks
insertBreak indent chunk breaks = Map.insert indent (above <> chunk) lower
    where
    above = mconcat $ maybe mempty id at : map snd (Map.toAscList higher)
    (lower, at, higher) = Map.splitLookup indent breaks

-- | New text is broken into lines and each line is processed separately.
-- Each line is checked for overflow.  If it's under, then it gets appended to
-- the post of a Break at the current indent.  If it's over, and there's
-- a pending break, I have to break a line.  If there's no pending break then
-- I just have to go over the width.
appendLine :: B -> Width -> State -> Maybe Text -> State
appendLine indentText _ state Nothing = hardBreak indentText state
appendLine indentText maxWidth state (Just txt)
    | overflow, Just minBreak <- lowestBreak breaks =
        retry (breakLine indentText minBreak state) (Just txt)
    | otherwise = state
        { stateCollect = stateCollect state <> bFromText txt
        , stateCollectIndent = stateIndent state
        }
    where
    retry = appendLine indentText maxWidth
    breaks = stateBreaks state
    overflow = breaksWidth (bWidth indentText) breaks
        + bWidth (stateCollect state) + textWidth txt
        > maxWidth

-- | I choose the break with the lowest indent.  The text before the break
-- goes into 'stateOutput', plus a newline and the relevant amount of indent.
-- This reduces the current column since I removed a Break, so try again to
-- append the text.
breakLine :: B -> (Indent, Chunk) -> State -> State
breakLine indentText (indent, Chunk b _space) state = state
    { stateOutput = stateOutput state <> indentB <> bToBuilder b <> newline
    -- , stateCollect = mempty
    , stateBreaks = Map.delete indent (stateBreaks state)
    }
    where
    indentB = mconcat $ replicate indent $ bToBuilder indentText
    newline = Builder.singleton '\n'

type Collect = B

lowestBreak :: Breaks -> Maybe (Indent, Chunk)
lowestBreak breaks
    | Map.null breaks = Nothing
    | otherwise = Just (Map.findMin breaks)

-- | Replace newlines with Nothing.
splitLines :: Text -> [Maybe Text]
splitLines = List.intersperse Nothing . map Just . Text.split (=='\n')

-- * break

data Chunk = Chunk !B !Space deriving (Show)

instance Monoid.Monoid Chunk where
    mempty = Chunk mempty NoSpace
    mappend (Chunk b1 space1) (Chunk b2 space2)
        | bNull b2 = Chunk b1 (space1 <> space2)
        | otherwise = Chunk (b1 <> spaceToB space1 <> b2) space2

spaceToB :: Space -> B
spaceToB NoSpace = mempty
spaceToB Space = B (Builder.singleton ' ') 1

spaceWidth :: Space -> Width
spaceWidth NoSpace = 0
spaceWidth Space = 1

breaksWidth :: Width -> Breaks -> Width
breaksWidth indentWidth breaks = case Map.toAscList breaks of
    [] -> 0
    pairs@((indent, _) : _) -> go (map snd pairs) + indent * indentWidth
    where
    go [] = 0
    go [Chunk b _] = bWidth b
    go (Chunk b space : rest) = bWidth b + spaceWidth space + go rest

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

bToBuilder :: B -> Builder.Builder
bToBuilder (B b _) = b

-- | TODO take double-width characters into account
textWidth :: Text -> Width
textWidth = Text.length

bNull :: B -> Bool
bNull = (==0) . bWidth
