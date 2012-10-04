{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- | Pull deriver call documentation out of a Performance and format it nicely.
module Cmd.CallDoc where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import Data.Text (Text)

import Util.Control
import qualified Util.Format as Format
import qualified Util.Pretty as Pretty

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Derive as Derive
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackInfo as TrackInfo

import Types


-- * output

-- | Convert a Document to plain text.
doc_text :: Document -> Text.Text
doc_text = Format.run . mapM_ section
    where
    section (call_type, scope_docs) = do
        Format.write $ "## " <> call_type <> " calls" <> "\n\n"
        mapM_ scope_doc scope_docs
    scope_doc (source, calls) = do
        Format.write $ "### from " <> source <> "\n\n"
        mapM_ named_call_doc calls
    named_call_doc (shadowed, sym, (name, sections)) = do
        Format.write $ if shadowed then strikeout sym else sym
        Format.write $ " -- " <> name <> ": "
        show_sections sections
        Format.newline
    strikeout sym = "~~" <> sym <> "~~ (shadowed)"
    show_sections [(ValCall, Derive.CallDoc doc args)] = do
        write_doc doc
        Format.indented 2 $ arg_docs args
    show_sections sections = Format.indented 2 $ do
        Format.newline
        mapM_ call_section sections
    call_section (call_type, Derive.CallDoc doc args) = do
        Format.write $ show_call_type call_type <> ": "
        write_doc doc
        Format.indented 2 $ arg_docs args
    arg_docs (Derive.ArgsParsedSpecially doc) = do
        Format.write $ "Args parsed by call: "
        write_doc doc
    arg_docs (Derive.ArgDocs args) = mapM_ arg_doc args
    arg_doc (Derive.ArgDoc name typ deflt doc) = do
        Format.write $ Text.pack name <> " :: "
            <> Text.pack (Pretty.pretty typ) <> show_default deflt <> " -- "
        write_doc doc
    show_default = maybe "" ((" = "<>) . Text.pack)

write_doc :: String -> Format.FormatM ()
write_doc text = do
    Format.wrapped_words 75 4 (Text.pack text)
    Format.newline

-- ** html output

-- | Convert a Document to HTML.
doc_html :: Document -> Text.Text
doc_html = un_html . (header <>) . mconcatMap section
    where
    section (call_type, scope_docs) =
        tag "h2" (html call_type) <> "\n\n"
        <> mconcatMap scope_doc scope_docs
    scope_doc (source, calls) =
        tag "h3" ("from " <> html source) <> "\n\n<dl>\n"
        <> mconcatMap named_call_doc calls
        <> "</dl>\n"
    named_call_doc (shadowed, sym, (name, sections)) =
        "<dt>" <> (if shadowed then strikeout sym else tag "code" (html sym))
        <> " &mdash; " <> tag "b" (html name) <> ":\n"
        <> "<dd> <dl class=compact\n" <> show_sections sections <> "</dl>\n\n"
    strikeout sym = tag "strike" (tag "code" (html sym))
        <> tag "em" "(shadowed)"
    show_sections sections = mconcatMap call_section sections
    call_section (call_type, Derive.CallDoc doc args) =
        "<dt>" <> tag "em" (html (show_call_type call_type)) <> ": "
        <> "<dd>" <> html_doc doc <> "\n<dd>" <> tag "ul" (arg_docs args)
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "\n<li><b>Args parsed by call:</b> " <> html_doc doc
    arg_docs (Derive.ArgDocs args) = mconcatMap arg_doc args
    arg_doc (Derive.ArgDoc name typ deflt doc) =
        "<li>" <> tag "code" (html (Text.pack name)) <> " :: "
        <> tag "em" (html (Text.pack (Pretty.pretty typ)))
        <> show_default deflt <> " &mdash; " <> html_doc doc <> "\n"
    show_default = maybe "" ((" = " <>) . tag "code" . html . Text.pack)

    header = "<style type=text/css>\n" <> css <> "</style>\n"
    css = "dl.compact {\n\
        \    margin: 0px;\n\
        \    padding: 0;\n\
        \    border-bottom: 1px solid #999;\n\
        \}\n\
        \.compact dt {\n\
        \    margin: 0;\n\
        \    padding: 0;\n\
        \}\n\
        \.compact dd {\n\
        \    margin: 0 0 1em 0;\n\
        \    padding: 0;\n\
        \}\n"

    tag :: Html -> Html -> Html
    tag name content = "<" <> name <> ">" <> content <> "</" <> name <> ">"
    mconcatMap f = mconcat . map f

newtype Html = Html Text.Text
    deriving (Monoid.Monoid, String.IsString, Show)

un_html :: Html -> Text.Text
un_html (Html text) = text

html :: Text.Text -> Html
html = Html . Text.replace "<" "&lt;" . Text.replace ">" "&gt;"
        . Text.replace "&" "&amp;"

html_doc :: String -> Html
html_doc = map_html postproc . html . Text.pack
    where
    map_html f (Html text) = Html (f text)
    postproc = para . backticks
    para = Text.replace "\n" "\n<p>"
    backticks = mconcat . codify . Text.split (=='`')
    -- foo `bar` ba`z` -> ["foo ", "bar", "ba", "z", ""]
    codify (x:y:zs) = x : ("<code>" <> y <> "</code>") : codify zs
    codify xs = xs


-- * doc

-- | Document is an intermediate format between Derive.Scope and the eventual
-- textual output.
type Document = [Section]

-- | Get documentation for calls in scope at the given block and track.
track :: (Cmd.M m) => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require_msg "dynamic for doc"
        =<< Perf.lookup_dynamic block_id (Just track_id)
    ttype <- TrackInfo.track_type <$> State.get_track_title track_id
    return $ generate ttype dynamic

-- | (call type, docs)
type Section = (Text, [ScopeDoc])

generate :: TrackInfo.Type -> Derive.Dynamic -> [Section]
generate ttype dynamic = (\d -> [d, val_doc]) $ case ttype of
        TrackInfo.ControlTrack -> ("control", scope_doc control)
        TrackInfo.TempoTrack -> ("tempo", scope_doc control)
        TrackInfo.PitchTrack -> ("pitch", scope_doc pitch)
        TrackInfo.NoteTrack -> ("note", scope_doc note)
    where
    val_doc = ("val", scope_doc val)
    Derive.Scope note control pitch val = Derive.state_scope dynamic

-- | Documentation for one type of scope: (scope_source, calls)
type ScopeDoc = (Text, [NamedCallDoc])

-- | Walk up the scopes, keeping track of shadowed names.
scope_doc :: Derive.ScopeType call -> [ScopeDoc]
scope_doc (Derive.ScopeType inst scale builtin) = filter (not . null . snd)
    [ ("instrument", lookup_docs (map Derive.lookup_docs inst))
    , ("scale", lookup_docs (map Derive.lookup_docs scale))
    , ("builtins", lookup_docs (map Derive.lookup_docs builtin))
    ]

-- | (is_shadowed, bound_symbol, doc)
type NamedCallDoc = (Bool, Text, DocumentedCall)

lookup_docs :: [Derive.LookupDocs] -> [NamedCallDoc]
lookup_docs = snd . List.mapAccumL go Set.empty . concatMap flatten
    where
    flatten (Derive.LookupPattern lookup_doc call) = [(Left lookup_doc, call)]
    flatten (Derive.LookupMap cmap) =
        [(Right sym, call) | (sym, call) <- Map.toAscList cmap]
    go shadowed (Left lookup_doc, call) =
        -- There's no way to know if a programmatic lookup shadows.
        (shadowed, (False, "lookup: " <> Text.pack lookup_doc,
            documented_call call))
    go shadowed (Right sym, call) = (Set.insert sym shadowed,
        (sym `Set.member` shadowed, show_sym sym, documented_call call))
    show_sym sym
        | null s = "\"\""
        | otherwise = Text.pack s
        where s = ShowVal.show_val sym

-- | (name, sections)
type DocumentedCall = (Text, [(CallType, Derive.CallDoc)])
data CallType = ValCall | GeneratorCall | TransformerCall
    deriving (Eq, Show)

show_call_type :: CallType -> Text
show_call_type ValCall = "val"
show_call_type GeneratorCall = "generator"
show_call_type TransformerCall = "transformer"

documented_call :: Derive.DocumentedCall -> DocumentedCall
documented_call (Derive.DocumentedCall name generator transformer) =
    (Text.pack name,
        doc GeneratorCall generator ++ doc TransformerCall transformer)
    where doc typ = maybe [] ((:[]) . ((,) typ))
documented_call (Derive.DocumentedValCall name cdoc) =
    (Text.pack name, [(ValCall, cdoc)])
