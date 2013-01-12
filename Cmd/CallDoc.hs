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
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackInfo as TrackInfo

import Types


-- * output

-- | Convert a Document to plain text.
doc_text :: Document -> Text.Text
doc_text = Format.run (Just 75) . mapM_ section
    where
    section (call_type, scope_docs) = do
        Format.write $ "## " <> call_type <> " calls" <> "\n\n"
        mapM_ scope_doc scope_docs
    scope_doc (source, calls) = do
        Format.write $ "### from " <> source <> "\n\n"
        mapM_ call_bindings_text calls

call_bindings_text :: CallBindings -> Format.FormatM ()
call_bindings_text (binds, sections) = do
        mapM_ show_bind binds
        Format.indented 2 $ show_sections sections
        Format.newline
    where
    show_bind (shadowed, sym, name) = do
        Format.write $ if shadowed then strikeout sym else sym
        Format.write $ " -- " <> name <> ":\n"
    strikeout sym = "~~" <> sym <> "~~ (shadowed)"
    show_sections [(ValCall, Derive.CallDoc doc args)] = do
        write_doc doc
        Format.indented 2 $ arg_docs args
    show_sections sections = mapM_ call_section sections
    call_section (call_type, Derive.CallDoc doc args) = do
        Format.write $ show_call_type call_type <> ": "
        write_doc doc
        Format.indented 2 $ arg_docs args
    arg_docs (Derive.ArgsParsedSpecially doc) = do
        Format.write "Args parsed by call: "
        write_doc doc
    arg_docs (Derive.ArgDocs args) = mapM_ arg_doc args
    arg_doc (Derive.ArgDoc name typ parser doc) = do
        Format.write $ Text.pack name <> fromMaybe mempty char <> " :: "
            <> Text.pack (Pretty.pretty typ) <> maybe "" (" = "<>) deflt
            <> " -- "
        write_doc doc
        where (char, deflt) = show_parser parser

write_doc :: String -> Format.FormatM ()
write_doc text = do
    Format.wrapped_words 4 (Text.pack text)
    Format.newline

show_parser :: Derive.ArgParser -> (Maybe Text, Maybe Text)
show_parser p = case p of
    Derive.Required -> (Nothing, Nothing)
    Derive.Defaulted deflt -> (Nothing, Just (Text.pack deflt))
    Derive.Optional -> (Just "?", Nothing)
    Derive.Many -> (Just "*", Nothing)
    Derive.Many1 -> (Just "+", Nothing)

-- ** html output

-- | Convert a Document to HTML.
doc_html :: Document -> Text.Text
doc_html = un_html . (html_header <>) . mconcatMap section
    where
    section (call_type, scope_docs) =
        tag "h2" (html call_type) <> "\n\n"
        <> mconcatMap scope_doc scope_docs
    scope_doc (source, calls) =
        tag "h3" ("from " <> html source) <> "\n\n<dl class=main>\n"
        <> mconcatMap call_bindings_html calls
        <> "</dl>\n"

html_header :: Html
html_header = "<style type=text/css>\n" <> css <> "</style>\n"
    where
    css = ".main dl { border-bottom: 1px solid #999 }\n"
        <> "dl.compact {\n"
        <> "    margin: 0px;\n"
        <> "    padding: 0;\n"
        <> "}\n"
        <> ".compact dt {\n"
        <> "    margin: 0;\n"
        <> "    padding: 0;\n"
        <> "}\n"
        <> ".compact dd {\n"
        <> "    margin: 0 0 1em 0;\n"
        <> "    padding: 0;\n"
        <> "}\n"

call_bindings_html :: CallBindings -> Html
call_bindings_html (binds, sections) =
    mconcatMap show_bind binds <> show_sections sections <> "\n\n"
    where
    show_bind (shadowed, sym, name) =
        "<dt>" <> (if shadowed then strikeout sym else tag "code" (html sym))
        <> " &mdash; " <> tag "b" (html name) <> ":\n"
    strikeout sym = tag "strike" (tag "code" (html sym))
        <> tag "em" "(shadowed)"
    show_sections [(ValCall, Derive.CallDoc doc args)] =
        "<dd>" <> html_doc doc <> "\n<dd>" <> tag "ul" (arg_docs args)
    show_sections sections = "<dd> <dl class=compact>\n"
        <> mconcatMap call_section sections <> "</dl>\n"
    call_section (call_type, Derive.CallDoc doc args) =
        "<dt>" <> tag "em" (html (show_call_type call_type)) <> ": "
        <> "<dd>" <> html_doc doc <> "\n<dd>" <> tag "ul" (arg_docs args)
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "\n<li><b>Args parsed by call:</b> " <> html_doc doc
    arg_docs (Derive.ArgDocs args) = mconcatMap arg_doc args
    arg_doc (Derive.ArgDoc name typ parser doc) =
        "<li>" <> tag "code" (html (Text.pack name)) <> show_char char
        <> " :: " <> tag "em" (html (Text.pack (Pretty.pretty typ)))
        <> show_default deflt <> " &mdash; " <> html_doc doc <> "\n"
        where (char, deflt) = show_parser parser
    show_default = maybe "" ((" = " <>) . tag "code" . html)
    show_char = maybe "" (tag "sup" . html)

tag :: Html -> Html -> Html
tag name content = "<" <> name <> ">" <> content <> "</" <> name <> ">"

mconcatMap :: (Monoid.Monoid b) => (a -> b) -> [a] -> b
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
    para = Text.replace "\n" "\n<br>"
    backticks = mconcat . codify . Text.split (=='`')
    -- foo `bar` ba`z` -> ["foo ", "bar", "ba", "z", ""]
    codify (x:y:zs) = x : ("<code>" <> y <> "</code>") : codify zs
    codify xs = xs

-- * scale doc

type Scale = [CallBindings]

scales_html :: [Scale] -> Text.Text
scales_html scales = un_html $ html_header
        <> "<h2> Scales </h2>\n"
        <> "<dl class=main>\n" <> mconcatMap scale_html scales
        <> "</dl>\n"
    where scale_html = mconcatMap call_bindings_html

scale_doc :: Scale.Scale -> Scale
scale_doc scale =
    lookup_docs [Derive.lookup_docs $ Derive.scale_to_lookup scale]

-- * doc

-- | Document is an intermediate format between Derive.Scope and the eventual
-- textual output.
type Document = [Section]

-- | Emit docs for all calls in the default scope.
builtin :: (Cmd.M m) => m Document
builtin = all_sections <$> Cmd.gets Cmd.state_global_scope

all_sections :: Derive.Scope -> [Section]
all_sections (Derive.Scope note control pitch val) =
    [ ("note", scope_doc note)
    , ("control", scope_doc control)
    , ("pitch", scope_doc pitch)
    , ("val", scope_doc val)
    ]

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrument_calls :: Derive.InstrumentCalls -> [ScopeDoc]
instrument_calls (Derive.InstrumentCalls notes vals) =
    [ ("note", lookup_docs (map Derive.lookup_docs notes))
    , ("val", lookup_docs (map Derive.lookup_docs vals))
    ]

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: (Cmd.M m) => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require_msg "dynamic for doc"
        =<< Perf.lookup_dynamic block_id (Just track_id)
    ttype <- TrackInfo.track_type <$> State.get_track_title track_id
    return $ track_sections ttype (Derive.state_scope dynamic)

-- | (call type, docs)
type Section = (Text, [ScopeDoc])

track_sections :: TrackInfo.Type -> Derive.Scope -> [Section]
track_sections ttype (Derive.Scope note control pitch val) =
    (\d -> [d, ("val", scope_doc val)]) $ case ttype of
        TrackInfo.NoteTrack -> ("note", scope_doc note)
        TrackInfo.ControlTrack -> ("control", scope_doc control)
        TrackInfo.TempoTrack -> ("tempo", scope_doc control)
        TrackInfo.PitchTrack -> ("pitch", scope_doc pitch)

-- | Documentation for one type of scope: (scope_source, calls)
type ScopeDoc = (Text, [CallBindings])

-- | Walk up the scopes, keeping track of shadowed names.
scope_doc :: Derive.ScopeType call -> [ScopeDoc]
scope_doc (Derive.ScopeType inst scale builtin) = filter (not . null . snd)
    [ ("instrument", lookup_docs (map Derive.lookup_docs inst))
    , ("scale", lookup_docs (map Derive.lookup_docs scale))
    , ("builtins", lookup_docs (map Derive.lookup_docs builtin))
    ]

-- | Multiple bound symbols with the same DocumentedCall are grouped together:
-- ([(is_shadowed, bound_symbol, call_name)], doc)
type CallBindings = ([(Bool, SymbolName, CallName)], DocumentedCall)
type CallName = Text
type SymbolName = Text

lookup_docs :: [Derive.LookupDocs] -> [CallBindings]
lookup_docs = group . snd . List.mapAccumL go Set.empty . concatMap flatten
    where
    flatten (Derive.LookupPattern pattern call) = [(Left pattern, call)]
    flatten (Derive.LookupMap cmap) =
        [(Right sym, call) | (sym, call) <- Map.toAscList cmap]
    go shadowed (Left pattern, call) =
        -- There's no way to know if a programmatic lookup shadows.
        (shadowed, ((False, "lookup: " <> Text.pack pattern),
            documented_call call))
    go shadowed (Right sym, call) = (Set.insert sym shadowed,
        ((sym `Set.member` shadowed, show_sym sym), documented_call call))
    show_sym sym
        | null s = "\"\""
        | otherwise = Text.pack s
        where s = ShowVal.show_val sym
    group :: [((Bool, SymbolName), (CallName, DocumentedCall))]
        -> [CallBindings]
    group pairs = [(extract names, doc_call)
        | (doc_call, names) <- Seq.keyed_group_on (snd . snd) pairs]
    extract names =
        [(shadowed, name, cname) | ((shadowed, name), (cname, _)) <- names]

type DocumentedCall = [(CallType, Derive.CallDoc)]
data CallType = ValCall | GeneratorCall | TransformerCall
    deriving (Eq, Ord, Show)

show_call_type :: CallType -> Text
show_call_type ValCall = "val"
show_call_type GeneratorCall = "generator"
show_call_type TransformerCall = "transformer"

documented_call :: Derive.DocumentedCall -> (CallName, DocumentedCall)
documented_call (Derive.DocumentedCall name generator transformer) =
    (Text.pack name,
        doc GeneratorCall generator ++ doc TransformerCall transformer)
    where doc typ = maybe [] ((:[]) . (,) typ)
documented_call (Derive.DocumentedValCall name cdoc) =
    (Text.pack name, [(ValCall, cdoc)])
