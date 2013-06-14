-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Pull deriver call documentation out of a Performance and format it nicely.
module Cmd.CallDoc where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text

import Util.Control
import qualified Util.File as File
import qualified Util.Format as Format
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Sig as Sig
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import Types


-- * output

-- | Convert a Document to plain text.
doc_text :: Document -> Text
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
    show_sections [(ValCall, Derive.CallDoc tags doc args)] = do
        write_doc doc
        write_tags tags
        Format.indented 2 $ arg_docs args
    show_sections sections = mapM_ call_section sections
    call_section (call_type, Derive.CallDoc tags doc args) = do
        Format.write $ show_call_type call_type <> ": "
        write_doc doc
        write_tags tags
        Format.indented 2 $ arg_docs args
    arg_docs (Derive.ArgsParsedSpecially doc) = do
        Format.write "Args parsed by call: "
        write_doc doc
    arg_docs (Derive.ArgDocs args) = mapM_ arg_doc args
    arg_doc (Derive.ArgDoc name typ parser doc) = do
        Format.write $ name <> fromMaybe mempty char <> " :: "
            <> txt (Pretty.pretty typ) <> maybe "" (" = "<>) deflt
            <> " -- "
        write_doc doc
        where (char, deflt) = show_parser parser
    write_tags tags
        | tags == mempty = return ()
        | otherwise = Format.write $
            "Tags: " <> Text.intercalate ", " (Tags.untag tags) <> "\n"

write_doc :: Text -> Format.FormatM ()
write_doc text = do
    Format.wrapped_words 4 text
    Format.newline

show_parser :: Derive.ArgParser -> (Maybe Text, Maybe Text)
show_parser p = case p of
    Derive.Required -> (Nothing, Nothing)
    Derive.Defaulted deflt -> (Nothing, Just deflt)
    Derive.Optional -> (Just "?", Nothing)
    Derive.Many -> (Just "*", Nothing)
    Derive.Many1 -> (Just "+", Nothing)

-- ** html output

-- | (haddock_dir, directory_tree)
type HtmlState = (FilePath, Set.Set FilePath)

get_html_state :: FilePath -> FilePath -> IO HtmlState
get_html_state haddock_dir app_dir = do
    files <- liftIO $ get_files app_dir
    -- The eventual output is in build/doc.
    return (haddock_dir, files)
    where
    get_files dir = do
        files <- File.listRecursive (maybe False Char.isUpper . Seq.head) dir
        return $ Set.fromList files

-- | Convert a Document to HTML.
doc_html :: HtmlState -> Document -> Text
doc_html hstate = un_html . (html_header <>) . mconcatMap section
    where
    section (call_kind, scope_docs) =
        tag "h2" (html call_kind) <> "\n\n"
        <> mconcatMap (scope_doc call_kind) scope_docs
    scope_doc call_kind (source, calls) =
        tag "h3" ("from " <> html source) <> "\n\n<dl class=main>\n"
        <> mconcatMap (call_bindings_html hstate call_kind) calls
        <> "</dl>\n"

html_header :: Html
html_header =
    "<meta charset=utf-8>\n"
    <> "<style type=text/css>\n" <> css <> "</style>\n"
    <> "<script>\n" <> javascript <> "</script>\n"
    <> mconcat (List.intersperse "; "
        [ "<code>arg = val</code> &mdash; arg with default"
        , "<code>arg<sup>?</sup></code> &mdash; optional arg"
        , "<code>arg<sup>*</sup></code> &mdash; zero or more args"
        , "<code>arg<sup>+</sup></code> &mdash; one or more args"
        ])
    <> "<br> <code>word</code> to include a tag, <code>-word</code> to\n\
        \exclude: <input type=text onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...)\n"

css :: Html
css = ".main dl { border-bottom: 1px solid #999 }\n\
    \dl.compact {\n\
    \    margin: 0px;\n\
    \    padding: 0;\n\
    \}\n\
    \ul { margin: 0; }\n\
    \div { margin-bottom: 10px; }\n\
    \.compact dt {\n\
    \    margin: 0;\n\
    \    padding: 0;\n\
    \}\n\
    \.compact dd {\n\
    \    margin: 0 0 1em 0;\n\
    \    padding: 0;\n\
    \}\n"

javascript :: Html
javascript =
    "var search = function(val) {\n\
    \   var search = val.split(/ +/).filter(function(x) { return x != '' });\n\
    \   var defs = document.getElementsByClassName('main');\n\
    \   for (var i = 0; i < defs.length; i++) {\n\
    \       for (var j = 0; j < defs[i].children.length; j++) {\n\
    \           var c = defs[i].children[j];\n\
    \           var tags = c.attributes.tags.value.split(' ');\n\
    \           c.hidden = !matches(search, tags);\n\
    \       }\n\
    \   }\n\
    \};\n\
    \var matches = function(search, tags) {\n\
    \   tags = tags.filter(function(x) { return x != '' });\n\
    \   return search.every(function(x) {\n\
    \       if (x[0] === '-')\n\
    \           return tags.indexOf(x.slice(1)) === -1;\n\
    \       else\n\
    \           return tags.indexOf(x) !== -1;\n\
    \    });\n\
    \};\n"

call_bindings_html :: HtmlState -> Text -> CallBindings -> Html
call_bindings_html hstate call_kind bindings@(binds, sections) =
    "<div tags=\"" <> html (Text.unwords tags) <> "\">"
    <> mconcatMap show_bind binds <> show_sections sections
    <> "</div>\n\n"
    where
    tags = call_kind : binding_tags bindings
    show_bind (shadowed, sym, name) =
        "<dt>" <> (if shadowed then strikeout sym else tag "code" (html sym))
        <> " &mdash; " <> tag "b" (html name) <> ":\n"
    strikeout sym = tag "strike" (tag "code" (html sym))
        <> tag "em" "(shadowed)"
    show_sections [(ValCall, Derive.CallDoc tags doc args)] =
        "<dd>" <> html_doc hstate doc <> write_tags tags <> "\n<dd>"
        <> tag "ul" (arg_docs args)
    show_sections sections = "<dd> <dl class=compact>\n"
        <> mconcatMap call_section sections <> "</dl>\n"
    call_section (call_type, Derive.CallDoc tags doc args) =
        "<dt>" <> tag "em" (html (show_call_type call_type)) <> ": "
        <> "<dd>" <> html_doc hstate doc <> write_tags tags <> "\n<dd>"
        <> tag "ul" (arg_docs args)
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "\n<li><b>Args parsed by call:</b> " <> html_doc hstate doc
    arg_docs (Derive.ArgDocs args) = mconcatMap arg_doc args
    arg_doc (Derive.ArgDoc name typ parser doc) =
        "<li>" <> tag "code" (html name) <> show_char char
        <> " :: " <> tag "em" (html (txt (Pretty.pretty typ)))
        <> show_default deflt <> " &mdash; " <> html_doc hstate doc <> "\n"
        where (char, deflt) = show_parser parser
    show_default = maybe "" ((" = " <>) . tag "code" . html)
    show_char = maybe "" (tag "sup" . html)
    write_tags tags
        | tags == mempty = ""
        | otherwise = "<br><b>Tags:</b> <em>"
            <> html (Text.intercalate ", " (Tags.untag tags))
            <> "</em>"

-- | Extract explicit tags as well as some implicit tags.  Implicit tags are
-- @%control@ for controls in the default arguments, @name-arg@ for environ
-- keys that default the arguments, and @note@, @control@, @pitch@, or @val@ for
-- the call kind.
binding_tags :: CallBindings -> [Text]
binding_tags (binds, dcall) = Seq.unique (concatMap extract dcall)
    where
    names = [name | (_, _, name) <- binds]
    extract (_, call_doc) =
        cdoc_tags call_doc ++ args_tags (Derive.cdoc_args call_doc)
    cdoc_tags = Tags.untag . Derive.cdoc_tags
    args_tags (Derive.ArgDocs args) = concatMap arg_tags args
    args_tags (Derive.ArgsParsedSpecially {}) = []
    arg_tags arg =
        [ unsym $ Sig.arg_environ_default name (Derive.arg_name arg)
        | name <- names
        ] ++ arg_control_tags (Derive.arg_parser arg)
    unsym (TrackLang.Symbol sym) = sym
    -- An arg with a control signal default should look like "%sig,.5".
    -- This is a hack, since the default isn't stored in a structured way.
    arg_control_tags (Derive.Defaulted deflt)
        | "%" `Text.isPrefixOf` deflt = [Text.takeWhile (/=',') deflt]
        | otherwise = []
    arg_control_tags _ = []

tag :: Html -> Html -> Html
tag name content = "<" <> name <> ">" <> content <> "</" <> name <> ">"

mconcatMap :: (Monoid.Monoid b) => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f

newtype Html = Html Text
    deriving (Monoid.Monoid, String.IsString, Show)

un_html :: Html -> Text
un_html (Html text) = text

html :: Text -> Html
html = Html . html_quote

html_quote :: Text -> Text
html_quote = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"
    . Text.replace "&" "&amp;"

html_doc :: HtmlState -> Text -> Html
html_doc (haddock_dir, files) = Html . postproc . html_quote
    where
    -- To keep the Text vs. Html type distinction I'd have to have [Either Text
    -- Html] and make mapDelimited return a list, and I couldn't use
    -- Text.replace.  It's doable, but would be more trouble than it's worth.
    postproc = para . backticks . single_quotes
    para = Text.replace "\n" "\n<br>"
    backticks = TextUtil.mapDelimited True "`" "`"
        (\t -> "<code>" <> t <> "</code>")
    single_quotes = TextUtil.mapDelimited False "'" "'" $ \text ->
        case TextUtil.haddockUrl files haddock_dir text of
            Nothing -> "'" <> text <> "'"
            Just url -> html_link text url

html_link :: Text -> String -> Text
html_link text url =
    "<a href=\"" <> html_quote (txt url) <> "\">" <> html_quote text <> "</a>"


-- * scale doc

type Scale = [CallBindings]

scales_html :: HtmlState -> [Scale] -> Text
scales_html hstate scales = un_html $ html_header
        <> "<h2> Scales </h2>\n"
        <> "<dl class=main>\n" <> mconcatMap scale_html scales
        <> "</dl>\n"
    where scale_html = mconcatMap (call_bindings_html hstate "scale")

scale_doc :: Scale.Scale -> Scale
scale_doc scale =
    lookup_docs [Derive.lookup_docs $ Derive.scale_to_lookup scale]

-- * doc

-- | Document is an intermediate format between Derive.Scope and the eventual
-- textual output.
type Document = [Section]

-- | Emit docs for all calls in the default scope.
builtin :: (Cmd.M m) => m Document
builtin = all_sections <$> Cmd.gets (Cmd.state_global_scope . Cmd.state_config)

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

-- | (call kind, docs)
--
-- Call kind is note, control, pitch, val.  CallType is val, generator,
-- transformer.
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
scope_doc (Derive.ScopeType override inst scale builtin) =
    filter (not . null . snd)
    [ ("override", lookup_docs (map Derive.lookup_docs override))
    , ("instrument", lookup_docs (map Derive.lookup_docs inst))
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
        (shadowed, ((False, "lookup: " <> pattern),
            documented_call call))
    go shadowed (Right sym, call) = (Set.insert sym shadowed,
        ((sym `Set.member` shadowed, show_sym sym), documented_call call))
    show_sym (TrackLang.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
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
    (name, doc GeneratorCall generator ++ doc TransformerCall transformer)
    where doc typ = maybe [] ((:[]) . (,) typ)
documented_call (Derive.DocumentedValCall name cdoc) = (name, [(ValCall, cdoc)])
