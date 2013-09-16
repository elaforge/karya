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
call_bindings_text (binds, ctype, call_doc) = do
        mapM_ show_bind binds
        Format.indented 2 $ show_call_doc call_doc
        Format.newline
    where
    show_bind (shadowed, sym, name) = do
        Format.write $ if shadowed then strikeout sym else sym
        Format.write $ " -- " <> name <> ": (" <> show_call_type ctype <> ")\n"
    strikeout sym = "~~" <> sym <> "~~ (shadowed)"
    show_call_doc (Derive.CallDoc tags doc args) = do
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
doc_html hstate = un_html . (html_header hstate <>) . mconcatMap section
    where
    section (call_kind, scope_docs) =
        tag "h2" (html call_kind) <> "\n\n"
        <> mconcatMap (scope_doc call_kind) scope_docs
    scope_doc call_kind (source, calls) =
        tag "h3" ("from " <> html source) <> "\n\n<dl class=main>\n"
        <> mconcatMap (call_bindings_html hstate call_kind) calls
        <> "</dl>\n"

html_header :: HtmlState -> Html
html_header hstate =
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
        \exclude: <input id=input type=text size=60 value=\"" <> default_search
        <> "\" onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...)\n\
        \<br>" <> html_doc hstate
            "Common tags are documneted at 'Derive.Call.Tags'.\n"
    where default_search = "-internal -ly-only"

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
    \};\n\
    \window.onload = function() {\n\
    \   search(document.getElementById('input').value);\n\
    \};\n"

call_bindings_html :: HtmlState -> Text -> CallBindings -> Html
call_bindings_html hstate call_kind bindings@(binds, ctype, call_doc) =
    "<div tags=\"" <> html (Text.unwords tags) <> "\">"
    <> mconcatMap show_bind (zip (True : repeat False) binds)
        <> show_call_doc call_doc
    <> "</div>\n\n"
    where
    tags = call_kind : binding_tags bindings
    show_bind (first, (shadowed, sym, name)) =
        "<dt>" <> (if shadowed then strikeout sym else tag "code" (html sym))
        <> " &mdash; " <> tag "b" (html name) <> ": "
        <> (if first then show_ctype else "") <> "\n"
    show_ctype = "<div style='float:right'>"
        <> tag "em" (html (show_call_type ctype)) <> "</div>"
    strikeout sym = tag "strike" (tag "code" (html sym))
        <> tag "em" "(shadowed)"
    show_call_doc (Derive.CallDoc tags doc args) =
        "<dd> <dl class=compact>\n"
        <> html_doc hstate doc <> write_tags tags <> "\n"
        <> tag "ul" (arg_docs args)
        <> "</dl>\n"
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "\n<li><b>Args parsed by call:</b> " <> html_doc hstate doc
    arg_docs (Derive.ArgDocs args) = mconcatMap arg_doc args
    arg_doc (Derive.ArgDoc name typ parser doc) =
        "<li>" <> tag "code" (html name) <> show_char char
        <> " :: " <> tag "em" (html (txt (Pretty.pretty typ)))
        <> show_default deflt
        <> (if Text.null doc then "" else " &mdash; " <> html_doc hstate doc)
        <> "\n"
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
binding_tags (binds, ctype, call_doc) =
    Seq.unique (show_call_type ctype : extract call_doc)
    where
    names = [name | (_, _, name) <- binds]
    extract call_doc =
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

scales_html :: HtmlState -> [CallBindings] -> Text
scales_html hstate scales = un_html $ html_header hstate
        <> "<h2> Scales </h2>\n"
        <> "<dl class=main>\n" <> scale_html scales
        <> "</dl>\n"
    where scale_html = mconcatMap (call_bindings_html hstate "scale")

scale_docs :: [Scale.Scale] -> [CallBindings]
scale_docs = sort_calls . lookup_docs ValCall
    . map (Derive.lookup_docs . Derive.scale_to_lookup)

-- * doc

-- | Document is an intermediate format between Derive.Scope and the eventual
-- textual output.
type Document = [Section]

-- | Emit docs for all calls in the default scope.
builtin :: (Cmd.M m) => m Document
builtin = all_sections <$> Cmd.gets (Cmd.state_global_scopes . Cmd.state_config)

all_sections :: Derive.Scopes -> [Section]
all_sections (Derive.Scopes (Derive.Scope gnote gcontrol gpitch)
        (Derive.Scope tnote tcontrol tpitch) val) =
    [ ("note", merged_scope_docs gnote tnote)
    , ("control", merged_scope_docs gcontrol tcontrol)
    , ("pitch", merged_scope_docs gpitch tpitch)
    , ("val", scope_doc ValCall val)
    ]

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrument_calls :: Derive.InstrumentCalls -> [ScopeDoc]
instrument_calls (Derive.InstrumentCalls gs ts vals) =
    [ ("note", lookup_calls GeneratorCall gs ++ lookup_calls TransformerCall ts)
    , ("val", lookup_calls ValCall vals)
    ]

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: (Cmd.M m) => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require_msg "dynamic for doc"
        =<< Perf.lookup_dynamic block_id (Just track_id)
    ttype <- TrackInfo.track_type <$> State.get_track_title track_id
    return $ track_sections ttype (Derive.state_scopes dynamic)

-- | (call kind, docs)
--
-- Call kind is note, control, pitch, val.  CallType is val, generator,
-- transformer.
type Section = (Text, [ScopeDoc])

track_sections :: TrackInfo.Type -> Derive.Scopes -> [Section]
track_sections ttype (Derive.Scopes (Derive.Scope gnote gcontrol gpitch)
        (Derive.Scope tnote tcontrol tpitch) val) =
    (\d -> [d, ("val", scope_doc ValCall val)]) $ case ttype of
        TrackInfo.NoteTrack -> ("note", merged_scope_docs gnote tnote)
        TrackInfo.ControlTrack ->
            ("control", merged_scope_docs gcontrol tcontrol)
        TrackInfo.TempoTrack -> ("tempo", merged_scope_docs gcontrol tcontrol)
        TrackInfo.PitchTrack -> ("pitch", merged_scope_docs gpitch tpitch)

-- | Documentation for one type of scope: (scope_source, calls)
type ScopeDoc = (Text, [CallBindings])

-- | Create docs for generator and transformer calls, and merge and sort them.
merged_scope_docs :: Derive.ScopeType (Derive.Generator d)
    -> Derive.ScopeType (Derive.Transformer d) -> [ScopeDoc]
merged_scope_docs generator transformer = merge_scope_docs $
    scope_doc GeneratorCall generator ++ scope_doc TransformerCall transformer

merge_scope_docs :: [ScopeDoc] -> [ScopeDoc]
merge_scope_docs = map (second (sort_calls . concat)) . Seq.group_fst

sort_calls :: [CallBindings] -> [CallBindings]
sort_calls = Seq.sort_on $ \(binds, _, _) ->
    (\(_, sym, _) -> sym) <$> Seq.head binds

-- | Walk up the scopes, keeping track of shadowed names.
scope_doc :: CallType -> Derive.ScopeType call -> [ScopeDoc]
scope_doc ctype (Derive.ScopeType override inst scale builtin) =
    filter (not . null . snd)
    [ ("override", lookup_calls ctype override)
    , ("instrument", lookup_calls ctype inst)
    , ("scale", lookup_calls ctype scale)
    , ("builtin", lookup_calls ctype builtin)
    ]

-- | Multiple bound symbols with the same DocumentedCall are grouped together:
type CallBindings = ([Binding], CallType, Derive.CallDoc)
-- | (is_shadowed, bound_symbol, call_name, call_type)
type Binding = (Bool, SymbolName, CallName)
type CallName = Text
type SymbolName = Text

lookup_calls :: CallType -> [Derive.LookupCall call] -> [CallBindings]
lookup_calls ctype scope = lookup_docs ctype (map Derive.lookup_docs scope)

lookup_docs :: CallType -> [Derive.LookupDocs] -> [CallBindings]
lookup_docs ctype =
    group . map extract . snd . List.mapAccumL go Set.empty . concatMap flatten
    where
    flatten (Derive.LookupPattern pattern call) = [(Left pattern, call)]
    flatten (Derive.LookupMap cmap) =
        [(Right sym, call) | (sym, call) <- Map.toAscList cmap]
    go shadowed (Left pattern, call) =
        -- There's no way to know if a programmatic lookup shadows.
        (shadowed, ((False, "lookup: " <> pattern), call))
    go shadowed (Right sym, call) =
        (Set.insert sym shadowed,
            ((sym `Set.member` shadowed, show_sym sym), call))
    show_sym (TrackLang.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
    extract ((shadowed, sym), Derive.DocumentedCall name doc) =
        ((shadowed, sym, name), doc)
    -- Group calls with the same CallDoc.
    group docs =
        [(map fst group, ctype, doc)
            | (doc, group) <- Seq.keyed_group_on snd docs]

data CallType = ValCall | GeneratorCall | TransformerCall
    deriving (Eq, Ord, Show)

show_call_type :: CallType -> Text
show_call_type ValCall = "val"
show_call_type GeneratorCall = "generator"
show_call_type TransformerCall = "transformer"
