-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pull deriver call documentation out of a Performance and format it nicely.
module Cmd.CallDoc where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import qualified Util.File as File
import qualified Util.Format as Format
import Util.Format ((<+>), (</>), (<//>), (<+/>))
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import Global
import Types


-- * output

-- | Convert a Document to plain text.
doc_text :: Document -> Lazy.Text
doc_text = Format.render "  " 75 . mconcatMap section
    where
    section (call_kind, scope_docs) =
        "##" <+> Format.text call_kind <+> "calls" <> "\n\n"
        <> mconcatMap scope_doc scope_docs
    scope_doc (source, calls) = "### from" <+> Format.text source <> "\n\n"
        <> Format.unlines (map (call_bindings_text True) calls)

call_bindings_text :: Bool -> CallBindings -> Format.Doc
call_bindings_text show_module (binds, ctype, call_doc) =
    (if show_module
        then (module_text (Derive.cdoc_module call_doc) <//>)
        else id) $
    Format.unlines (map show_bind binds)
    </> show_call_doc call_doc
    where
    module_text (Module.Module m) = Format.text $ "\tModule: " <> m
    show_bind (sym, name) = Format.text $
        sym <> " -- " <> name <> ": (" <> show_call_type ctype <> ")"
    show_call_doc (Derive.CallDoc _module tags doc args)
        | tags == mempty && no_args args = write_doc doc
        | otherwise = write_doc doc <//> write_tags tags
            <> Format.indentLine (arg_docs args)
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "Args parsed by call:" <+/> write_doc doc <> "\n"
    arg_docs (Derive.ArgDocs args) = Format.unlines (map arg_doc args)
    arg_doc (Derive.ArgDoc name typ parser env_default doc) =
        Format.text (name <> super <> " :: " <> pretty typ
            <> maybe "" (" = "<>) deflt
            <> " " <> environ_keys name env_default
            <> " --")
        <+/> write_doc doc
        where
        (super_, deflt) = show_parser parser
        super = maybe "" (\t -> if Text.length t == 1 then t else " " <> t)
            super_
    write_tags tags
        -- Otherwise there's nothing separating the call doc and args doc.
        | tags == mempty = "Args:"
        | otherwise = Format.text $ "Tags: "
            <> Text.intercalate ", " (Tags.untag tags)
    no_args (Derive.ArgDocs []) = True
    no_args _ = False

environ_keys :: Text -> Sig.EnvironDefault -> Text
environ_keys name deflt =
    "[" <> Text.intercalate ", " (map unsym (Sig.environ_keys "*" name deflt))
        <> "]"
    where unsym (BaseTypes.Symbol sym) = sym

write_doc :: Text -> Format.Doc
write_doc = Format.indent . Format.wrapWords . map Format.text . Text.words

show_parser :: Derive.ArgParser -> (Maybe Text, Maybe Text)
    -- ^ (superscript to mark this arg, default value)
show_parser p = case p of
    Derive.Required -> (Nothing, Nothing)
    Derive.Defaulted deflt -> (Nothing, Just deflt)
    Derive.Optional deflt -> (Just "?", Just deflt)
    Derive.Many -> (Just "*", Nothing)
    Derive.Many1 -> (Just "+", Nothing)
    Derive.Environ deflt -> (Just "env", deflt)

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
        tag_class "div" "call-kind" $ tag "h2" (html call_kind)
            <> "\n\n" <> mconcatMap (scope_doc call_kind) scope_docs
    scope_doc call_kind (source, calls)
        -- 'builtin_scope_doc' does this since Library doesn't have source
        -- types.
        | Text.null source = doc
        | otherwise =
            tag_class "div" "call-source" $ tag "h3" ("from " <> html source)
                <> "\n\n" <> doc
        where
        doc = "<dl class=main-dl>\n"
            <> mconcatMap (show_module_group call_kind)
                (Seq.keyed_group_sort module_of calls)
            <> "</dl>\n"
    module_of (_, _, call_doc) = Derive.cdoc_module call_doc
    show_module_group call_kind (module_, calls) =
        tag_class "div" "call-module" $
            show_module module_ (length calls) <> "<br>\n"
            <> mconcatMap (call_bindings_html hstate call_kind) calls
    show_module (Module.Module m) calls = tag "center" $
        tag "b" "Module: " <> tag "code" (html m)
            <> " (" <> html (showt calls) <> " calls)"

html_header :: HtmlState -> Html
html_header hstate = mconcat
    [ "<meta charset=utf-8>\n"
    , "<style type=text/css>\n" <> css <> "</style>\n"
    , "<script>\n" <> javascript <> "\n</script>\n"
    , Seq.join "; "
        [ "<code>arg<sup>?</sup></code> &mdash; optional arg"
        , "<code>arg<sup>*</sup></code> &mdash; zero or more args"
        , "<code>arg<sup>+</sup></code> &mdash; one or more args"
        , "<code>arg<sup>env</sup></code> &mdash; looked up in the environ"
        , "<code>arg = val</code> &mdash; arg with default"
        , ":: <em>type</em> &mdash; argument type"
        , "<code>[*-arg arg]</code> &mdash; default from environ values\
            \ <code>name-arg</code> followed by <code>arg</code>"
        ]
    , "<p> <code>word</code> to include a tag containing the word,\
        \ <code>-word</code> to\n\
        \exclude.  Prefix with <code>m:</code> for modules,\
        \ <code>kind:(note|control|pitch|val)</code> for call kinds, or\
        \ <code>type:(val|generator|transformer></code> for call types.\n\
        \<input id=input type=text size=60 value=\"" <> default_search
        <> "\" onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...).\
        \\n<br>Search for calls with the browser's text search, \"call --\"\
        \ to search by binding, \"-- call\" to search by name.\n<br>"
    , html_doc hstate "Common tags are documented at 'Derive.Call.Tags'."
    , " &mdash; <span id=totals> x </span>\n"
    ]
    where default_search = "-m:internal -m:ly "

css :: Html
css = Seq.join "\n"
    [ ".main-dl dl { border-bottom: 1px solid #999 }"
    , "dl.compact {"
    , "    margin: 0px;"
    , "    padding: 0;"
    , "}"
    , "ul { margin: 0; }"
    , "div { margin-bottom: 10px; }"
    , ".compact dt {"
    , "    margin: 0;"
    , "    padding: 0;"
    , "}"
    , ".compact dd {"
    , "    margin: 0 0 1em 0;"
    , "    padding: 0;"
    , "}"
    ]

javascript :: Html
javascript = Seq.join "\n"
    [ search_javascript
    , ""
    , hide_empty_javascript
    , ""
    , "window.onload = function() {"
    , "    var input = document.getElementById('input');"
    , "    input.focus();"
    , "    input.setSelectionRange(999, 999);"
    , "    search(input.value);"
    , "};"
    ]

search_javascript :: Html
search_javascript = Seq.join "\n"
    [ "var total_calls = 0;"
    , "var displayed_calls = 0;"
    , "var search = function(val) {"
    , "    var search_words = val.split(/ +/).filter("
    , "        function(x) { return x != '' });"
    , "    total_calls = 0;"
    , "    displayed_calls = 0;"
    , "    var calls = document.getElementsByClassName('call');"
    , "    for (var i = 0; i < calls.length; i++) {"
    , "        var c = calls[i];"
    , "        var tags = c.attributes.tags.value.split(' ');"
    , "        c.hidden = !matches(search_words, tags);"
    , "        total_calls++;"
    , "        if (!c.hidden) displayed_calls++;"
    , "    }"
    , "    hide_all_empty();"
    , "    document.getElementById('totals').innerText ="
    , "        'calls displayed/total: ' + displayed_calls + '/' + total_calls;"
    , "};"
    , ""
    , "var matches = function(search_words, tags) {"
    , "    tags = tags.filter(function(x) { return x != '' });"
    , "    return search_words.every(function(x) {"
    , "        if (x[0] === '-')"
    , "            return !tags_match(tags, x.slice(1));"
    , "        else"
    , "            return tags_match(tags, x);"
    , "     });"
    , "};"
    , ""
    , "var tags_match = function(tags, val) {"
    , "    return tags.some(function(t) { return t.indexOf(val) !== -1 });"
    , "};"
    ]

hide_empty_javascript :: Html
hide_empty_javascript = Seq.join "\n"
    [ "var hide_all_empty = function() {"
    , "    hide_if_empty('call-module');"
    , "    hide_if_empty('call-source');"
    , "    hide_if_empty('call-kind');"
    , "};"
    , ""
    , "var hide_if_empty = function(class_) {"
    , "    var elts = document.getElementsByClassName(class_);"
    , "    for (var i = 0; i < elts.length; i++) {"
    , "        elts[i].hidden = !any_shown_call(elts[i]);"
    , "    }"
    , "};"
    , "" -- True if this has any unhidden 'call' children.
    , "var any_shown_call = function(parent) {"
    , "    for (var i = 0; i < parent.children.length; i++) {"
    , "        var c = parent.children[i];"
    , "        if (c.className === 'call' && !c.hidden || any_shown_call(c))"
    , "            return true;"
    , "    }"
    , "    return false;"
    , "};"
    ]

call_bindings_html :: HtmlState -> Text -> CallBindings -> Html
call_bindings_html hstate call_kind bindings@(binds, ctype, call_doc) =
    "<div class=call tags=\"" <> html (Text.unwords tags) <> "\">"
    <> mconcatMap show_bind (zip (True : repeat False) binds)
        <> show_call_doc call_doc
    <> "</div>\n\n"
    where
    tags = "kind:" <> call_kind : binding_tags bindings
    show_bind (first, (sym, name)) =
        "<dt>" <> tag "code" (html sym)
        -- This used to be &mdash;, but that's too hard to use text search on.
        <> " -- " <> tag "b" (html name) <> ": "
        <> (if first then show_ctype else "") <> "\n"
    show_ctype = "<div style='float:right'>"
        <> tag "em" (html (show_call_type ctype)) <> "</div>"
    show_call_doc (Derive.CallDoc _module tags doc args) =
        "<dd> <dl class=compact>\n"
        <> html_doc hstate doc
        <> write_tags tags <> "\n"
        <> tag "ul" (arg_docs args)
        <> "</dl>\n"
    arg_docs (Derive.ArgsParsedSpecially doc) =
        "\n<li><b>Args parsed by call:</b> " <> html_doc hstate doc
    arg_docs (Derive.ArgDocs args) = mconcatMap arg_doc args
    arg_doc (Derive.ArgDoc name typ parser env_default doc) =
        "<li" <> li_type <> ">" <> tag "code" (html name) <> show_char char
        <> " :: " <> tag "em" (html (pretty typ))
        <> show_default deflt
        <> " " <> tag "code" (html (environ_keys name env_default))
        <> (if Text.null doc then "" else " &mdash; " <> html_doc hstate doc)
        <> "\n"
        where
        (char, deflt) = show_parser parser
        li_type = if Maybe.isNothing deflt then ""
            else " style=list-style-type:circle"
    show_default = maybe "" ((" = " <>) . tag "code" . html)
    show_char = maybe "" (tag "sup" . html)
    write_tags tags
        | tags == mempty = ""
        | otherwise = "<br><b>Tags:</b> "
            <> tag "em" (html (Text.intercalate ", " (Tags.untag tags)))

-- | Extract explicit tags as well as some implicit tags.  Implicit tags are
-- @%control@ for controls in the default arguments, @name-arg@ for environ
-- keys that default the arguments, and @note@, @control@, @pitch@, or @val@ for
-- the call kind.
binding_tags :: CallBindings -> [Text]
binding_tags (binds, ctype, call_doc) =
    Seq.unique $ "type:" <> show_call_type ctype : extract call_doc
    where
    names = map snd binds
    extract call_doc = module_ (Derive.cdoc_module call_doc)
        : cdoc_tags call_doc ++ args_tags (Derive.cdoc_args call_doc)
    cdoc_tags = Tags.untag . Derive.cdoc_tags
    module_ (Module.Module m) = "m:" <> m
    args_tags (Derive.ArgDocs args) = concatMap arg_tags args
    args_tags (Derive.ArgsParsedSpecially {}) = []
    arg_tags arg =
        [ unsym $ Sig.prefixed_environ name (Derive.arg_name arg)
        | name <- names
        ] ++ arg_control_tags (Derive.arg_parser arg)
    unsym (BaseTypes.Symbol sym) = sym
    -- An arg with a control signal default should look like "%sig,.5".
    -- This is a hack, since the default isn't stored in a structured way.
    arg_control_tags (Derive.Defaulted deflt)
        | "%" `Text.isPrefixOf` deflt = [Text.takeWhile (/=',') deflt]
        | otherwise = []
    arg_control_tags _ = []

tag :: Text -> Html -> Html
tag name content = tag_attrs name [] (Just content)

tag_class :: Text -> Text -> Html -> Html
tag_class name class_ content =
    tag_attrs name [("class", class_)] (Just content)

html_link :: Text -> Text -> Html
html_link text url = tag_attrs "a" [("href", url)] (Just (html text))

tag_attrs :: Text -> [(Text, Text)] -> Maybe Html -> Html
tag_attrs name attrs maybe_content =
    "<" <> html name <> attrs_text <> ">" <> content_text
    where
    content_text = case maybe_content of
        Nothing -> ""
        Just content -> content <> "</" <> html name <> ">"
    attrs_text
        | null attrs = ""
        | otherwise = (" "<>) $ Seq.join " "
            [html name <> "=\"" <> html val <> "\"" | (name, val) <- attrs]

newtype Html = Html Text
    deriving (Monoid, String.IsString, Show)
    -- TODO doesn't IsString defeat the purpose of using Html in the first
    -- place?

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
    backticks = TextUtil.mapDelimited True '`'
        (\t -> "<code>" <> t <> "</code>")
    single_quotes = TextUtil.mapDelimited False '\'' $ \text ->
        case TextUtil.haddockUrl files haddock_dir text of
            Nothing -> "'" <> text <> "'"
            Just url -> un_html $ html_link text (txt url)


-- * scale doc

type Scale = [CallBindings]

scales_html :: HtmlState -> [CallBindings] -> Text
scales_html hstate scales = un_html $ html_header hstate
        <> "<h2>Scales</h2>\n"
        <> "<dl class=main-dl>\n" <> scale_html scales <> "</dl>\n"
    where scale_html = mconcatMap (call_bindings_html hstate "scale")

-- | Extract documentation from scales.
scale_docs :: [(Pitch.ScaleId, Text, Derive.DocumentedCall)] -> [CallBindings]
scale_docs = sort_calls . lookup_calls ValCall . map convert
    where
    convert (scale_id, pattern, doc) =
        Derive.LookupPattern name doc (const (return Nothing))
        where name = pretty scale_id <> ": " <> pattern

-- * doc

-- | Document is an intermediate format between Derive.Scope and the eventual
-- textual output.
type Document = [Section]

-- | Keep only CallDocs whose name or binding name matches the function.
-- TODO strip empty sections afterwards
filter_calls :: (Text -> Bool) -> Document -> Document
filter_calls matches = map (second (map scope_doc))
    where
    scope_doc = second (filter call_bindings)
    call_bindings (bindings, _, _) = any binding bindings
    binding (sym, call) = matches sym || matches call

-- | Emit docs for all calls in the default scope.
builtin :: Cmd.M m => m Document
builtin = library <$> Cmd.gets (Cmd.config_library . Cmd.state_config)

-- | Extract all the documentation from a Library.  Document extraction is
-- a big mess of walking over nested data and converting it to a parallel
-- nested data structure.  It's tedious, but the types make it hard to get
-- wrong.
library :: Derive.Library -> [Section]
library (Derive.Library note control pitch val) =
    [ ("note", call_maps note)
    , ("control", call_maps control)
    , ("pitch", call_maps pitch)
    , ("val", builtin_scope_doc ValCall (map convert_val_call val))
    ]

-- | A 'Derive.LookupCall' with the call stripped out and replaced with
-- just documentation.  This is so 'Derive.Call's and 'Derive.ValCall's can
-- be treated uniformly.
type LookupCall = Derive.LookupCall Derive.DocumentedCall

-- | Strip out the actual code part of the call, and make it just a lookup for
-- DocumentedCall.
convert_call :: Derive.LookupCall (Derive.Call f) -> LookupCall
convert_call = fmap_lookup Derive.extract_doc

convert_val_call :: Derive.LookupCall Derive.ValCall -> LookupCall
convert_val_call = fmap_lookup Derive.extract_val_doc

fmap_lookup :: (a -> b) -> Derive.LookupCall a -> Derive.LookupCall b
fmap_lookup extract_doc (Derive.LookupMap calls) = Derive.LookupMap $
    Map.map extract_doc calls
fmap_lookup _ (Derive.LookupPattern pattern doc _) =
    Derive.LookupPattern pattern doc (const (return Nothing))

-- | Create docs for generator and transformer calls, and merge and sort them.
call_maps :: Derive.CallMaps d -> [ScopeDoc]
call_maps (Derive.CallMaps generator transformer) = merge_scope_docs $
    builtin_scope_doc GeneratorCall (convert generator)
    ++ builtin_scope_doc TransformerCall (convert transformer)
    where convert = map convert_call

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrument_calls :: Derive.InstrumentCalls -> [ScopeDoc]
instrument_calls (Derive.InstrumentCalls gs ts vals) =
    [ ("note", lookup_calls GeneratorCall (map convert_call gs)
        ++ lookup_calls TransformerCall (map convert_call ts))
    , ("val", lookup_calls ValCall (map convert_val_call vals))
    ]

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: Cmd.M m => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require "dynamic for doc"
        =<< Perf.lookup_root_dynamic (block_id, Just track_id)
    ttype <- ParseTitle.track_type <$> State.get_track_title track_id
    return $ track_sections ttype (Derive.state_scopes dynamic)

-- | (call kind, docs)
--
-- Call kind is note, control, pitch, val.  CallType is val, generator,
-- transformer.
type Section = (Text, [ScopeDoc])

-- | This is an alternate doc extraction path which extracts the docs from
-- 'Derive.Scopes' instead of 'Derive.Library'.
track_sections :: ParseTitle.Type -> Derive.Scopes -> [Section]
track_sections ttype (Derive.Scopes (Derive.Scope gnote gcontrol gpitch)
        (Derive.Scope tnote tcontrol tpitch) val_) =
    (\d -> [d, ("val", scope_type ValCall val)]) $ case ttype of
        ParseTitle.NoteTrack ->
            ("note", merged_scope_docs (convert gnote) (convert tnote))
        ParseTitle.ControlTrack ->
            ("control", merged_scope_docs (convert gcontrol) (convert tcontrol))
        ParseTitle.TempoTrack ->
            ("tempo", merged_scope_docs (convert gcontrol) (convert tcontrol))
        ParseTitle.PitchTrack ->
            ("pitch", merged_scope_docs (convert gpitch) (convert tpitch))
    where
    val = convert_scope convert_val_call val_
    convert = convert_scope convert_call

convert_scope :: (Derive.LookupCall call -> LookupCall)
    -> Derive.ScopeType call -> Derive.ScopeType Derive.DocumentedCall
convert_scope convert (Derive.ScopeType override inst scale builtin) =
    Derive.ScopeType (map convert override) (map convert inst)
        (map convert scale) (map convert builtin)

-- | Documentation for one type of scope: (scope_source, calls)
type ScopeDoc = (Text, [CallBindings])

-- | Create docs for generator and transformer calls, and merge and sort them.
merged_scope_docs :: Derive.ScopeType Derive.DocumentedCall
    -> Derive.ScopeType Derive.DocumentedCall -> [ScopeDoc]
merged_scope_docs generator transformer =
    merge_scope_docs $ scope_type GeneratorCall generator
        ++ scope_type TransformerCall transformer

merge_scope_docs :: [ScopeDoc] -> [ScopeDoc]
merge_scope_docs = map (second (sort_calls . concat)) . Seq.group_fst

sort_calls :: [CallBindings] -> [CallBindings]
sort_calls = Seq.sort_on $ \(binds, _, _) ->
    Text.toLower . fst <$> Seq.head binds

-- | A 'Derive.Library' only has builtins, but ScopeDoc wants a source so
-- it can work uniformly with 'track_sections', which does have separate
-- sources.
builtin_scope_doc :: CallType -> [LookupCall] -> [ScopeDoc]
builtin_scope_doc ctype lookups = [("", lookup_calls ctype lookups)]

scope_type :: CallType -> Derive.ScopeType Derive.DocumentedCall -> [ScopeDoc]
scope_type ctype (Derive.ScopeType override inst scale builtin) =
    filter (not . null . snd)
    [ ("override", lookup_calls ctype override)
    , ("instrument", lookup_calls ctype inst)
    , ("scale", lookup_calls ctype scale)
    , ("builtin", lookup_calls ctype builtin)
    ]

-- | Multiple bound symbols with the same DocumentedCall are grouped together:
type CallBindings = ([Binding], CallType, Derive.CallDoc)
type Binding = (SymbolName, CallName)
type CallName = Text
type SymbolName = Text

lookup_calls :: CallType -> [LookupCall] -> [CallBindings]
lookup_calls ctype = group . map (extract . go) . concatMap flatten
    where
    flatten (Derive.LookupPattern pattern doc _) = [(Left pattern, doc)]
    flatten (Derive.LookupMap cmap) =
        [(Right sym, call) | (sym, call) <- Map.toAscList cmap]
    go (Left pattern, call) = ("lookup: " <> pattern, call)
    go (Right sym, call) = (show_sym sym, call)
    show_sym (BaseTypes.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
    extract (sym, Derive.DocumentedCall name doc) = ((sym, name), doc)
    -- Group calls with the same CallDoc.
    group docs = [(map fst group, ctype, doc)
        | (doc, group) <- Seq.keyed_group_sort snd docs]

data CallType = ValCall | GeneratorCall | TransformerCall
    deriving (Eq, Ord, Show)

show_call_type :: CallType -> Text
show_call_type ValCall = "val"
show_call_type GeneratorCall = "generator"
show_call_type TransformerCall = "transformer"
