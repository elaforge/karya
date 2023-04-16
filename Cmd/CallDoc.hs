-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pull deriver call documentation out of a Performance and format it nicely.
module Cmd.CallDoc where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import qualified Util.Doc as Doc
import qualified Util.Format as Format
import           Util.Format ((<+/>), (<+>), (<//>), (</>))
import qualified Util.Html as Html
import           Util.Html (html, tag)
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * output

-- | Convert a Document to plain text.
doc_text :: Document -> Lazy.Text
doc_text = Format.render "  " 75 . mconcatMap section
    where
    section (call_kind, scope_docs) =
        "##" <+> Format.text call_kind <+> "calls" <> "\n\n"
        <> mconcatMap scope_doc scope_docs
    scope_doc (source, calls) = "### from" <+> Format.text (pretty source)
        <> "\n\n" <> Format.unlines (map (call_bindings_text True) calls)

call_bindings_text :: Bool -> CallBindings -> Format.Doc
call_bindings_text include_module (binds, ctype, call_doc) =
    Format.unlines (map show_bind binds)
    </> show_call_doc call_doc
    <> "\n\n"
    where
    show_bind (sym, Derive.CallName name) =
        Format.text sym <+> "--" <+> Format.text name
        <+> (if include_module
            then Format.text (show_module (Derive.cdoc_module call_doc))
            else mempty)
        <+> ("(" <> Format.text (pretty ctype) <> ")")
    show_call_doc (Derive.CallDoc _module tags doc args)
        | tags == mempty && null args = write_doc doc
        | otherwise = write_doc doc <//> write_tags tags
            <> Format.indentLine (arg_docs args)
    arg_docs = Format.unlines . map arg_doc
    arg_doc (Derive.ArgDoc name typ parser env_default doc) =
        Format.text (unname name <> super <> " :: " <> pretty typ
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
    unname (Derive.ArgName s) = s

environ_keys :: Derive.ArgName -> Sig.EnvironDefault -> Text
environ_keys name deflt =
    "[" <> Text.intercalate ", " (Sig.environ_keys "*" name deflt) <> "]"

write_doc :: Doc.Doc -> Format.Doc
write_doc =
    Format.indent . Format.wrapWords . map Format.text . Text.words . undoc
    where undoc (Doc.Doc s) = s

show_module :: Module.Module -> Text
show_module (Module.Module name) = "[" <> name <> "]"

show_parser :: Derive.ArgParser -> (Maybe Text, Maybe Text)
    -- ^ (superscript to mark this arg, default value)
show_parser p = case p of
    Derive.Required -> (Nothing, Nothing)
    Derive.Defaulted deflt -> (Nothing, Just deflt)
    Derive.Optional deflt -> (Just "?", Just deflt)
    Derive.Many -> (Just "*", Nothing)
    Derive.Many1 -> (Just "+", Nothing)
    Derive.Environ deflt -> (Just "env", deflt)

{- | Print an abbreviated list of calls, grouped by namespace and ordered by
    shadowing priority.  Should look like:

    >     note generator
    > n -- note (instrument) [inst] When the event has zero duration, dispatc...
    > n -- note (imported) [prelude] The note call is the main note generator...
    >     note transformer
    > n -- note-attributes (imported) [prelude] This is similar to to `=`, bu...
-}
bindings_text :: Document -> Text
bindings_text =
    Text.unlines . filter (not . Text.null) . concatMap section . flatten
    where
    section ((call_kind, call_type), bindings)
        | null bindings = []
        | otherwise = "\t" <> call_kind <> " " <> pretty call_type
            : map binding bindings
    binding (scope_source,
            ((symbol_name, Derive.CallName call_name), call_doc)) =
        doc <> " " <> Texts.ellipsis (width - Text.length doc - 1)
            (undoc (Derive.cdoc_doc call_doc))
        where
        undoc (Doc.Doc d) = d
        doc = symbol_name <> " -- " <> call_name <> " (" <> pretty scope_source
            <> ") " <> show_module (Derive.cdoc_module call_doc)
    width = 76
    flatten :: Document
        -> [((CallKind, CallType), [(ScopeSource, (Binding, Derive.CallDoc))])]
    flatten sections = map (second (Lists.sortOn fst . concat)) $ Lists.groupFst
        [ ((call_kind, call_type),
            map (scope_source,) (map (, call_doc) bindings))
        | (call_kind, scope_docs) <- sections
        , (scope_source, call_bindings) <- scope_docs
        , (bindings, call_type, call_doc) <- call_bindings
        ]

-- ** html output

-- | Convert a Document to HTML.
doc_html :: Html.HtmlState -> Document -> Html.Html
doc_html hstate = (html_header hstate <>) . mconcatMap section
    where
    section (call_kind, scope_docs) =
        Html.tag_class "div" "call-kind" $ tag "h2" (html call_kind)
            <> "\n\n" <> mconcatMap (scope_doc call_kind) scope_docs
    scope_doc call_kind (maybe_source, call_bindings) = case maybe_source of
        -- 'imported_scope_doc' does this since Library doesn't have source
        -- types.
        Nothing -> doc
        Just source -> Html.tag_class "div" "call-source" $
            tag "h3" ("from " <> html (pretty source)) <> "\n\n" <> doc
        where
        doc = "<dl class=main-dl>\n"
            <> mconcatMap (show_module_group call_kind)
                (Lists.keyedGroupSort module_of call_bindings)
            <> "</dl>\n"
    module_of (_, _, call_doc) = Derive.cdoc_module call_doc
    show_module_group call_kind (module_, call_bindings) =
        Html.tag_class "div" "call-module" $
            show_module module_ (length call_bindings) <> "<br>\n"
            <> mconcatMap (call_bindings_html hstate call_kind) call_bindings
    show_module (Module.Module m) calls = tag "center" $
        tag "b" "Module: " <> tag "code" (html m)
            <> " (" <> html (showt calls) <> " calls)"

html_header :: Html.HtmlState -> Html.Html
html_header hstate = mconcat
    [ "<meta charset=utf-8>\n"
    , "<style type=text/css>\n" <> css <> "</style>\n"
    , "<script>\n" <> javascript <> "\n</script>\n"
    , Lists.join "; "
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
        \ <code>type:(val|generator|transformer)</code> for call types.\n\
        \<input id=input type=text size=60 value=\"" <> default_search
        <> "\" onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...).\
        \\n<br>Search for calls with the browser's text search, \"call --\"\
        \ to search by binding, \"-- call\" to search by name.\n<br>"
    , Html.html_doc hstate "Common tags are documented at 'Derive.Call.Tags'."
    , "\n<p> <span id=totals> x </span>\n"
    ]
    where default_search = "-m:internal -m:ly "

css :: Html.Html
css = Lists.join "\n"
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

javascript :: Html.Html
javascript = Lists.join "\n"
    [ search_javascript
    , ""
    , hide_empty_javascript
    , ""
    , "window.onload = function() {"
    , "    var input = document.getElementById('input');"
    -- Theoretically this is useful, but in practice I'm usually reloading to
    -- see doc changes, and so auto-focus is just an annoyance.
    -- , "    input.focus();"
    -- , "    input.setSelectionRange(999, 999);"
    , "    search(input.value);"
    , "};"
    ]

search_javascript :: Html.Html
search_javascript = Lists.join "\n"
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

hide_empty_javascript :: Html.Html
hide_empty_javascript = Lists.join "\n"
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

call_bindings_html :: Html.HtmlState -> Text -> CallBindings -> Html.Html
call_bindings_html hstate call_kind bindings@(binds, ctype, call_doc) = mconcat
    [ "<div class=call tags=\"" <> html (Text.unwords tags) <> "\">"
    , mconcatMap (show_bind (Derive.cdoc_module call_doc))
        (zip (True : repeat False) binds)
        <> show_call_doc call_doc
    , "</div>\n\n"
    ]
    where
    tags = "kind:" <> call_kind : binding_tags bindings
    show_bind module_ (first, (sym, Derive.CallName name)) =
        "<dt>" <> tag "code" (html sym)
        -- This used to be &mdash;, but that's too hard to use text search on.
        <> " -- " <> tag "b" (html name)
        <> (if first then show_ctype module_ else "") <> "\n"
    show_ctype module_ = mconcat
        [ "<div style='float:right'>"
        , tag "code" (html (pretty module_))
        , " : "
        , tag "em" (html (call_kind <> " " <> pretty ctype))
        , "</div>"
        ]
    show_call_doc (Derive.CallDoc _module tags doc args) =
        "<dd> <dl class=compact>\n"
        <> Html.html_doc hstate doc
        <> write_tags tags <> "\n"
        <> tag "ul" (mconcatMap arg_doc args)
        <> "</dl>\n"
    arg_doc (Derive.ArgDoc name typ parser env_default doc) =
        "<li" <> li_type <> ">" <> tag "code" (html (unname name))
        <> show_char char
        <> " :: " <> tag "em" (html (pretty typ))
        <> show_default deflt
        <> " " <> tag "code" (html (environ_keys name env_default))
        <> (if doc == "" then "" else " &mdash; " <> Html.html_doc hstate doc)
        <> "\n"
        where
        unname (Derive.ArgName s) = s
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
    Lists.unique $ "type:" <> pretty ctype : extract call_doc
    where
    names = map snd binds
    extract call_doc = module_ (Derive.cdoc_module call_doc)
        : cdoc_tags call_doc ++ concatMap arg_tags (Derive.cdoc_args call_doc)
    cdoc_tags = Tags.untag . Derive.cdoc_tags
    module_ (Module.Module m) = "m:" <> m
    arg_tags arg =
        [ Sig.prefixed_environ name (Derive.arg_name arg)
        | name <- names
        ] ++ arg_control_tags (Derive.arg_parser arg)
    -- An arg with a control signal default should look like "%sig,.5".
    -- This is a hack, since the default isn't stored in a structured way.
    arg_control_tags (Derive.Defaulted deflt)
        | "%" `Text.isPrefixOf` deflt = [Text.takeWhile (/=',') deflt]
        | otherwise = []
    arg_control_tags _ = []


-- * scale doc

type Scale = [CallBindings]

scales_html :: Html.HtmlState -> [CallBindings] -> Html.Html
scales_html hstate scales = html_header hstate
        <> "<h2>Scales</h2>\n"
        <> "<dl class=main-dl>\n" <> scale_html scales <> "</dl>\n"
    where scale_html = mconcatMap (call_bindings_html hstate "scale")

-- | Extract documentation from scales.
scale_docs :: [(Pitch.ScaleId, Text, Derive.DocumentedCall)] -> [CallBindings]
scale_docs = sort_calls . entries ValCall . map convert
    where
    convert (scale_id, pattern, doc) = Library.Pattern $
        Derive.PatternCall desc doc (const (return Nothing))
        where desc = pretty scale_id <> ": " <> pattern

-- * doc

-- | An intermediate format between 'Derive.Scopes' and the eventual textual
-- output.
type Document = [Section]

type Section = (CallKind, [ScopeDoc])

-- | From the fields of 'Derive.Scope' and 'Derive.Scopes': note, control,
-- pitch, or val.
type CallKind = Text

-- | Documentation for one type of scope.
type ScopeDoc = (ScopeSource, [CallBindings])

-- | Nothing is when the source is irrelevant, so don't put it in the docs.
type ScopeSource = Maybe Derive.CallPriority

-- | Multiple bound symbols with the same DocumentedCall are grouped together:
type CallBindings = ([Binding], CallType, Derive.CallDoc)
type Binding = (SymbolName, Derive.CallName)
-- | This is the name the call is bound to.
type SymbolName = Text

data CallType = ValCall | GeneratorCall | TransformerCall | TrackCall
    deriving (Eq, Ord, Show)

instance Pretty CallType where
    pretty ctype = case ctype of
        ValCall -> "val"
        GeneratorCall -> "generator"
        TransformerCall -> "transformer"
        TrackCall -> "track"


-- ** implementation

-- | Keep only CallDocs whose name or binding name matches the function.
filter_calls :: (SymbolName -> Derive.CallName -> Bool) -> Document -> Document
filter_calls matches = strip_empty . map (second (map scope_doc))
    where
    scope_doc = second (map call_bindings)
    call_bindings (bindings, call_type, call_doc) =
        (filter binding bindings, call_type, call_doc)
    binding (sym, call) = matches sym call

strip_empty :: Document -> Document
strip_empty = mapMaybe section
    where
    section (call_kind, scope_docs) = case mapMaybe scope_doc scope_docs of
        [] -> Nothing
        stripped -> Just (call_kind, stripped)
    scope_doc (scope_source, call_bindings) =
        case mapMaybe call_binding call_bindings of
            [] -> Nothing
            stripped -> Just (scope_source, stripped)
    call_binding b@(bindings, _, _)
        | null bindings = Nothing
        | otherwise = Just b

-- | Extract all the documentation from the Builtins.  Document extraction is
-- a big mess of walking over nested data and converting it to a parallel
-- nested data structure.  It's tedious, but the types make it hard to get
-- wrong.
builtins :: Derive.Builtins -> [Section]
builtins (Derive.Scopes
        (Derive.Scope ngen cgen pgen)
        (Derive.Scope ntrans ctrans ptrans)
        (Derive.Scope ntrack ctrack ptrack)
        val) =
    [ ("note", scope ngen ntrans ntrack)
    , ("control", scope cgen ctrans ctrack)
    , ("pitch", scope pgen ptrans ptrack)
    , ("val", convert_modules ValCall Derive.extract_val_doc val)
    ]

convert_modules :: CallType -> (call -> Derive.DocumentedCall)
    -> Map mod (Derive.CallMap call) -> [ScopeDoc]
convert_modules ctype extract_doc = imported_scope_doc ctype
    . concatMap (call_map_to_entries . call_map_doc extract_doc)
    . Map.elems

-- | Create docs for generator, transformer, and track calls, and merge and
-- sort them.
scope :: Map Module.Module (Derive.CallMap (Derive.Call gen))
    -> Map Module.Module (Derive.CallMap (Derive.Call trans))
    -> Map Module.Module (Derive.CallMap (Derive.TrackCall track))
    -> [ScopeDoc]
scope gen trans track = merge_scope_docs $ concat
    [ convert_modules GeneratorCall Derive.extract_doc gen
    , convert_modules TransformerCall Derive.extract_doc trans
    , convert_modules TrackCall Derive.extract_track_doc track
    ]

-- | A 'Library.Entry' with the call stripped out and replaced with
-- just documentation.  This is so 'Derive.Call's and 'Derive.ValCall's can
-- be treated uniformly.
type Entry = Library.Entry Derive.DocumentedCall
type CallMap = Derive.CallMap Derive.DocumentedCall

-- | Convert 'Library.Entry' to 'Entry' by stripping out the code part of the
-- call, and replacing it with DocumentedCall.
entry_doc :: (call -> Derive.DocumentedCall) -> Library.Entry call -> Entry
entry_doc extract_doc (Library.Single sym call) =
    Library.Single sym (extract_doc call)
entry_doc _ (Library.Pattern pattern) =
    Library.Pattern $ pattern { Derive.pat_function = const $ return Nothing }

call_map_doc :: (call -> Derive.DocumentedCall) -> Derive.CallMap call
    -> CallMap
call_map_doc extract_doc (Derive.CallMap calls patterns) = Derive.CallMap
    { call_map = extract_doc <$> calls
    , call_patterns =
        map (\p -> p { Derive.pat_function = const $ return Nothing }) patterns
    }

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrument_calls :: Derive.InstrumentCalls -> Document
instrument_calls (Derive.Scopes gen trans track vals) =
    [ ("note", [(Just Derive.PrioInstrument, concat
        [ ctype_entries GeneratorCall gen
        , ctype_entries TransformerCall trans
        , call_map_entries TrackCall Derive.extract_track_doc track
        ])])
    , ("val", [(Just Derive.PrioInstrument,
        call_map_entries ValCall Derive.extract_val_doc vals)])
    ]
    where
    ctype_entries ctype = call_map_entries ctype Derive.extract_doc
    call_map_entries ctype extract_doc =
        entries ctype . call_map_to_entries . call_map_doc extract_doc

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: Cmd.M m => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require "CallDoc.track: no root dynamic"
        =<< Perf.lookup_root_dynamic (block_id, Just track_id)
    ttype <- ParseTitle.track_type <$> Ui.get_track_title track_id
    return $ track_sections ttype (Derive.state_scopes dynamic)

-- | This is an alternate doc extraction path which extracts the docs from
-- 'Derive.Scopes' instead of 'Derive.Builtins'.
track_sections :: ParseTitle.Type -> Derive.Scopes -> [Section]
track_sections ttype (Derive.Scopes
        (Derive.Scope gen_n gen_c gen_p)
        (Derive.Scope trans_n trans_c trans_p)
        (Derive.Scope track_n track_c track_p)
        val) =
    (\d -> [d, ("val", val_doc)]) $ case ttype of
        ParseTitle.NoteTrack -> ("note", merge3 gen_n trans_n track_n)
        ParseTitle.ControlTrack -> ("control", merge3 gen_c trans_c track_c)
        ParseTitle.TempoTrack -> ("tempo", merge3 gen_c trans_c track_c)
        ParseTitle.PitchTrack -> ("pitch", merge3 gen_p trans_p track_p)
    where
    merge3 gen trans track = merged_scope_docs
        [ (GeneratorCall, convert gen)
        , (TransformerCall, convert trans)
        , (TrackCall,
            convert_scope (call_map_doc Derive.extract_track_doc) track)
        ]
    val_doc = scope_type ValCall $
        convert_scope (call_map_doc Derive.extract_val_doc) val
    convert = convert_scope (call_map_doc Derive.extract_doc)

convert_scope :: (Derive.CallMap call -> CallMap)
    -> Derive.ScopePriority call -> Derive.ScopePriority Derive.DocumentedCall
convert_scope convert (Derive.ScopePriority prio_map) =
    Derive.ScopePriority (convert <$> prio_map)

-- | Create docs for generator and transformer calls, and merge and sort them.
merged_scope_docs :: [(CallType, Derive.ScopePriority Derive.DocumentedCall)]
    -> [ScopeDoc]
merged_scope_docs = merge_scope_docs . concatMap (uncurry scope_type)

merge_scope_docs :: [ScopeDoc] -> [ScopeDoc]
merge_scope_docs = map (second (sort_calls . concat)) . Lists.groupFst

sort_calls :: [CallBindings] -> [CallBindings]
sort_calls = Lists.sortOn $ \(binds, _, _) ->
    Text.toLower . fst <$> Lists.head binds

-- | A 'Derive.Library' only has builtins, but ScopeDoc wants a source so
-- it can work uniformly with 'track_sections', which does have separate
-- sources.
imported_scope_doc :: CallType -> [Entry] -> [ScopeDoc]
imported_scope_doc ctype lookups = [(Nothing, entries ctype lookups)]

scope_type :: CallType -> Derive.ScopePriority Derive.DocumentedCall
    -> [ScopeDoc]
scope_type ctype (Derive.ScopePriority prio_map) =
    map (first Just) $ filter (not . null . snd) $ Map.toDescList $
        entries ctype . call_map_to_entries <$> prio_map

call_map_to_entries :: CallMap -> [Entry]
call_map_to_entries (Derive.CallMap calls patterns) =
    map (uncurry Library.Single) (Map.toAscList calls)
    ++ map Library.Pattern patterns

entries :: CallType -> [Entry] -> [CallBindings]
entries ctype = group . map (extract . go) . map flatten
    where
    flatten (Library.Pattern pattern) =
        (Left (Derive.pat_description pattern), Derive.pat_doc pattern)
    flatten (Library.Single sym doc) = (Right sym, doc)
    go (Left desc, doc) = ("pattern:" <> desc, doc)
    go (Right sym, doc) = (show_sym sym, doc)
    show_sym (Expr.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
    extract (sym, Derive.DocumentedCall name doc) = ((sym, name), doc)
    -- Group calls with the same CallDoc.
    group docs = [(map fst group, ctype, doc)
        | (doc, group) <- Lists.keyedGroupSort snd docs]
