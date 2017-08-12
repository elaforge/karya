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
import Util.Doc (html, tag)
import qualified Util.Format as Format
import Util.Format ((<+>), (</>), (<//>), (<+/>))
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
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
        doc <> " " <> TextUtil.ellipsis (width - Text.length doc - 1)
            (undoc (Derive.cdoc_doc call_doc))
        where
        undoc (Doc.Doc d) = d
        doc = symbol_name <> " -- " <> call_name <> " (" <> pretty scope_source
            <> ") " <> show_module (Derive.cdoc_module call_doc)
    width = 76
    flatten :: Document
        -> [((CallKind, CallType), [(ScopeSource, (Binding, Derive.CallDoc))])]
    flatten sections = map (second (Seq.sort_on fst . concat)) $ Seq.group_fst
        [ ((call_kind, call_type),
            map (scope_source,) (map (, call_doc) bindings))
        | (call_kind, scope_docs) <- sections
        , (scope_source, call_bindings) <- scope_docs
        , (bindings, call_type, call_doc) <- call_bindings
        ]

-- ** html output

-- | Convert a Document to HTML.
doc_html :: Doc.HtmlState -> Document -> Doc.Html
doc_html hstate = (html_header hstate <>) . mconcatMap section
    where
    section (call_kind, scope_docs) =
        Doc.tag_class "div" "call-kind" $ tag "h2" (html call_kind)
            <> "\n\n" <> mconcatMap (scope_doc call_kind) scope_docs
    scope_doc call_kind (maybe_source, call_bindings) = case maybe_source of
        -- 'imported_scope_doc' does this since Library doesn't have source
        -- types.
        Nothing -> doc
        Just source -> Doc.tag_class "div" "call-source" $
            tag "h3" ("from " <> html (pretty source)) <> "\n\n" <> doc
        where
        doc = "<dl class=main-dl>\n"
            <> mconcatMap (show_module_group call_kind)
                (Seq.keyed_group_sort module_of call_bindings)
            <> "</dl>\n"
    module_of (_, _, call_doc) = Derive.cdoc_module call_doc
    show_module_group call_kind (module_, call_bindings) =
        Doc.tag_class "div" "call-module" $
            show_module module_ (length call_bindings) <> "<br>\n"
            <> mconcatMap (call_bindings_html hstate call_kind) call_bindings
    show_module (Module.Module m) calls = tag "center" $
        tag "b" "Module: " <> tag "code" (html m)
            <> " (" <> html (showt calls) <> " calls)"

html_header :: Doc.HtmlState -> Doc.Html
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
        \ <code>type:(val|generator|transformer)</code> for call types.\n\
        \<input id=input type=text size=60 value=\"" <> default_search
        <> "\" onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...).\
        \\n<br>Search for calls with the browser's text search, \"call --\"\
        \ to search by binding, \"-- call\" to search by name.\n<br>"
    , Doc.html_doc hstate "Common tags are documented at 'Derive.Call.Tags'."
    , "\n<p> <span id=totals> x </span>\n"
    ]
    where default_search = "-m:internal -m:ly "

css :: Doc.Html
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

javascript :: Doc.Html
javascript = Seq.join "\n"
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

search_javascript :: Doc.Html
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

hide_empty_javascript :: Doc.Html
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

call_bindings_html :: Doc.HtmlState -> Text -> CallBindings -> Doc.Html
call_bindings_html hstate call_kind bindings@(binds, ctype, call_doc) =
    "<div class=call tags=\"" <> html (Text.unwords tags) <> "\">"
    <> mconcatMap show_bind (zip (True : repeat False) binds)
        <> show_call_doc call_doc
    <> "</div>\n\n"
    where
    tags = "kind:" <> call_kind : binding_tags bindings
    show_bind (first, (sym, Derive.CallName name)) =
        "<dt>" <> tag "code" (html sym)
        -- This used to be &mdash;, but that's too hard to use text search on.
        <> " -- " <> tag "b" (html name)
        <> (if first then show_ctype else "") <> "\n"
    show_ctype = "<div style='float:right'>"
        <> tag "em" (html (pretty ctype)) <> "</div>"
    show_call_doc (Derive.CallDoc _module tags doc args) =
        "<dd> <dl class=compact>\n"
        <> Doc.html_doc hstate doc
        <> write_tags tags <> "\n"
        <> tag "ul" (mconcatMap arg_doc args)
        <> "</dl>\n"
    arg_doc (Derive.ArgDoc name typ parser env_default doc) =
        "<li" <> li_type <> ">" <> tag "code" (html (unname name))
        <> show_char char
        <> " :: " <> tag "em" (html (pretty typ))
        <> show_default deflt
        <> " " <> tag "code" (html (environ_keys name env_default))
        <> (if doc == "" then "" else " &mdash; " <> Doc.html_doc hstate doc)
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
    Seq.unique $ "type:" <> pretty ctype : extract call_doc
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

scales_html :: Doc.HtmlState -> [CallBindings] -> Doc.Html
scales_html hstate scales = html_header hstate
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

-- | Emit docs for all calls in the default scope.
builtin :: Cmd.M m => m Document
builtin = library <$> Cmd.gets (Cmd.config_library . Cmd.state_config)

-- | Extract all the documentation from a Library.  Document extraction is
-- a big mess of walking over nested data and converting it to a parallel
-- nested data structure.  It's tedious, but the types make it hard to get
-- wrong.
library :: Derive.Library -> [Section]
library (Derive.Library note control pitch val _aliases) =
    [ ("note", call_maps note)
    , ("control", call_maps control)
    , ("pitch", call_maps pitch)
    , ("val", imported_scope_doc ValCall (map convert_val_call val))
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

convert_track_call :: Derive.LookupCall (Derive.TrackCall d) -> LookupCall
convert_track_call = fmap_lookup Derive.extract_track_doc

fmap_lookup :: (a -> b) -> Derive.LookupCall a -> Derive.LookupCall b
fmap_lookup extract_doc (Derive.LookupMap calls) = Derive.LookupMap $
    Map.map extract_doc calls
fmap_lookup _ (Derive.LookupPattern pattern doc _) =
    Derive.LookupPattern pattern doc (const (return Nothing))

-- | Create docs for generator, transformer, and track calls, and merge and
-- sort them.
call_maps :: Derive.CallMaps d -> [ScopeDoc]
call_maps (Derive.Scopes generator transformer track ()) = merge_scope_docs $
    imported_scope_doc GeneratorCall (convert generator)
    ++ imported_scope_doc TransformerCall (convert transformer)
    ++ imported_scope_doc TrackCall (map convert_track_call track)
    where convert = map convert_call

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrument_calls :: Derive.InstrumentCalls -> Document
instrument_calls (Derive.Scopes gen trans track vals) =
    [ ("note", [(Just Derive.PrioInstrument,
        lookup GeneratorCall gen ++ lookup TransformerCall trans ++ track_doc)])
    , ("val", [(Just Derive.PrioInstrument,
        lookup_calls ValCall (map convert_val_call vals))])
    ]
    where
    lookup ctype = lookup_calls ctype . map convert_call
    track_doc = lookup_calls TrackCall (map convert_track_call track)

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: Cmd.M m => BlockId -> TrackId -> m Document
track block_id track_id = do
    dynamic <- Cmd.require "CallDoc.track: no root dynamic"
        =<< Perf.lookup_root_dynamic (block_id, Just track_id)
    ttype <- ParseTitle.track_type <$> Ui.get_track_title track_id
    return $ track_sections ttype (Derive.state_scopes dynamic)

-- | This is an alternate doc extraction path which extracts the docs from
-- 'Derive.Scopes' instead of 'Derive.Library'.
track_sections :: ParseTitle.Type -> Derive.Scopes -> [Section]
track_sections ttype (Derive.Scopes
        (Derive.Scope gen_n gen_c gen_p)
        (Derive.Scope trans_n trans_c trans_p)
        (Derive.Scope track_n track_c track_p)
        val_) =
    (\d -> [d, ("val", val_doc)]) $ case ttype of
        ParseTitle.NoteTrack -> ("note", merge3 gen_n trans_n track_n)
        ParseTitle.ControlTrack -> ("control", merge3 gen_c trans_c track_c)
        ParseTitle.TempoTrack -> ("tempo", merge3 gen_c trans_c track_c)
        ParseTitle.PitchTrack -> ("pitch", merge3 gen_p trans_p track_p)
    where
    merge3 gen trans track = merged_scope_docs
        [ (GeneratorCall, convert gen)
        , (TransformerCall, convert trans)
        , (TrackCall, convert_scope convert_track_call track)
        ]
    val_doc = scope_type ValCall $ convert_scope convert_val_call val_
    convert = convert_scope convert_call

convert_scope :: (Derive.LookupCall call -> LookupCall)
    -> Derive.ScopePriority call -> Derive.ScopePriority Derive.DocumentedCall
convert_scope convert (Derive.ScopePriority prio_map) =
    Derive.ScopePriority (map convert <$> prio_map)

-- | Create docs for generator and transformer calls, and merge and sort them.
merged_scope_docs :: [(CallType, Derive.ScopePriority Derive.DocumentedCall)]
    -> [ScopeDoc]
merged_scope_docs = merge_scope_docs . concatMap (uncurry scope_type)

merge_scope_docs :: [ScopeDoc] -> [ScopeDoc]
merge_scope_docs = map (second (sort_calls . concat)) . Seq.group_fst

sort_calls :: [CallBindings] -> [CallBindings]
sort_calls = Seq.sort_on $ \(binds, _, _) ->
    Text.toLower . fst <$> Seq.head binds

-- | A 'Derive.Library' only has builtins, but ScopeDoc wants a source so
-- it can work uniformly with 'track_sections', which does have separate
-- sources.
imported_scope_doc :: CallType -> [LookupCall] -> [ScopeDoc]
imported_scope_doc ctype lookups =
    [(Nothing, lookup_calls ctype lookups)]

scope_type :: CallType -> Derive.ScopePriority Derive.DocumentedCall
    -> [ScopeDoc]
scope_type ctype (Derive.ScopePriority call_map) =
    map (first Just) $ filter (not . null . snd) $ Map.toDescList $
        lookup_calls ctype <$> call_map

lookup_calls :: CallType -> [LookupCall] -> [CallBindings]
lookup_calls ctype = group . map (extract . go) . concatMap flatten
    where
    flatten (Derive.LookupPattern pattern doc _) = [(Left pattern, doc)]
    flatten (Derive.LookupMap cmap) =
        [(Right sym, call) | (sym, call) <- Map.toAscList cmap]
    go (Left pattern, call) = ("lookup: " <> pattern, call)
    go (Right sym, call) = (show_sym sym, call)
    show_sym (Expr.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
    extract (sym, Derive.DocumentedCall name doc) = ((sym, name), doc)
    -- Group calls with the same CallDoc.
    group docs = [(map fst group, ctype, doc)
        | (doc, group) <- Seq.keyed_group_sort snd docs]
