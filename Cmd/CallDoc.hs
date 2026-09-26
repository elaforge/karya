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
docText :: Document -> Lazy.Text
docText = Format.render "  " 75 . mconcatMap section
    where
    section (callKind, scopeDocs) =
        "##" <+> Format.text callKind <+> "calls" <> "\n\n"
        <> mconcatMap scopeDoc scopeDocs
    scopeDoc (source, calls) = "### from" <+> Format.text (pretty source)
        <> "\n\n" <> Format.unlines (map (callBindingsText True) calls)

callBindingsText :: Bool -> CallBindings -> Format.Doc
callBindingsText includeModule (binds, ctype, callDoc) =
    Format.unlines (map showBind binds)
    </> showCallDoc callDoc
    <> "\n\n"
    where
    showBind (sym, Derive.CallName name) =
        Format.text sym <+> "--" <+> Format.text name
        <+> (if includeModule
            then Format.text (showModule (Derive.cdocModule callDoc))
            else mempty)
        <+> ("(" <> Format.text (pretty ctype) <> ")")
    showCallDoc (Derive.CallDoc _module tags doc args)
        | tags == mempty && null args = writeDoc doc
        | otherwise = writeDoc doc <//> writeTags tags
            <> Format.indentLine (argDocs args)
    argDocs = Format.unlines . map argDoc
    argDoc (Derive.ArgDoc name typ parser envDefault doc) =
        Format.text (unname name <> super <> " :: " <> pretty typ
            <> maybe "" (" = "<>) deflt
            <> " " <> environKeys name envDefault
            <> " --")
        <+/> writeDoc doc
        where
        (super_, deflt) = showParser parser
        super = maybe "" (\t -> if Text.length t == 1 then t else " " <> t)
            super_
    writeTags tags
        -- Otherwise there's nothing separating the call doc and args doc.
        | tags == mempty = "Args:"
        | otherwise = Format.text $ "Tags: "
            <> Text.intercalate ", " (Tags.untag tags)
    unname (Derive.ArgName s) = s

environKeys :: Derive.ArgName -> Sig.EnvironDefault -> Text
environKeys name deflt =
    "[" <> Text.intercalate ", " (Sig.environKeys "*" name deflt) <> "]"

writeDoc :: Doc.Doc -> Format.Doc
writeDoc =
    Format.indent . Format.wrapWords . map Format.text . Text.words . undoc
    where undoc (Doc.Doc s) = s

showModule :: Module.Module -> Text
showModule (Module.Module name) = "[" <> name <> "]"

showParser :: Derive.ArgParser -> (Maybe Text, Maybe Text)
    -- ^ (superscript to mark this arg, default value)
showParser p = case p of
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
bindingsText :: Document -> Text
bindingsText =
    Text.unlines . filter (not . Text.null) . concatMap section . flatten
    where
    section ((callKind, callType), bindings)
        | null bindings = []
        | otherwise = "\t" <> callKind <> " " <> pretty callType
            : map binding bindings
    binding (scopeSource,
            ((symbolName, Derive.CallName callName), callDoc)) =
        doc <> " " <> Texts.ellipsis (width - Text.length doc - 1)
            (undoc (Derive.cdocDoc callDoc))
        where
        undoc (Doc.Doc d) = d
        doc = symbolName <> " -- " <> callName <> " (" <> pretty scopeSource
            <> ") " <> showModule (Derive.cdocModule callDoc)
    width = 76
    flatten :: Document
        -> [((CallKind, CallType), [(ScopeSource, (Binding, Derive.CallDoc))])]
    flatten sections = map (second (Lists.sortOn fst . concat)) $ Lists.groupFst
        [ ((callKind, callType),
            map (scopeSource,) (map (, callDoc) bindings))
        | (callKind, scopeDocs) <- sections
        , (scopeSource, callBindings) <- scopeDocs
        , (bindings, callType, callDoc) <- callBindings
        ]

-- ** html output

-- | Convert a Document to HTML.
docHtml :: Html.HtmlState -> Document -> Html.Html
docHtml hstate = (htmlHeader hstate <>) . mconcatMap section
    where
    section (callKind, scopeDocs) =
        Html.tagClass "div" "call-kind" $ tag "h2" (html callKind)
            <> "\n\n" <> mconcatMap (scopeDoc callKind) scopeDocs
    scopeDoc callKind (maybeSource, callBindings) = case maybeSource of
        -- 'imported_scope_doc' does this since Library doesn't have source
        -- types.
        Nothing -> doc
        Just source -> Html.tagClass "div" "call-source" $
            tag "h3" ("from " <> html (pretty source)) <> "\n\n" <> doc
        where
        doc = "<dl class=main-dl>\n"
            <> mconcatMap (showModuleGroup callKind)
                (Lists.keyedGroupSort moduleOf callBindings)
            <> "</dl>\n"
    moduleOf (_, _, callDoc) = Derive.cdocModule callDoc
    showModuleGroup callKind (module_, callBindings) =
        Html.tagClass "div" "call-module" $
            showModule module_ (length callBindings) <> "<br>\n"
            <> mconcatMap (callBindingsHtml hstate callKind) callBindings
    showModule (Module.Module m) calls = tag "center" $
        tag "b" "Module: " <> tag "code" (html m)
            <> " (" <> html (showt calls) <> " calls)"

htmlHeader :: Html.HtmlState -> Html.Html
htmlHeader hstate = mconcat
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
        \<input id=input type=text size=60 value=\"" <> defaultSearch
        <> "\" onchange=\"search(this.value)\">\n\
        \<br>You can also search by <code>%control</code>, arg default\n\
        \(<code>name-arg</code>), and call kind (<code>note</code>,\n\
        \<code>control</code>, ...).\
        \\n<br>Search for calls with the browser's text search, \"call --\"\
        \ to search by binding, \"-- call\" to search by name.\n<br>"
    , Html.htmlDoc hstate "Common tags are documented at 'Derive.Call.Tags'."
    , "\n<p> <span id=totals> x </span>\n"
    ]
    where defaultSearch = "-m:internal -m:ly "

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
    [ searchJavascript
    , ""
    , hideEmptyJavascript
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

searchJavascript :: Html.Html
searchJavascript = Lists.join "\n"
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

hideEmptyJavascript :: Html.Html
hideEmptyJavascript = Lists.join "\n"
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

callBindingsHtml :: Html.HtmlState -> Text -> CallBindings -> Html.Html
callBindingsHtml hstate callKind bindings@(binds, ctype, callDoc) = mconcat
    [ "<div class=call tags=\"" <> html (Text.unwords tags) <> "\">"
    , mconcatMap (showBind (Derive.cdocModule callDoc))
        (zip (True : repeat False) binds)
        <> showCallDoc callDoc
    , "</div>\n\n"
    ]
    where
    tags = "kind:" <> callKind : bindingTags bindings
    showBind module_ (first, (sym, Derive.CallName name)) =
        "<dt>" <> tag "code" (html sym)
        -- This used to be &mdash;, but that's too hard to use text search on.
        <> " -- " <> tag "b" (html name)
        <> (if first then showCtype module_ else "") <> "\n"
    showCtype module_ = mconcat
        [ "<div style='float:right'>"
        , tag "code" (html (pretty module_))
        , " : "
        , tag "em" (html (callKind <> " " <> pretty ctype))
        , "</div>"
        ]
    showCallDoc (Derive.CallDoc _module tags doc args) =
        "<dd> <dl class=compact>\n"
        <> Html.htmlDoc hstate doc
        <> writeTags tags <> "\n"
        <> tag "ul" (mconcatMap argDoc args)
        <> "</dl>\n"
    argDoc (Derive.ArgDoc name typ parser envDefault doc) =
        "<li" <> liType <> ">" <> tag "code" (html (unname name))
        <> showChar char
        <> " :: " <> tag "em" (html (pretty typ))
        <> showDefault deflt
        <> " " <> tag "code" (html (environKeys name envDefault))
        <> (if doc == "" then "" else " &mdash; " <> Html.htmlDoc hstate doc)
        <> "\n"
        where
        unname (Derive.ArgName s) = s
        (char, deflt) = showParser parser
        liType = if Maybe.isNothing deflt then ""
            else " style=list-style-type:circle"
    showDefault = maybe "" ((" = " <>) . tag "code" . html)
    showChar = maybe "" (tag "sup" . html)
    writeTags tags
        | tags == mempty = ""
        | otherwise = "<br><b>Tags:</b> "
            <> tag "em" (html (Text.intercalate ", " (Tags.untag tags)))

-- | Extract explicit tags as well as some implicit tags.  Implicit tags are
-- @%control@ for controls in the default arguments, @name-arg@ for environ
-- keys that default the arguments, and @note@, @control@, @pitch@, or @val@ for
-- the call kind.
bindingTags :: CallBindings -> [Text]
bindingTags (binds, ctype, callDoc) =
    Lists.unique $ "type:" <> pretty ctype : extract callDoc
    where
    names = map snd binds
    extract callDoc = module_ (Derive.cdocModule callDoc)
        : cdocTags callDoc ++ concatMap argTags (Derive.cdocArgs callDoc)
    cdocTags = Tags.untag . Derive.cdocTags
    module_ (Module.Module m) = "m:" <> m
    argTags arg =
        [ Sig.prefixedEnviron name (Derive.argName arg)
        | name <- names
        ] ++ argControlTags (Derive.argParser arg)
    -- An arg with a control signal default should look like "%sig,.5".
    -- This is a hack, since the default isn't stored in a structured way.
    argControlTags (Derive.Defaulted deflt)
        | "%" `Text.isPrefixOf` deflt = [Text.takeWhile (/=',') deflt]
        | otherwise = []
    argControlTags _ = []


-- * scale doc

type Scale = [CallBindings]

scalesHtml :: Html.HtmlState -> [CallBindings] -> Html.Html
scalesHtml hstate scales = htmlHeader hstate
        <> "<h2>Scales</h2>\n"
        <> "<dl class=main-dl>\n" <> scaleHtml scales <> "</dl>\n"
    where scaleHtml = mconcatMap (callBindingsHtml hstate "scale")

-- | Extract documentation from scales.
scaleDocs :: [(Pitch.ScaleId, Text, Derive.DocumentedCall)] -> [CallBindings]
scaleDocs = sortCalls . entries ValCall . map convert
    where
    convert (scaleId, pattern, doc) = Library.Pattern $
        Derive.PatternCall desc doc (const (pure Nothing))
        where desc = pretty scaleId <> ": " <> pattern

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
filterCalls :: (SymbolName -> Derive.CallName -> Bool) -> Document -> Document
filterCalls matches = stripEmpty . map (second (map scopeDoc))
    where
    scopeDoc = second (map callBindings)
    callBindings (bindings, callType, callDoc) =
        (filter binding bindings, callType, callDoc)
    binding (sym, call) = matches sym call

stripEmpty :: Document -> Document
stripEmpty = mapMaybe section
    where
    section (callKind, scopeDocs) = case mapMaybe scopeDoc scopeDocs of
        [] -> Nothing
        stripped -> Just (callKind, stripped)
    scopeDoc (scopeSource, callBindings) =
        case mapMaybe callBinding callBindings of
            [] -> Nothing
            stripped -> Just (scopeSource, stripped)
    callBinding b@(bindings, _, _)
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
    , ("val", convertModules ValCall val)
    ]

convertModules :: CallType -> Map mod (Derive.CallMap (Derive.Call d))
    -> [ScopeDoc]
convertModules ctype = importedScopeDoc ctype
    . concatMap (callMapToEntries . callMapDoc)
    . Map.elems

-- | Create docs for generator, transformer, and track calls, and merge and
-- sort them.
scope :: Map Module.Module (Derive.CallMap (Derive.Call gen))
    -> Map Module.Module (Derive.CallMap (Derive.Call trans))
    -> Map Module.Module (Derive.CallMap (Derive.TrackCall track))
    -> [ScopeDoc]
scope gen trans track = mergeScopeDocs $ concat
    [ convertModules GeneratorCall gen
    , convertModules TransformerCall trans
    , convertModules TrackCall track
    ]

-- | A 'Library.Entry' with the call stripped out and replaced with
-- just documentation.  This is so 'Derive.Call's and 'Derive.ValCall's can
-- be treated uniformly.
type Entry = Library.Entry Derive.DocumentedCall
type CallMap = Derive.CallMap Derive.DocumentedCall

-- | Convert 'Library.Entry' to 'Entry' by stripping out the code part of the
-- call, and replacing it with DocumentedCall.
entryDoc :: (call -> Derive.DocumentedCall) -> Library.Entry call -> Entry
entryDoc extractDoc (Library.Single sym call) =
    Library.Single sym (extractDoc call)
entryDoc _ (Library.Pattern pattern) =
    Library.Pattern $ pattern { Derive.patFunction = const $ pure Nothing }

callMapDoc :: Derive.CallMap (Derive.Call d) -> CallMap
callMapDoc (Derive.CallMap calls patterns) = Derive.CallMap
    { callMap = Derive.extractDoc <$> calls
    , callPatterns =
        map (\p -> p { Derive.patFunction = const $ pure Nothing }) patterns
    }

-- ** instrument doc

-- | Get docs for the calls introduced by an instrument.
instrumentCalls :: Derive.InstrumentCalls -> Document
instrumentCalls (Derive.Scopes gen trans track vals) =
    [ ("note", [(Just Derive.PrioInstrument, concat
        [ callMapEntries GeneratorCall gen
        , callMapEntries TransformerCall trans
        , callMapEntries TrackCall track
        ])])
    , ("val", [(Just Derive.PrioInstrument, callMapEntries ValCall vals)])
    ]
    where
    callMapEntries ctype = entries ctype . callMapToEntries . callMapDoc

-- ** track doc

-- | Get documentation for calls in scope at the given block and track.
track :: Cmd.M m => BlockId -> TrackId -> m Document
track blockId trackId = do
    dynamic <- Cmd.require "CallDoc.track: no root dynamic"
        =<< Perf.lookupRootDynamic (blockId, Just trackId)
    ttype <- ParseTitle.trackType <$> Ui.getTrackTitle trackId
    pure $ trackSections ttype (Derive.stateScopes dynamic)

-- | This is an alternate doc extraction path which extracts the docs from
-- 'Derive.Scopes' instead of 'Derive.Builtins'.
trackSections :: ParseTitle.Type -> Derive.Scopes -> [Section]
trackSections ttype (Derive.Scopes
        (Derive.Scope genN genC genP)
        (Derive.Scope transN transC transP)
        (Derive.Scope trackN trackC trackP)
        val) =
    (\d -> [d, ("val", valDoc)]) $ case ttype of
        ParseTitle.NoteTrack -> ("note", merge3 genN transN trackN)
        ParseTitle.ControlTrack -> ("control", merge3 genC transC trackC)
        ParseTitle.TempoTrack -> ("tempo", merge3 genC transC trackC)
        ParseTitle.PitchTrack -> ("pitch", merge3 genP transP trackP)
    where
    merge3 gen trans track = mergedScopeDocs
        [ (GeneratorCall, convertScope gen)
        , (TransformerCall, convertScope trans)
        , (TrackCall, convertScope track)
        ]
    valDoc = scopeType ValCall $ convertScope val

convertScope :: Derive.ScopePriority (Derive.Call d)
    -> Derive.ScopePriority Derive.DocumentedCall
convertScope (Derive.ScopePriority prioMap) =
    Derive.ScopePriority (callMapDoc <$> prioMap)

-- | Create docs for generator and transformer calls, and merge and sort them.
mergedScopeDocs :: [(CallType, Derive.ScopePriority Derive.DocumentedCall)]
    -> [ScopeDoc]
mergedScopeDocs = mergeScopeDocs . concatMap (uncurry scopeType)

mergeScopeDocs :: [ScopeDoc] -> [ScopeDoc]
mergeScopeDocs = map (second (sortCalls . concat)) . Lists.groupFst

sortCalls :: [CallBindings] -> [CallBindings]
sortCalls = Lists.sortOn $ \(binds, _, _) ->
    Text.toLower . fst <$> Lists.head binds

-- | A 'Derive.Library' only has builtins, but ScopeDoc wants a source so
-- it can work uniformly with 'track_sections', which does have separate
-- sources.
importedScopeDoc :: CallType -> [Entry] -> [ScopeDoc]
importedScopeDoc ctype lookups = [(Nothing, entries ctype lookups)]

scopeType :: CallType -> Derive.ScopePriority Derive.DocumentedCall
    -> [ScopeDoc]
scopeType ctype (Derive.ScopePriority prioMap) =
    map (first Just) $ filter (not . null . snd) $ Map.toDescList $
        entries ctype . callMapToEntries <$> prioMap

callMapToEntries :: CallMap -> [Entry]
callMapToEntries (Derive.CallMap calls patterns) =
    map (uncurry Library.Single) (Map.toAscList calls)
    ++ map Library.Pattern patterns

entries :: CallType -> [Entry] -> [CallBindings]
entries ctype = group . map (extract . go) . map flatten
    where
    flatten (Library.Pattern pattern) =
        (Left (Derive.patDescription pattern), Derive.patDoc pattern)
    flatten (Library.Single sym doc) = (Right sym, doc)
    go (Left desc, doc) = ("pattern:" <> desc, doc)
    go (Right sym, doc) = (showSym sym, doc)
    showSym (Expr.Symbol sym)
        | Text.null sym = "\"\""
        | otherwise = sym
    extract (sym, Derive.DocumentedCall name doc) = ((sym, name), doc)
    -- Group calls with the same CallDoc.
    group docs = [(map fst group, ctype, doc)
        | (doc, group) <- Lists.keyedGroupSort snd docs]
