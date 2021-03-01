-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Extract Korvai definitions into a list in a generated module.
module Solkattu.ExtractKorvais where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Environment

import qualified Util.ExtractHs as ExtractHs
import qualified Util.Texts as Texts
import Global


main :: IO ()
main = do
    args <- System.Environment.getArgs
    ExtractHs.process args
        (extract . ExtractHs.typeDeclarations . ExtractHs.stripComments)
        generate

data Type = Korvai | Score deriving (Eq, Show)

-- | Extract "something :: Korvai" lines.
extract :: [(Int, (Text, Text))] -> [(Type, (Int, Text))]
    -- ^ (lineno, variableName)
extract = mapMaybe $ \(lineno, (variable, type_)) ->
    (, (lineno, variable)) <$> case type_ of
        "Korvai" -> Just Korvai
        "Score" -> Just Score
        _ -> Nothing

generate :: FilePath -> Map FilePath [(Type, (Int, Text))]
    -> Either ExtractHs.Error ([ExtractHs.Warning], Text)
generate outFname extracted = fmap (warnings,) $
    Texts.interpolate template $ Map.fromList
        [ ("module", ExtractHs.moduleDeclaration outFname)
        , ("imports", Text.unlines $
            map ExtractHs.makeImport (Map.keys fnameDefs))
        , ("scores", Text.intercalate "\n    , "
            [ scoreDef fname def type_
            | (fname, defs) <- Map.toList fnameDefs, (type_, def) <- defs
            ])
        ]
    where
    (empty, fnameDefs) = Map.partition null extracted
    warnings = map (("Warning: no korvai defs in " <>) . txt) (Map.keys empty)

scoreDef :: FilePath -> (Int, Text) -> Type -> Text
scoreDef fname (lineno, variableName) type_ = Text.unwords $ filter (/="")
    [ "setLocation", showt (module_, lineno, variableName), "$"
    , case type_ of
        Score -> ""
        Korvai -> "Single"
    , module_ <> "." <> variableName
    ]
    where module_ = ExtractHs.pathToModule fname

template :: Text
template =
    "-- | Collect korvais into one database.\n\
    \-- This is automatically generated, but checked in for convenience.\n\
    \-- Don't edit it directly.  Any modifications to the the source\n\
    \-- directory should cause it to be regenerated.\n\
    \${module}\n\
    \import qualified Solkattu.Korvai as Korvai\n\
    \import           Solkattu.Korvai (Score(Single), setLocation)\n\
    \${imports}\n\
    \\n\
    \scores :: [Korvai.Score]\n\
    \scores = map Korvai.inferMetadataS\n\
    \    [ ${scores}\n\
    \    ]\n"
