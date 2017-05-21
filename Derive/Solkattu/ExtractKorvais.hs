-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Extract Korvai definitions into a list in a generated module.
module Derive.Solkattu.ExtractKorvais where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Environment

import qualified Util.ExtractHs as ExtractHs
import qualified Util.TextUtil as TextUtil
import Global


main :: IO ()
main = do
    args <- System.Environment.getArgs
    ExtractHs.process args
        (extract . ExtractHs.typeDeclarations . ExtractHs.stripComments)
        generate

extract :: [(Int, (Text, Text))] -> [(Int, Text)] -- ^ (lineno, variableName)
extract = mapMaybe $ \(lineno, (variable, type_)) -> if type_ == "Korvai"
    then Just (lineno, variable) else Nothing

generate :: FilePath -> Map FilePath [(Int, Text)]
    -> Either ExtractHs.Error ([ExtractHs.Warning], Text)
generate outFname extracted = fmap (warnings,) $
    TextUtil.interpolate template $ Map.fromList
        [ ("module", ExtractHs.moduleDeclaration outFname)
        , ("imports", Text.unlines $
            map ExtractHs.makeImport (Map.keys fnameDefs))
        , ("korvais", Text.intercalate "\n    , "
            [ korvaiDef fname def
            | (fname, defs) <- Map.toList fnameDefs, def <- defs
            ])
        ]
    where
    (empty, fnameDefs) = Map.partition null extracted
    warnings = map (("Warning: no korvai defs in " <>) . txt) (Map.keys empty)

korvaiDef :: FilePath -> (Int, Text) -> Text
korvaiDef fname (lineno, variableName) = mconcat
    [ "variable_name ", showt variableName
    , " $\n        module_ ", showt module_
    , " $\n        line_number ", showt lineno
    , " ", module_ <> "." <> variableName
    ]
    where module_ = ExtractHs.pathToModule fname

template :: Text
template =
    "-- | Collect korvais into one database.\n\
    \-- This is automatically generated, but checked in for convenience.\n\
    \-- Don't edit it directly.  Any modifications to the the source\n\
    \-- directory should cause it to be regenerated.\n\
    \${module}\n\
    \import qualified Derive.Solkattu.Korvai as Korvai\n\
    \import Derive.Solkattu.Metadata\n\
    \${imports}\n\
    \\n\
    \korvais :: [Korvai.Korvai]\n\
    \korvais =\n\
    \    [ ${korvais}\n\
    \    ]\n"

