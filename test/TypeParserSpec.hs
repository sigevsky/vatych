module TypeParserSpec (spec) where

import           Test.Hspec
import           Typed
import           TypeParser
import           Text.Megaparsec
import           Test.Hspec.Megaparsec
import           Data.Map (Map)
import qualified Data.Map as M
import           TestTypeData
import           Prelude hiding (any)

testDeclarations :: TypeDeclarations
testDeclarations = TD $ M.fromList [
                           ("Any", any),
                           ("List", lst a),
                           ("Rectangle", rect),
                           ("Shape", shape),
                           ("Map", mapB k v)
                          ]

spec :: Spec
spec = 
    describe "Basic parsers test" $ do
        it "should read type name" $ example $
            parse nameP "" "Hello" `shouldParse` "Hello"

        it "should read params" $ example $ do
            parse typeParamsP "" "[A]" `shouldParse` [inv "A"]
            parse typeParamsP "" "[A, +B, -C]" `shouldParse` [inv "A", cov "B", contr "C"]

        it "should fail on an empty param list " $ example $
            parse typeParamsP "" `shouldFailOn` "[]"

        it "should get parameterless type from context" $ example $ do
            parse (parseTypeP testDeclarations) "" "Shape" `shouldParse` shape
            parse (parseTypeP testDeclarations) "" "Rectangle" `shouldParse` rect

        it "should parse deeply nested type" $ example $ do
            parse (parseTypeP testDeclarations) "" "Map[Rectangle, A]" `shouldParse` mapB rect a
            parse (parseTypeP testDeclarations) "" "Map[List[Rectangle], List[Map[Any, List[A]]]]" 
                `shouldParse` mapB (lst rect) (lst (mapB any (lst a)))

        it "should fail on a mismatching argument list" $ example $ do
            parse (parseTypeP testDeclarations) "" `shouldFailOn` "Map[Rectangle]"
            parse (parseTypeP testDeclarations) "" `shouldFailOn` "Map[Rectangle, A, Shape]"

        it "should fail on unknown type of a kind (* -> *)" $ example $
            parse (parseTypeP testDeclarations) "" `shouldFailOn` "List[F[A]]"
