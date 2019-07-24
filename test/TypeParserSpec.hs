module TypeParserSpec (spec) where

import           Test.Hspec
import           Typed
import           TypeParser
import           Text.Megaparsec
import           Test.Hspec.Megaparsec
import           Data.Map (Map)
import qualified Data.Map as M
import           TestTypeData

testDeclarations :: TypeDeclarations
testDeclarations = TD $ M.fromList [
                           ("List[A]", lst a),
                           ("Rectangle", rect),
                           ("Shape", shape)
                          ]

spec :: Spec
spec = do
    describe "Basic parsers test" $ do
        it "should read type name" $ example $ do
            parse nameP "" "Hello" `shouldParse` "Hello"

        it "should read params" $ example $ do
            parse typeParamsP "" "[A, +B, -C]" `shouldParse` [inv "A", cov "B", contr "C"]

        it "should fail on an empty param list " $ example $ do
            parse typeParamsP "" `shouldFailOn` "[]"

        it "should get parameterless type from context" $ example $ do
            parse (parseTypeP testDeclarations) "" "Shape" `shouldParse` shape
            parse (parseTypeP testDeclarations) "" "Rectangle" `shouldParse` rect
        
