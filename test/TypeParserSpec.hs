{-# LANGUAGE OverloadedStrings #-}

module TypeParserSpec (spec) where

import           Test.Hspec
import           Typed
import           TypeParser
import           Text.Megaparsec hiding (single)
import           Test.Hspec.Megaparsec
import           Data.Map (Map)
import qualified Data.Map as M
import           TestTypeData
import           Prelude hiding (any)
import           Data.Text.Internal

testCont :: TypeDeclarations
testCont = TD $ M.fromList [
                           ("Any", any),
                           ("List", lst a),
                           ("Rectangle", rect),
                           ("Shape", shape),
                           ("Map", mapB k v)
                          ]

spec :: Spec
spec = do
    describe "Basic parsers capabilities test" $ do
        it "should read type name" $ example $
            parse nameP "" "Hello" `shouldParse` "Hello"

        it "should read params" $ example $ do
            parse typeParamsP "" "[A]" `shouldParse` [inv "A"]
            parse typeParamsP "" "[A, +B, -C]" `shouldParse` [inv "A", cov "B", contr "C"]

        it "should fail on an empty param list " $ example $
            parse typeParamsP "" `shouldFailOn` "[]"

        it "should get parameterless type from context" $ example $ do
            parse (parseTypeP testCont) "" "Shape" `shouldParse` shape
            parse (parseTypeP testCont) "" "Rectangle" `shouldParse` rect

        it "should parse deeply nested type" $ example $ do
            parse (parseTypeP testCont) "" "Map[Rectangle, A]" `shouldParse` mapB rect a
            parse (parseTypeP testCont) "" "Map[List[Rectangle], List[Map[Any, List[A]]]]" 
                `shouldParse` mapB (lst rect) (lst (mapB any (lst a)))

        it "should fail on a mismatching argument list" $ example $ do
            parse (parseTypeP testCont) "" `shouldFailOn` "Map[Rectangle]"
            parse (parseTypeP testCont) "" `shouldFailOn` "Map[Rectangle, A, Shape]"

        it "should fail on unknown type of a kind (* -> *)" $ example $ do
            parse (parseTypeP testCont) "" `shouldFailOn` "List[F[A]]"
            parse (parseTypeP testCont) "" `shouldFailOn` "Rectangle[A]"

    describe "Declaration parsing test" $ do
        it "should parse basic declaration" $ example $
            parse (declarationP testCont) "" "class Some\n" `shouldParse` single "Some"
        
        it "should parse verbose version of basic declaration" $ example $ do
            let some = single "Some"
            (parse (declarationP testCont) "" "class Some extends Any\n") `shouldParseHierarchyOf` (single "Some")
            (parse (declarationP testCont) "" "class Some extends Rectangle\n") `shouldParseHierarchyOf` (some `extends` rect)

        it "should parse basic declaration with type params" $ example $
            parse (declarationP testCont) "" "class Hello[A, +B]\n" `shouldParse` comp "Hello" [inv a, cov b]
            
        it "should parse basic declaration with type params" $ example $
            parse (declarationP testCont) "" "class Hello[A, +B]\n" `shouldParse` comp "Hello" [inv a, cov b]

        it "should parse 'extends' declaration with type params" $ example $ do
            let t = buildType "Hello" [cov "A"] [shape, a] (mapB b c) 
            parse (declarationP testCont) "" "class Hello[+A] extends Map[Shape, A]\n" `shouldParseHierarchyOf` t

class CompareHierarchycally b where
    shouldParseHierarchyOf :: (HasCallStack, ShowErrorComponent e, Stream s) =>
        Either (ParseErrorBundle s e) Type -> b -> Expectation  

instance CompareHierarchycally Type where
    shouldParseHierarchyOf t1 t2 = (showHierarchy <$> t1) `shouldParse` (showHierarchy t2)

instance CompareHierarchycally (Either String Type) where
    shouldParseHierarchyOf t1 (Right t2) = t1 `shouldParseHierarchyOf` t2
    shouldParseHierarchyOf _  (Left e  ) = fail e