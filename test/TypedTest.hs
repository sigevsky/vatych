{-# LANGUAGE LambdaCase #-}

import           Test.Hspec
import           System.IO.Unsafe
import           Typed
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

unsafeEither :: Either String b -> b
unsafeEither (Left a) = unsafePerformIO . fail $ "Failed to prepare test data: " <> a
unsafeEither (Right a) = a

--Unsafe hierarchy
square, rect, shape :: Type
[square, rect, shape] = unsafeEither $ do
    shape  <- Right $ single "Shape"
    rect   <- single "Rectangle" `extends` shape
    square <- single "Square" `extends` rect
    return [square, rect, shape]

-- Func examples
funcB :: Type -> Type -> Type
funcB a b = comp "=>" [contr a, cov b]


func1 = funcB rect rect
func2 = funcB func1 square
func3 = funcB square func1

main :: IO ()
main = hspec $ do
    describe "Basic type capabilities" $ do
        it "should print types in infix form" $ example $ do
            show func1  `shouldBe` "Rectangle => Rectangle"
            show func2  `shouldBe` "(Rectangle => Rectangle) => Square"
            show func3  `shouldBe` "Square => Rectangle => Rectangle"

        it "should print types in postfix form" $ example $ do
            show (single "Number")  `shouldBe` "Number"
            show (comp "List" [cov square])  `shouldBe` "List[Square]"
            show (comp "Triple" [cov square, contr shape, inv rect])  `shouldBe` "Triple[Square, Shape, Rectangle]"

    describe "Hierarchy check" $ do
        it "should print hierarchy of a square" $ example $
            showHierarchy square  `shouldBe` "Square -> Rectangle -> Shape -> Any"
        
        it "should match ancestor" $ example $
            anc rect `shouldBe` shape

    describe "Polyless inheritance check" $ do
        it "should check inheritance of basic example" $ example $ do
            rect   `parentOf` square  `shouldBe` True
            shape  `parentOf` square  `shouldBe` True
            square `parentOf` shape  `shouldBe` False

        it "should pass covariance check" $ example $ do
            let 
              listOfShapes = comp "List" [cov shape]
              listOfSquares = comp "List" [cov shape]
            listOfShapes `parentOf` listOfSquares `shouldBe` True

        it "wided function should be subtyped properly" $ example $ do
            let 
                func1 = funcB rect rect
                func2 = funcB shape square
            func1 `parentOf` func2 `shouldBe` True
            func2 `parentOf` func1 `shouldBe` False

        it "nested function should be subtyped properly" $ example $ do
            let 
                func = funcB rect rect
                funcN1 = funcB func1 shape
                funcN2 = funcB (funcB square shape) square
            funcN1 `parentOf` funcN2 `shouldBe` True
            funcN2 `parentOf` funcN1 `shouldBe` False

    -- describe "Inner machinery test" $ do
    --     it "should add list to map" $ example $ do
    --         let m = insertList [("beautiful", 2), ("World", 3), ("Hello", 4)] $ M.singleton "Hello" 1
    --         M.size m `shouldBe` 3
    --         M.lookup "Hello" m `shouldBe` Just 4
    --         M.lookup "beautiful" m `shouldBe` Just 2
    --         M.lookup "World" m `shouldBe` Just 3

    --     it "should return unmatched hole E [+A, -B=R, -D=Q, F=E] unf [+R, -Q]" $ example $ do
    --         let parentPolys = [cov "R", contr "Q"]
    --             childrenBinds  = M.fromList [("R", Cov), ("Q", Contr), ("E", Inv)] 
    --         unmatchedPoly parentPolys childrenBinds `shouldBe` Right (M.singleton "E" Inv)
    
    --     it "should match all holes in [+A, +B=[P, R], -D=Q] unf [+P, +R, -Q]" $ example $ do
    --         let parentPolys = [cov "P", cov "R", contr "Q"]
    --             childrenBinds   = M.fromList [("P", Cov), ("R", Cov), ("Q", Contr)] 
    --         unmatchedPoly parentPolys childrenBinds `shouldBe` Right M.empty
    
    --     -- Fail fast on unexisting binding to parents' poly hole
    --     it "should fail on missing P hole in [+A, +B=R, -D=Q] unf [+P, +R, -Q]" $ example $ do
    --         let parentPolys = [cov "P", cov "R", contr "Q"]
    --             childrenBinds   = M.fromList [("R", Cov), ("Q", Contr)] 
    --         unmatchedPoly parentPolys childrenBinds `shouldBe` Left "Unbinded poly hole P in parent list"

    --     it "should fail on unvalid P hole in [+A, +B=[?E, R], -D=Q] unf [+P, +R, -Q]" $ example $ do
    --         let parentPolys = [cov "P", cov "R", contr "Q"]
    --             childrenBinds   = M.fromList [("E", Cov), ("R", Cov), ("Q", Contr)] 
    --         unmatchedPoly parentPolys childrenBinds `shouldBe` Left "Unbinded poly hole P in parent list"

    --     it "should fail on inappr covariance type in [+A, -B=R, -D=Q] unf [+R, -Q]" $ example $ do
    --         let parentPolys = [cov "R", contr "Q"]
    --             childrenBinds  = M.fromList [("R", Contr), ("Q", Contr)] 
    --         unmatchedPoly parentPolys childrenBinds `shouldBe` Left "Poly hole R has incompatible variance Contr with Cov"

    -- describe "Type building test" $ do
    --     it "should create List[String] impl Some[A, String]" $ example $ do

            -- let lhs = [cov $ Free "A", cov $ Binded "B" ["P", "R"], contr $ Binded "D" ["Q"]]