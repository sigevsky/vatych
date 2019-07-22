import           Test.Hspec
import           System.IO.Unsafe
import           Typed
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import Prelude hiding (any)

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

a, b, c, d :: Type
[a, b, c, d] = poly <$> ["A", "B", "C", "D"]

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
            rect   ==> square  `shouldBe` True
            shape  ==> square  `shouldBe` True
            square ==> shape  `shouldBe` False

        it "should not pass invaiance check" $ example $ do
            let 
              listR = comp "MutableList" [inv rect]
              listSh = comp "MutableList" [inv shape]
              listSq = comp "MutableList" [inv square]
            listR   ==> listR  `shouldBe` True
            listSh  ==> listR  `shouldBe` False
            listR  ==> listSq  `shouldBe` False

        it "should pass covariance check" $ example $ do
            let 
              listOfShapes = comp "List" [cov shape]
              listOfSquares = comp "List" [cov shape]
            listOfShapes ==> listOfSquares `shouldBe` True

        it "wided function should be subtyped properly" $ example $ do
            let 
                func1 = funcB rect rect
                func2 = funcB shape square
            func1 ==> func2 `shouldBe` True
            func2 ==> func1 `shouldBe` False

        it "nested function should be subtyped properly" $ example $ do
            let 
                func = funcB rect rect
                funcN1 = funcB func1 shape
                funcN2 = funcB (funcB square shape) square
            funcN1 ==> funcN2 `shouldBe` True
            funcN2 ==> funcN1 `shouldBe` False

    describe "Type building tests" $ do
        it "should create parameterless base type" $ example $ do
            let
              t = buildType "Circle" [] [] shape
            showE t `shouldBe` "Circle"
        
        it "should create base polymorphic function" $ example $ do
            let
              foo = buildType "=>" [contr a, cov b] [] any
            showE foo `shouldBe` "A => B"
        
        it "should partially subtype poly function" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [contr c] [c, rect] foo
            showEstr (showHierarchy <$> im) `shouldBe` "Partial[C] -> C => Rectangle -> Any"   
        
        it "should fail on type params lengths mismatch" $ example $ do
            let
              foo = funcB a b
            buildType "Partial" [contr c] [c] foo `shouldBe` Left "Not all parent's type params were covered"
            buildType "Partial" [contr c, cov a] [c, a, c] foo `shouldBe` Left "Parent's type param length is less then substitution specifies"

        it "should fail on mismatching variance" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [contr c] [c, c] foo
            im `shouldBe` Left "Mismatching varience in the substitution Contr C -> Cov B"
    
        it "should fail on unspecified substitution parameter" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [contr c] [c, a] foo
            im `shouldBe` Left "Cant substitute, A was not found in type's param list"

        it "should return poly type's variance position" $ example $ do
            let
              c t = comp "C" [contr t]
              p t = comp "P" [cov t]
              i t = comp "I" [inv t]
              c1 = p $ p $ p a
              c2 = c $ p $ i $ p a
              c3 = p $ p $ c $ p a
              c4 = p $ c $ c $ p a
            show c1 `shouldBe` "P[P[P[A]]]"
            polyPosition a c1 `shouldBe` [Cov]

            show c2 `shouldBe` "C[P[I[P[A]]]]"
            polyPosition a c2 `shouldBe` [Inv]

            show c3 `shouldBe` "P[P[C[P[A]]]]"
            polyPosition a c3 `shouldBe` [Contr]

            show c4 `shouldBe` "P[C[C[P[A]]]]"
            polyPosition a c4 `shouldBe` [Cov]

        it "should return double enterance" $ example $ do
            let
              lst = comp "GenList" [cov a]
              foo = funcB lst a
            polyPosition a foo `shouldBe` [Contr, Cov] -- TODO impl

        it "!!!!!!should fail putting covariant in contravariant position" $ example $ do
            let
              foo = funcB a b
              lst = comp "GenList" [cov a]
              im  = buildType "Partial" [cov a] [lst, a] foo
            im `shouldBe` Left []

showE :: (Show a) => Either String a -> String
showE = either id show

showEstr :: Either String String -> String
showEstr = either id id