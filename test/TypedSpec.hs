module TypedSpec (spec) where

import           Test.Hspec
import           Typed
import           Prelude hiding (any)
import           TestTypeData

func1 = funcB rect rect
func2 = funcB func1 square
func3 = funcB square func1

spec :: Spec
spec = do
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
        
        it "should fail on non poly type's parameters passed" $ example $ do
          let
            foo = buildType "Some" [cov a, cov $ lst a, contr b] [] any
          foo `shouldBe` Left "List of param for the type should contain only polymorphic ones"

        it "should fail on type params lengths mismatch" $ example $ do
            let
              foo = funcB a b
            buildType "Partial" [contr c] [c] foo `shouldBe` Left "Not all parent's type params were covered"
            buildType "Partial" [contr c, cov a] [c, a, c] foo `shouldBe` Left "Parent's type param length is less then substitution specifies"

        it "should fail on mismatching variance" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [contr c] [c, c] foo
            im `shouldBe` Left "Variance mismatch of type C. Declared Contr is not compatible with required Cov"
    
        it "should fail on unspecified substitution parameter" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [contr c] [c, a] foo
            im `shouldBe` Left "Substitution poly type A is not present in type's param list"

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
            polysFromTypeParams c1 `shouldBe` [cov a]

            show c2 `shouldBe` "C[P[I[P[A]]]]"
            polysFromTypeParams c2 `shouldBe` [inv a]

            show c3 `shouldBe` "P[P[C[P[A]]]]"
            polysFromTypeParams c3 `shouldBe` [contr a]

            show c4 `shouldBe` "P[C[C[P[A]]]]"
            polysFromTypeParams c4 `shouldBe` [cov a]

        it "should return double enterance" $ example $ do
            let
              foo = funcB (lst a) a
            polysFromTypeParams foo `shouldBe` [contr a, cov a]

        it "should return double enterance" $ example $ do
          let
            c t = comp "C" [contr t]
            p t = comp "P" [cov t]
            cps = funcB (p $ c $ funcB a (c b)) (c $ c b)
          show cps `shouldBe` "P[C[A => C[B]]] => C[C[B]]"
          polysFromTypeParams cps `shouldBe` [contr a, contr b, cov b]

        it "should fail putting covariant type in contravariant position" $ example $ do
            let
              foo = funcB a b
              im  = buildType "Partial" [cov a] [lst a, a] foo
            im `shouldBe` Left "Variance mismatch of type A. Declared Cov is not compatible with required Contr"