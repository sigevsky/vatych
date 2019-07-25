{-# LANGUAGE DeriveFunctor #-}

module Typed
    (  -- Reduce scope of Type(..)
        Variance(..), Param(..), Type(..), Substitutable, (==>), (<==),
        cov, inv, contr, single, comp, extends, anc, poly, any,
        lstHierarchy, showHierarchy, buildType, polysFromTypeParams,
        rewriteType, rewriteMap
    )
where

import           Data.List (intercalate, nub, (\\))
import           Data.Map (Map)
import           Data.Semigroup hiding (Any)
import qualified Data.Map as M

import           Prelude hiding (any) 

data Variance = Inv | Cov | Contr deriving (Eq, Show, Ord)

-- inv - 0 contr - (-1) cov - (+1)
instance Semigroup Variance where
    Contr <> Cov   = Contr
    Cov   <> Contr = Contr
    Contr <> Contr = Cov
    Cov   <> Cov   = Cov
    Inv   <> Inv   = Inv
    Contr <> Inv   = Inv
    Inv   <> Contr = Inv
    Cov   <> Inv   = Inv
    Inv   <> Cov   = Inv

data Param a = TP { var :: Variance, param :: a } deriving (Eq, Ord, Functor)

cov = TP Cov
inv = TP Inv
contr = TP Contr

instance (Show a) => Show (Param a) where
    show = show . param

data Type = Any
          | Poly String
          | Cp String [Param Type] Type deriving (Eq, Ord)

instance Show Type where
    show Any                 = "Any"
    show (Poly n) = n
    show (Cp n [] _) = n
    show (Cp n [f, s] _ ) = case param f of
            (Cp _ [_, _] _)  -> "(" <> show f <> ") " <> n <> " " <> show s
            _  -> show f <> " " <> n <> " " <> show s
    show (Cp n l _) = n <> "[" <> intercalate ", " (fmap show l) <> "]"

toPoly :: [Param String] -> [Param Type]
toPoly = (fmap . fmap) poly

buildType :: String -> [Param String] -> [Type] -> Type -> Either String Type
buildType name tparams [] Any = Right $ Cp name (toPoly tparams) Any
buildType _ _ _ Any = Left "Any doesnt has any holes, so no substitution can be performed"
buildType _ _ _ (Poly _) = Left "Poly type cannot be inherited from"
buildType name tparams [] (Cp _ [] _) = Right $ Cp name (toPoly tparams) Any
buildType _ _ _ (Cp _ [] _) = Left "Parent doesnt have any poly holes to perform binding with"
buildType name tparams xs p@(Cp _ al _) = Cp name (toPoly tparams) <$> rewrited
  where
    rewrited :: Either String Type = do
        m  <- rewriteMap xs al
        rewriteType p m `compliedWith` tparams
        
--check that builded parent type complies with generics constraints defined in type's parameter list
compliedWith :: Type -> [Param String] -> Either String Type
compliedWith t params = 
    case polysFromTypeParams t \\ params of
            []             -> Right t
            (TP rv tn : _) -> case M.lookup tn $ toMap params of
                Just bvar -> Left
                        $  "Variance mismatch of type " <> tn <> ". Declared " <> show bvar
                        <> " is not compatible with required " <> show rv
                Nothing   -> Left $  "Substitution poly type " <> tn <> " is not present in type's param list"
        where
            toMap :: [Param String] -> Map String Variance
            toMap [] = M.empty
            toMap (TP var n : xs) = M.insert n var $ toMap xs

rewriteType :: Type -> Map Type Type -> Type
rewriteType Any                 _ = Any
rewriteType a@(Poly _         ) _ = a
rewriteType (  Cp n params anc) m = Cp n (rewriteParams params) (rewriteType anc m)
  where
    rewriteParams :: [Param Type] -> [Param Type]
    rewriteParams []              = []
    rewriteParams (TP var t : xs) = case M.lookup t m of
                                      Just a  -> TP var a : rewriteParams xs
                                      Nothing -> rewriteParams xs

rewriteMap :: [Type] -> [Param Type] -> Either String (Map Type Type)
rewriteMap [] [] = Right M.empty 
rewriteMap [] _ = Left "Not all parent's type params were covered" 
rewriteMap _ [] = Left "Parent's type param length is less then substitution specifies" 
rewriteMap (nm : ns) (TP _ t : ts) = M.insert t nm <$> rewriteMap ns ts
            
-- extracts poly params from type param list i.e Foo[-C[-A], -P[-L[-B]]] -> [cov A, contr B] 
polysFromTypeParams :: Type -> [Param String]
polysFromTypeParams Any             = []
polysFromTypeParams (Poly _       ) = []
polysFromTypeParams (Cp _ params _) = nub $ acc params
  where
    acc :: [Param Type] -> [Param String]
    acc []                         = []
    acc (TP _          Any : _ ) = []
    acc (TP var (Poly n)   : xs) = TP var n : acc xs
    acc (TP var (Cp _ params _) : xs) = fmap (\(TP v n) -> TP (var <> v) n) (acc params) ++ acc xs

single :: String -> Type
single s = Cp s [] Any

comp :: String -> [Param Type] -> Type
comp s l = Cp s l Any

poly :: String -> Type
poly = Poly

any :: Type
any = Any

anc :: Type -> Type
anc (Cp _ _ an) = an
anc _ = Any

extends :: Type -> Type -> Either String Type
extends a Any = Right a
extends Any _ = Left "you cannot create surtype of Any"
extends _ (Poly _) = Left "you cannot extend from a hole"
extends (Poly _) _ = Left "you cannot extend hole from some type"
extends (Cp an al Any) b = Right $ Cp an al b  
extends t@(Cp _ _ par) b = if b == par then Right t else Left $ "Type" <> show t <> "already has a parent" <> show par

lstHierarchy :: Type -> Either String [String]
lstHierarchy Any            = Right ["Any"]
lstHierarchy (  Poly _    ) = Left "Cant deduce type hierarchy for a hole "
lstHierarchy t@(Cp _ _ anc) = fmap (\x -> show t : x) (lstHierarchy anc)

showHierarchy :: Type -> String
showHierarchy t = case lstHierarchy t of
    Left  s -> s
    Right l -> intercalate " -> " l

class Substitutable a where
    (==>) :: a -> a -> Bool
    (<==) :: a -> a -> Bool

    a ==> b = b <== a
    a <== b = b ==> a
    
instance Substitutable (Param Type) where
    (TP Inv   a) ==> (TP Inv   b) = a == b
    (TP Cov   a) ==> (TP Cov   b) = a ==> b
    (TP Contr a) ==> (TP Contr b) = b ==> a
    _            ==> _            = False

instance (Substitutable a) => Substitutable [a] where
    []       ==> []       = True
    _        ==> []       = False
    []       ==> _        = False
    (x : xs) ==> (y : ys) = x ==> y && xs ==> ys


instance Substitutable Type where
    Any      ==> _        = True
    _        ==> Any      = False
    _        ==> (Poly _) = False
    (Poly _) ==> _        = False
    a@(Cp ln lpar _) ==> (Cp rn rpar bnc) = (ln == rn && lpar ==> rpar) || a ==> bnc
