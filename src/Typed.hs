module Typed
    ( 
        Variance(..), Param, Type, Substitutable, (==>), (<==),
        cov, inv, contr, anc, single, comp, extends,
        lstHierarchy, showHierarchy,
    )
where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M

data Variance = Inv | Cov | Contr deriving (Eq, Show)

data Param a = TP { var :: Variance, param :: a } deriving Eq

cov = TP Cov
inv = TP Inv
contr = TP Contr

instance (Show a) => Show (Param a) where
    show = show . param

data Type = Any
          | Poly String
          | Cp String [Param Type] Type deriving Eq

instance Show Type where
    show Any                 = "Any"
    show (Poly n) = n
    show (Cp n [] _) = n
    show (Cp n [f, s] _ ) = case param f of
            (Cp _ [] _)  -> show f <> " " <> n <> " " <> show s
            (Poly _   ) -> show f <> " " <> n <> " " <> show s
            _            -> "(" <> show f <> ") " <> n <> " " <> show s
    show (Cp n l _) = n <> "[" <> intercalate ", " (fmap show l) <> "]"

buildType :: String -> [Param Type] -> [Type] -> Type -> Either String Type
buildType name tparams [] Any = Right $ comp name tparams

-- get poly types
polyNames :: [Param Type] -> Either String [Param String]
polyNames [] = Right []
polyNames (TP var (Poly n) : xs) = (\ys -> TP var n : ys) <$> polyNames xs
polyNames _ = Left "List of param types for type should contain only poly params"

mappedPolyNames :: [Param String] -> Map String Variance
mappedPolyNames l = M.fromList $ fmap (\case (TP var name) -> (name, var)) l

-- rewriteMap :: Map String Variance -> [Type] -> [Param Type] -> Either String (Map Type Type)
-- rewriteMap lparams [] [] = Right M.empty 
-- rewriteMap lparams [] x = Left "Not all parent params were covered" 
-- rewriteMap lparams x [] = Left "Parameter has less type params then substitution specifies" 
-- rewriteMap lparams (nm : ns) (TP yvar t : ts) | isValidSubst = _ -- M.insert t nm <$> rewriteMap lparams ns ts  -- xvar == yvar = M.insert nm t
--             where isValidSubst = case nm of 
--                                     Poly pn -> case M.lookup pn lparams of 
--                                                 Just var  -> var == yvar
--                                                 Nothing   -> False
--                                     _       -> True  


anc :: Type -> Type
anc (Cp _ _ a) = a
anc _          = Any

single :: String -> Type
single s = Cp s [] Any

comp :: String -> [Param Type] -> Type
comp s l = Cp s l Any

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
    x        ==> []       = False
    []       ==> x        = False
    (x : xs) ==> (y : ys) = x ==> y && xs ==> ys


instance Substitutable Type where
    Any      ==> _        = True
    _        ==> Any      = False
    _        ==> (Poly _) = False
    (Poly _) ==> _        = False
    a@(Cp ln lpar _) ==> (Cp rn rpar bnc) = (ln == rn && lpar ==> rpar) || a ==> bnc
