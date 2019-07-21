module Typed
    ( 
        Variance(..), Param, Type,
        cov, inv, contr, anc, single, comp, extends,
        parentOf, lstHierarchy, showHierarchy,
        -- unmatchedPoly, insertList
    )
where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M

data Variance = Inv | Cov | Contr deriving (Eq, Show)

data Param a = TP { var :: Variance, param :: a }

cov = TP Cov
inv = TP Inv
contr = TP Contr

instance (Show a) => Show (Param a) where
    show = show . param

instance Eq (Param Type) where
    (TP Inv   a) == (TP Inv   b) = a == b
    (TP Cov   a) == (TP Cov   b) = a `parentOf` b
    (TP Contr a) == (TP Contr b) = b `parentOf` a
    _            == _            = False

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

-- buildType :: String -> [Param Type] -> Type -> Either String Type
-- buildType name tparams Any = Right $ comp name tparams
-- buildType _ _ (Hole _) = Left "Cannot build type from parent as a hole"
-- buildType name tparams p@(Cp _ rp _) = case e of 
--                                         Right [] -> Right $ Cp name tparams p
--                                         Right l  -> Left  $ "Following binding were not found at parent poly list" <> intercalate ", " (fmap (\(h, var) -> show var <> " " <> h) l)
--                                         Left  e  -> Left  e
--                 where
--                     (_ , lBindedPoly) = distPolyParams tparams
--                     lbinded = toBindedMap lBindedPoly
--                     rPoly = (\case (TP var (Free n))     -> TP var n
--                                    (TP var (Binded n _)) -> TP var n) <$> polyParams rp
--                     e = M.toList <$> unmatchedPoly rPoly lbinded
                    

-- -- parent poly params minus children ones
-- unmatchedPoly :: [Param String] -> Map String Variance -> Either String (Map String Variance)
-- unmatchedPoly [] m = Right m
-- unmatchedPoly (TP pvar n : xs) m = case M.lookup n m of
--         Just var | var == pvar -> unmatchedPoly xs $ M.delete n m
--         Just var -> Left $ "Poly hole " <> n <> " has incompatible variance " <> show var <> " with " <> show pvar
--         Nothing  -> Left $ "Unbinded poly hole " <> n <> " in parent list"

-- -- Returns map of parent's poly variables declared to be binded by a child
-- toBindedMap :: [Param Poly] -> Map String Variance
-- toBindedMap [] = M.empty
-- toBindedMap (TP var (Binded _ to): xs) = insertList (fmap (, var) to) (toBindedMap xs) 
-- toBindedMap (_ : xs) = toBindedMap xs

-- insertList :: Ord k => [(k, v)] -> Map k v -> Map k v
-- insertList [] m = m
-- insertList ((k, v) : xs) m = M.insert k v (insertList xs m)

-- -- Returns set of polymorphic params undistinctively TODO: rewrite in more effective way
-- polyParams :: [Param Type] -> [Param Poly]
-- polyParams l = a ++ b
--             where
--                (a, b) = distPolyParams l

-- -- Returns set of binded and unbinded polymorphic params for specific type
-- distPolyParams :: [Param Type] -> ([Param Poly], [Param Poly])
-- distPolyParams  = foldl reducePolyParams ([], [])


-- reducePolyParams :: ([Param Poly], [Param Poly]) -> Param Type -> ([Param Poly], [Param Poly])
-- reducePolyParams (a , b) tp = case param tp of 
--                        (Hole h@(Free _)) -> (TP (var tp) h : a, b) 
--                        (Hole h@(Binded _ _)) -> (a, TP (var tp) h : b)
--                        _ -> (a, b)

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

parentOf :: Type -> Type -> Bool
parentOf Any      _              = True
parentOf _        Any            = False
parentOf _        (Poly _)       = False
parentOf (Poly _) _              = False
parentOf a@Cp{}   b@(Cp _ _ bnc) = a == b || parentOf a bnc

lstHierarchy :: Type -> Either String [String]
lstHierarchy Any            = Right ["Any"]
lstHierarchy (  Poly _    ) = Left "Cant deduce type hierarchy for a hole "
lstHierarchy t@(Cp _ _ anc) = fmap (\x -> show t : x) (lstHierarchy anc)

showHierarchy :: Type -> String
showHierarchy t = case lstHierarchy t of
    Left  s -> s
    Right l -> intercalate " -> " l
