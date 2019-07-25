module TestTypeData where

import           Typed
import           System.IO.Unsafe
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

unsafeEither :: Either String b -> b
unsafeEither (Left a) = unsafePerformIO . fail $ "Failed to prepare test data: " <> a
unsafeEither (Right a) = a

showE :: (Show a) => Either String a -> String
showE = either id show

showEstr :: Either String String -> String
showEstr = either id id

--Unsafe hierarchy
square, rect, shape :: Type
[square, rect, shape] = unsafeEither $ do
    shape  <- Right $ single "Shape"
    rect   <- single "Rectangle" `extends` shape
    square <- single "Square" `extends` rect
    return [square, rect, shape]

a, b, c, d :: Type
[a, b, c, d, k, v] = poly <$> ["A", "B", "C", "D", "K", "V"]

-- Func examples
funcB :: Type -> Type -> Type
funcB a b = comp "=>" [contr a, cov b]

mapB :: Type -> Type -> Type
mapB a b = comp "Map" [inv a, cov b]

lst :: Type -> Type
lst a = comp "List" [cov a]