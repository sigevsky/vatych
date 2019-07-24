module TypeParser (
    nameP, varP, typeParamsP, bindingsP, parseTypeP, TypeDeclarations(..), Parser
) where

import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, letterChar, char, space, string)
import Data.Text hiding (any)
import Data.Void
import Data.Map (Map)
import qualified Data.Map as M
import Typed
import Prelude hiding (any)

-- Use custom error type to add more context to it
type Parser = Parsec String Text
newtype TypeDeclarations = TD (Map String Type)

nameP :: Parser String
nameP = (:) <$> upperChar <*> many letterChar

varP :: Parser Variance
varP = option Inv (fmap foo (char '+' <|> char '-'))
    where foo '+' = Cov
          foo '-' = Contr

paramListP :: Parser a -> Parser [a]
paramListP p = char '[' *> repeating <* char ']'
    where
        repeating = (:) <$> p <*> some (spacedComma *> p)
        spacedComma = space *> char ',' *> space


typeParamsP :: Parser [Param String]
typeParamsP = paramListP (TP <$> varP <*> nameP)

bindingsP :: TypeDeclarations -> Parser [Type]
bindingsP c = paramListP (parseTypeP c)

parseTypeP :: TypeDeclarations -> Parser Type
parseTypeP (TD c) = do
        n <- nameP -- replace with recursive type parsing like List[List[A]]
        case M.lookup n c of
            Just t -> pure t
            Nothing -> customFailure $ "Failed to find type " <> n <> " declaration"

instance ShowErrorComponent String where
    showErrorComponent = show