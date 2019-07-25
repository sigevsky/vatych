module TypeParser (
    nameP, varP, typeParamsP, bindingsP, parseTypeP, TypeDeclarations(..), Parser, paramListP
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
        repeating = (:) <$> p <*> many (spacedComma *> p)
        spacedComma = space *> char ',' *> space


typeParamsP :: Parser [Param String]
typeParamsP = paramListP (TP <$> varP <*> nameP)

bindingsP :: TypeDeclarations -> Parser [Type]
bindingsP c = paramListP (parseTypeP c)

parseTypeP :: TypeDeclarations -> Parser Type
parseTypeP dc@(TD c) = do
        n <- nameP -- replace with recursive type parsing like List[List[A]]
        case M.lookup n c of
                 Nothing -> poly n <$ notFollowedBy lSquareBracket
                 Just Any  -> pure any
                 Just a@(Cp _ [] _) -> a <$ notFollowedBy lSquareBracket
                 Just a@(Cp _ params _) -> do 
                    types <- paramListP (parseTypeP dc)
                    rm <- either customFailure pure $ rewriteMap types params
                    return $ rewriteType a rm
    where
        lSquareBracket :: Parser Char = char '['
        

instance ShowErrorComponent String where
    showErrorComponent = show