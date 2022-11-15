module Parser (parse) where

import Expr (Expr (..))
import Relude
import Text.Parsec (ParseError, ParsecT, alphaNum, letter, oneOf)
import qualified Text.Parsec as Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

type Parser = ParsecT Text Int Identity

parse :: Text -> Either ParseError Expr
parse = Parsec.runParser expr 1 ""

expr :: Parser Expr
expr = lit <|> plus <|> times

lit :: Parser Expr
lit = Lit <$> integer

plus :: Parser Expr
plus = Plus <$> (reservedOp "+" *> expr) <*> expr

times :: Parser Expr
times = Times <$> (reservedOp "*" *> expr) <*> expr

lexer :: P.GenTokenParser Text u Identity
lexer = P.makeTokenParser language

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

integer :: Parser Integer
integer = P.integer lexer

language :: P.GenLanguageDef Text st Identity
language =
    emptyDef
        { P.commentStart = "{-"
        , P.commentEnd = "-}"
        , P.commentLine = "--"
        , P.nestedComments = True
        , P.identStart = letter
        , P.identLetter = alphaNum <|> oneOf "_'"
        , P.opStart = P.opLetter language
        , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedOpNames = ["+", "*"]
        , P.reservedNames = []
        , P.caseSensitive = True
        }
