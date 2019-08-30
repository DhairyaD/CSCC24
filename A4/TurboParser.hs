module TurboParser where

import Control.Applicative

import ParserLib
import TurboDef
import Data.Char
import Data.Functor
import Data.List

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Stmt)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans


stmt :: Parser Stmt
stmt = assign <|> penCmd <|> turn <|> forward <|> forLoop <|> seq
    where
        -- Derived our own "double"
        dblParser :: Parser String
        dblParser = (some (satisfy isDigit)) <* whitespaces

        small_lit = fmap (\a -> RLit(read a)) dblParser

        big_lit = do
                    num1 <- dblParser
                    period <- char '.'
                    num2 <- dblParser
                    structure <- return (num1 ++ "." ++ num2)
                    my_double <- return (read structure :: Double)
                    return (RLit my_double)

        -- Implementation of literal (2 cases shown above)
        literal = big_lit <|> small_lit

        -- Basic assignment of keywords
        var = identifier ["pendown", "penup", "turn", "forward", "for", "to"]

        -- Implementation of assign
        assign = do
            myVar <- var
            terminal "="
            myExprAssign <- expr
            return (myVar := myExprAssign)

        -- Implementation of penCmd
        penCmd = keyword "penup" *> whitespaces *> return (PenUp) <|>
                 keyword "pendown" *> whitespaces *> return (PenDown)

        -- Implementation of turn
        turn = do
            keyword "turn"
            my_expr_turn <- expr
            return (Turn my_expr_turn)

        -- Implementation of forward
        forward = do
            keyword "forward"
            myExprForward <- expr
            return (Forward myExprForward)

        -- Implementation of forloop
        forLoop = do
            keyword "for"
            myVarLoop <- var
            terminal "="
            myExprLoopOne <- expr
            keyword "to"
            myExprLoopTwo <- expr
            mySeq <- seq
            -- The line below is to remove the list of stmt from seq to return
            case mySeq of Seq list -> return (For myVarLoop myExprLoopOne myExprLoopTwo list)

        -- Implementation of seq
        seq = do
            terminal "{"
            manySeqExpr <- some seqHelper
            terminal "}"
            return (Seq manySeqExpr)
            where
                -- This helper function for seq checks for multiple statements seperated by ;
                seqHelper = do
                    someSeqExpr <- stmt
                    terminal ";"
                    return someSeqExpr

        -- Since our var doesn't convert the keyword to a RealExpr, this helper function will
        my_var_expr = do
            my_var <- var
            return (RVar my_var)

        -- Implementation of expr
        expr = adds
            where
                adds = chainl1 muls (operator "+" *> whitespaces *> pure (:+) <|> operator "-" *> pure (:-))
                muls = chainl1 negs (operator "*" *> whitespaces *> pure (:*) <|> operator "/" *> pure (:/))
                negs = operator "-" *> whitespaces *> (fmap Neg negs) <|> atom
                atom = literal <|> my_var_expr <|> between (char '(' *> whitespaces) (char ')' *> whitespaces) adds

        -- Implementation of op
        op = chainl1 literal (char '+' *> pure (:+)) <|>
             chainl1 literal (char '-' *> pure (:-)) <|>
             chainl1 literal (char '*' *> pure (:*)) <|>
             chainl1 literal (char '/' *> pure (:/))


mainParser :: Parser Stmt
mainParser = whitespaces *> stmt <* eof
