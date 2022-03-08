-- | This module defines helper functions for defining parsers using operator tables.

module StringParser.Expr
  ( Assoc(..)
  , Operator(..)
  , OperatorTable()
  , buildExprParser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldl, foldr)
import Data.List (List(..))
import StringParser.Parser (Parser)
import StringParser.Combinators (choice, (<?>))

-- | Operator associativity types.
data Assoc = AssocNone | AssocLeft | AssocRight

-- | Operator types: infix, prefix, postfix.
data Operator a
  = Infix (Parser (a -> a -> a)) Assoc
  | Prefix (Parser (a -> a))
  | Postfix (Parser (a -> a))

-- | An operator table arranges operators into precedence groups.
type OperatorTable a = Array (Array (Operator a))

type SplitAccum a =
  { rassoc :: List (Parser (a -> a -> a))
  , lassoc :: List (Parser (a -> a -> a))
  , nassoc :: List (Parser (a -> a -> a))
  , prefix :: List (Parser (a -> a))
  , postfix :: List (Parser (a -> a))
  }

buildExprParser :: forall a. OperatorTable a -> Parser a -> Parser a
buildExprParser operators simpleExpr = do
  let
    makeParser term ops =
      let
        accum = foldr splitOp { rassoc: Nil, lassoc: Nil, nassoc: Nil, prefix: Nil, postfix: Nil } ops

        rassocOp = choice accum.rassoc
        lassocOp = choice accum.lassoc
        nassocOp = choice accum.nassoc
        prefixOp = choice accum.prefix <?> ""
        postfixOp = choice accum.postfix <?> ""

        postfixP = postfixOp <|> pure identity
        prefixP = prefixOp <|> pure identity
      in
        do
          x <- termP prefixP term postfixP
          rassocP x rassocOp prefixP term postfixP
            <|> lassocP x lassocOp prefixP term postfixP
            <|> nassocP x nassocOp prefixP term postfixP
            <|> pure x
              <?> "operator"

    splitOp :: forall b. Operator b -> SplitAccum b -> SplitAccum b
    splitOp (Infix op AssocNone) accum = accum { nassoc = Cons op accum.nassoc }
    splitOp (Infix op AssocLeft) accum = accum { lassoc = Cons op accum.lassoc }
    splitOp (Infix op AssocRight) accum = accum { rassoc = Cons op accum.rassoc }
    splitOp (Prefix op) accum = accum { prefix = Cons op accum.prefix }
    splitOp (Postfix op) accum = accum { postfix = Cons op accum.postfix }

    rassocP :: forall b c d. b -> Parser (b -> b -> b) -> Parser (c -> d) -> Parser c -> Parser (d -> b) -> Parser b
    rassocP x rassocOp prefixP term postfixP = do
      f <- rassocOp
      y <- do
        z <- termP prefixP term postfixP
        rassocP1 z rassocOp prefixP term postfixP
      pure (f x y)

    rassocP1 :: forall b c d. b -> Parser (b -> b -> b) -> Parser (c -> d) -> Parser c -> Parser (d -> b) -> Parser b
    rassocP1 x rassocOp prefixP term postfixP = rassocP x rassocOp prefixP term postfixP <|> pure x

    lassocP :: forall b c d. b -> Parser (b -> b -> b) -> Parser (c -> d) -> Parser c -> Parser (d -> b) -> Parser b
    lassocP x lassocOp prefixP term postfixP = do
      f <- lassocOp
      y <- termP prefixP term postfixP
      lassocP1 (f x y) lassocOp prefixP term postfixP

    lassocP1 :: forall b c d. b -> Parser (b -> b -> b) -> Parser (c -> d) -> Parser c -> Parser (d -> b) -> Parser b
    lassocP1 x lassocOp prefixP term postfixP = lassocP x lassocOp prefixP term postfixP <|> pure x

    nassocP :: forall b c d. b -> Parser (b -> b -> b) -> Parser (c -> d) -> Parser c -> Parser (d -> b) -> Parser b
    nassocP x nassocOp prefixP term postfixP = do
      f <- nassocOp
      y <- termP prefixP term postfixP
      pure (f x y)

    termP :: forall b c d. Parser (b -> c) -> Parser b -> Parser (c -> d) -> Parser d
    termP prefixP term postfixP = do
      pre <- prefixP
      x <- term
      post <- postfixP
      pure (post (pre x))

  foldl (makeParser) simpleExpr operators
