{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Day21 (part1, part2) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Util (parseOrError)

type Name = ByteString

data Operation = Add | Sub | Mul | Div

data Expression
  = Constant Int
  | Operation Name Operation Name

parseLine :: ByteString -> (Name, Expression)
parseLine = parseOrError $ do
  exprName <- name
  _ <- P.string ": "
  expr <- constant <|> operation
  return (exprName, expr)
  where
    name = P.take 4

    constant =
      Constant <$> P.decimal
    operation =
      Operation
        <$> name
        <*> ( P.take 3 <&> \case
                " + " -> Add
                " - " -> Sub
                " / " -> Div
                " * " -> Mul
                s -> error ("Unknown operation: " ++ BS.unpack s)
            )
        <*> name

type Env = Map Name

evalExpression :: Env Int -> Expression -> Int
evalExpression _ (Constant n) = n
evalExpression env (Operation l op r) = evalOp op (env ! l) (env ! r)

evalOp :: Integral a => Operation -> a -> a -> a
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div

evalExpressions :: [(Name, Expression)] -> Env Int
evalExpressions expressionList =
  -- God bless laziness
  let env = Map.fromList $ map (second (evalExpression env)) expressionList
   in env

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map parseLine
    & evalExpressions
    & (! "root")

data SymbolicExpression
  = SOperation SymbolicExpression Operation SymbolicExpression
  | Unknown
  | Concrete Int

pattern a :+: b = SOperation a Add b

pattern a :*: b = SOperation a Mul b

pattern a :-: b = SOperation a Sub b

pattern a :/: b = SOperation a Div b

{-# COMPLETE Unknown, Concrete, (:+:), (:*:), (:-:), (:/:) #-}

formSymbolicExpressions :: [(Name, Expression)] -> Env SymbolicExpression
formSymbolicExpressions expressionList =
  let env =
        Map.fromList $
          map
            ( \case
                ("humn", _) -> ("humn", Unknown)
                (name, Constant n) -> (name, Concrete n)
                (name, Operation l op r) -> (name, SOperation (env ! l) op (env ! r))
            )
            expressionList
   in env

simplify :: SymbolicExpression -> SymbolicExpression
simplify Unknown = Unknown
simplify (Concrete n) = Concrete n
simplify (SOperation l op r) =
  case (simplify l, op, simplify r) of
    (Concrete lv, _, Concrete rv) -> Concrete (evalOp op lv rv)
    (l', _, r') -> SOperation l' op r'

-- | Find value that the unknown in the expression on the left needs to take to make the expression
-- take the value on the right
($=) :: SymbolicExpression -> Int -> Int
(l :+: (Concrete x)) $= y = l $= (y - x)
((Concrete x) :+: r) $= y = r $= (y - x)
(l :*: (Concrete x)) $= y = l $= (y `div` x)
((Concrete x) :*: r) $= y = r $= (y `div` x)
(l :-: (Concrete x)) $= y = l $= (y + x)
((Concrete x) :-: r) $= y = r $= (x - y)
(l :/: (Concrete x)) $= y = l $= (y * x)
((Concrete x) :/: r) $= y = r $= (x `div` y)
Unknown $= y = y
SOperation {} $= _ = error "Cannot proceed"
Concrete {} $= _ = error "No unknowns"

part2 :: ByteString -> Int
part2 input =
  BS.lines input
    & map parseLine
    & formSymbolicExpressions
    & Map.map simplify
    & ( \env ->
          let (SOperation e1 _ e2) = env ! "root"
           in case (simplify e1, simplify e2) of
                (Concrete y, e) -> e $= y
                (e, Concrete y) -> e $= y
                _ -> error "More than one unknown"
      )
