{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day21 (part1, part2) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Util (parseOrError)

type Name = ByteString

data Operation = Add | Sub | Mul | Div

data Expression n
  = Constant Int
  | Operation n Operation n
  deriving (Functor)

parseLine :: ByteString -> (Name, Expression Name)
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

evalExpression :: Expression Int -> Int
evalExpression (Constant n) = n
evalExpression (Operation l op r) = evalOp op l r

evalOp :: Integral a => Operation -> a -> a -> a
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div

evalExpressions :: [(Name, Expression Name)] -> Env Int
evalExpressions expressionList =
  -- God bless laziness
  let env = Map.fromList $ map (second (evalExpression . fmap (env Map.!))) expressionList
   in env

part1 :: ByteString -> Int
part1 input =
  BS.lines input
    & map parseLine
    & evalExpressions
    & (Map.! "root")

data SymbolicExpression
  = SOperation SymbolicExpression Operation SymbolicExpression
  | Unknown
  | Concrete Int

formSymbolicExpressions :: [(Name, Expression Name)] -> Env SymbolicExpression
formSymbolicExpressions expressionList =
  let env =
        Map.fromList $
          map
            ( \case
                ("humn", _) -> ("humn", Unknown)
                (name, Constant n) -> (name, Concrete n)
                (name, Operation l op r) -> (name, SOperation (env Map.! l) op (env Map.! r))
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

solve :: SymbolicExpression -> SymbolicExpression -> Maybe Int
-- a # b == C
solve (SOperation l Add (Concrete x)) (Concrete y) = l `solve` Concrete (y - x)
solve (SOperation l Sub (Concrete x)) (Concrete y) = l `solve` Concrete (y + x)
solve (SOperation (Concrete x) Sub r) (Concrete y) = r `solve` Concrete (x - y)
solve (SOperation l Mul (Concrete x)) (Concrete y) = l `solve` Concrete (y `div` x)
solve (SOperation l Div (Concrete x)) (Concrete y) = l `solve` Concrete (y * x)
solve (SOperation (Concrete x) Div r) (Concrete y) = r `solve` Concrete (x `div` y)
solve (SOperation (Concrete x) op r) (Concrete y) = SOperation r op (Concrete x) `solve` Concrete y
solve l@Concrete {} r@SOperation {} = r `solve` l
-- x == C
solve Unknown (Concrete n) = Just n
solve (Concrete n) Unknown = Just n
-- X == X
solve Unknown Unknown = trace "Cannot solve x=x" Nothing
-- a # b == c # d
solve l r = case (simplify l, simplify r) of
  (SOperation {}, SOperation {}) -> trace "Two unknowns" Nothing
  (l', r') -> l' `solve` r'

part2 :: ByteString -> Int
part2 input =
  BS.lines input
    & map parseLine
    & formSymbolicExpressions
    & Map.map simplify
    & ( \env ->
          let (SOperation e1 _ e2) = env Map.! "root"
           in fromJust (solve e1 e2)
      )
