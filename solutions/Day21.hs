{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day21 (part1, part2) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
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
import GHC.Generics (Generic)
import Util (parseOrError)

type Name = ByteString

data Operation = Add | Sub | Mul | Div

data Expression
  = Constant Int
  | Operation Name Operation Name

instance Show Expression where
  show (Constant n) = show n
  show (Operation l _ r) = BS.unpack (l <> " # " <> r)

parseLine :: ByteString -> (Name, Expression)
parseLine = parseOrError $ do
  exprName <- name
  _ <- P.string ": "
  expr <- constant <|> operation
  return (exprName, expr)
  where
    name = P.take 4

    constant = Constant <$> P.decimal
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
evalExpression env (Operation l op r) = evalOp op (env Map.! l) (env Map.! r)

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
    & (Map.! "root")

data SymbolicExpression
  = SOperation SymbolicExpression Operation SymbolicExpression
  | Unknown
  | Concrete Int

instance Show Operation where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show SymbolicExpression where
  show (Concrete n) = show n
  show Unknown = "??"
  show (SOperation l op r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

formSymbolicExpressions :: [(Name, Either SymbolicExpression Expression)] -> Env SymbolicExpression
formSymbolicExpressions expressionList =
  let env = Map.fromList $ map (second resolveExpression) expressionList

      resolveExpression (Left se) = se
      resolveExpression (Right (Constant n)) = Concrete (fromIntegral n)
      resolveExpression (Right (Operation l op r)) = SOperation (env Map.! l) op (env Map.! r)
   in env

simplifyExpr :: SymbolicExpression -> SymbolicExpression
simplifyExpr Unknown = Unknown
simplifyExpr (Concrete n) = Concrete n
simplifyExpr (SOperation l op r) =
  case (simplifyExpr l, op, simplifyExpr r) of
    (Concrete lv, _, Concrete rv) -> Concrete (evalOp op lv rv)
    (l', _, r') -> SOperation l' op r'

($==$) :: SymbolicExpression -> SymbolicExpression -> Maybe Int
-- l $==$ r | trace (show l ++ " = " ++ show r) False = undefined
-- a # b == C
(SOperation l Add (Concrete x)) $==$ (Concrete y) = l $==$ Concrete (y - x)
(SOperation l Sub (Concrete x)) $==$ (Concrete y) = l $==$ Concrete (y + x)
(SOperation (Concrete x) Sub r) $==$ (Concrete y) = r $==$ Concrete (x - y)
(SOperation l Mul (Concrete x)) $==$ (Concrete y) = l $==$ Concrete (y `div` x)
(SOperation l Div (Concrete x)) $==$ (Concrete y) = l $==$ Concrete (y * x)
(SOperation (Concrete x) Div r) $==$ (Concrete y) = r $==$ Concrete (x `div` y)
(SOperation (Concrete x) op r) $==$ (Concrete y) = SOperation r op (Concrete x) $==$ Concrete y
l@Concrete {} $==$ r@SOperation {} = r $==$ l
-- x == C
Unknown $==$ (Concrete n) = Just n
(Concrete n) $==$ Unknown = Just n
-- X == X
Unknown $==$ Unknown = trace "Cannot solve x=x" Nothing
-- a # b == c # d
l $==$ r = case (simplifyExpr l, simplifyExpr r) of
  (SOperation {}, SOperation {}) -> trace "Two unknowns" Nothing
  (l', r') -> l' $==$ r'

part2 :: ByteString -> Int
part2 input =
  BS.lines input
    & map parseLine
    & map
      ( \(name, expr) -> case name of
          "humn" -> ("humn", Left Unknown)
          _ -> (name, Right expr)
      )
    & formSymbolicExpressions
    & Map.map simplifyExpr
    & ( \env ->
          let (SOperation e1 _ e2) = env Map.! "root"
           in fromJust (e1 $==$ e2)
      )
