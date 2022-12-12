module Main
  ( Item
  , Monkey
  , MonkeyId
  , Op(..)
  , Term(..)
  , _inspected
  , _items
  , addItem
  , applyAll
  , eval
  , inputParser
  , items
  , itemsToThrow
  , main
  , monkey
  , monkeyRound
  , operation
  , runRound
  , testOperation
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, head, index, length, many, replicate, reverse, snoc, sort, tail, take, zip, (..))
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(..))
import Data.Foldable (product)
import Data.Lens (Lens', over, set)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (T3)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.String (string, char)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Type.Prelude (Proxy(..))

type MonkeyId = Int
type Item = BigInt
type Monkey = {
                id :: MonkeyId
              , items :: Array Item
              , testOperation :: BigInt -> MonkeyId
              , testValue :: BigInt
              , operation :: Op
              , inspected :: Int
              }
_items :: forall a r. Lens' { items :: a | r } a
_items = prop (Proxy :: Proxy "items")
_inspected :: forall a r. Lens' { inspected :: a | r } a
_inspected = prop (Proxy :: Proxy "inspected")

data Op = Add Term | Mul Term
data Term = Value BigInt | Old

eval :: Op -> BigInt -> BigInt
eval (Add Old) x = eval (Add (Value x)) x
eval (Add (Value y)) x = add y x
eval (Mul Old) x = mul x x
eval (Mul (Value y)) x = mul x y

monkeyRound :: MonkeyId -> Array Monkey -> Array Monkey
monkeyRound idx monkeys = case index monkeys idx of
  Nothing -> monkeys
  Just monkey_ -> foldl addItem (removeItems monkeys) itemsThrown
    where
      lcm = product $ (map _.testValue monkeys)
      itemsThrown = itemsToThrow lcm monkey_
      nItemsThrown = length itemsThrown
      removeItems = over (ix idx <<< _inspected) (add nItemsThrown)
                    <<< set (ix idx <<< _items) []

runRound :: Array Monkey -> Array Monkey
runRound monkeys = applyAll (map monkeyRound $ 0..(nMonkeys-1)) monkeys
  where nMonkeys = length monkeys

addItem :: Array Monkey -> Tuple Item MonkeyId -> Array Monkey
addItem m (Tuple item id) = over (ix id <<< _items) (flip snoc item) m

itemsToThrow :: BigInt -> Monkey -> Array (Tuple Item MonkeyId)
itemsToThrow lcm m = zip inspected (map m.testOperation inspected)
  where
    inspected = map ((flip mod lcm) <<< (eval m.operation)) m.items

monkey :: Parser String Monkey
monkey = do
  id_ <- monkeyId
  items_ <- items
  operation_ <- operation
  Tuple divisor (Tuple tVal fVal) <- testOperation
  pure {id: id_,
        items: items_,
        testOperation: (\x -> if mod x divisor == (fromInt 0) then tVal else fVal),
        testValue: divisor,
        operation: operation_,
        inspected: 0}
  where
    monkeyId = string "Monkey " *> intDecimal <* string ":\n"

items :: Parser String (Array BigInt)
items = do
  skipSpaces
  _ <- string "Starting items: "
  items_ <- many do
    item <- intDecimal
    _ <- optional $ string ", "
    pure (fromInt item)
  _ <- char '\n'
  pure (items_)

operation :: Parser String Op
operation = do
  skipSpaces
  _ <- string "Operation: new = old "
  operator <- Mul <$ char '*' <|>
              Add <$ char '+'
  skipSpaces
  value <- (Value <<< fromInt) <$> intDecimal <|>
           Old  <$ string "old"
  pure (operator (value))

testOperation :: Parser String (T3 BigInt Int Int)
testOperation = do
  skipSpaces
  divisor <- string "Test: divisible by "
             *> intDecimal <* char '\n'
  skipSpaces
  trueThrow  <- string "If true: throw to monkey "
                *> intDecimal <* char '\n'
  skipSpaces
  falseThrow <- string "If false: throw to monkey "
                *> intDecimal <* char '\n'
  pure (Tuple (fromInt divisor) (Tuple trueThrow falseThrow))
  -- pure (\x -> if mod x divisor == 0 then trueThrow else falseThrow)

inputParser :: Parser String (Array Monkey)
inputParser = do
  monkeys <- many $ monkey <* (optional $ char '\n')
  pure (monkeys)

applyAll :: forall a. Array (a -> a) -> a -> a
applyAll xs x = case head xs of
  Just f -> applyAll (tail' xs) (f x)
      where
        tail' = fromMaybe [] <<< tail
  Nothing -> x

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right monkeys -> do
      log "Part 1"
      let
        fin = applyAll (replicate 20 runRound) monkeys
        inspected = reverse $ sort $ map _.inspected fin
        monkeyBusiness = product $ take 2 $ inspected
      log $ "Monkey business: " <> show monkeyBusiness
      log "Part 2"
      let
        fin2 = applyAll (replicate 10000 runRound) monkeys
        inspected2 = reverse $ sort $ map _.inspected fin2
        monkeyBusiness2 = product $ map fromInt $ take 2 inspected2
      log $ "Monkey business: " <> show monkeyBusiness2
    Left  parseErr -> logShow parseErr
