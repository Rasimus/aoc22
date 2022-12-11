module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, head, index, length, many, replicate, reverse, snoc, sort, tail, take, zip, (..))
import Data.Either (Either(..))
import Data.Foldable (product)
import Data.Lens (Lens', over, set)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex.Flags (dotAll)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.String (string, char)
import Parsing.String.Basic (intDecimal, skipSpaces, whiteSpace)
import Type.Prelude (Proxy(..))

type MonkeyId = Int
type Item = Int
type Monkey = {
                id :: MonkeyId
              , items :: Array Item
              , testOperation :: Int -> Int
              , operation :: Int -> Int
              , inspected :: Int
              }
_items :: forall a r. Lens' { items :: a | r } a
_items = prop (Proxy :: Proxy "items")
_inspected :: forall a r. Lens' { inspected :: a | r } a
_inspected = prop (Proxy :: Proxy "inspected")

data Op = Add Term | Mul Term
data Term = Value Int | Old

eval :: Op -> Int -> Int
eval (Add Old) x = add x x
eval (Add (Value y)) x = add y x
eval (Mul Old) x = mul x x
eval (Mul (Value y)) x = mul y x

monkey :: Parser String Monkey
monkey = do
  id_ <- monkeyId
  items_ <- items
  operation_ <- operation
  testOperation_ <- testOperation
  pure {id: id_,
        items: items_,
        testOperation: testOperation_,
        operation: operation_,
        inspected: 0}
  where
    monkeyId = string "Monkey " *> intDecimal <* string ":\n"

monkeyRound :: MonkeyId -> Array Monkey -> Array Monkey
monkeyRound idx monkeys = case index monkeys idx of
  Nothing -> monkeys
  Just monkey_ -> foldl addItem (removeItems monkeys) itemsThrown
    where
      itemsThrown :: Array (Tuple Item MonkeyId)
      itemsThrown = itemsToThrow monkey_
      nItemsThrown = length itemsThrown
      removeItems = over (ix idx <<< _inspected) (add nItemsThrown)
                    <<< set (ix idx <<< _items) []

runRound :: Array Monkey -> Array Monkey
runRound monkeys = applyAll (map monkeyRound $ 0..(nMonkeys-1)) monkeys
  where nMonkeys = length monkeys

addItem :: Array Monkey -> Tuple Item MonkeyId -> Array Monkey
addItem m (Tuple item id) = over (ix id <<< _items) (flip snoc item) m

itemsToThrow :: Monkey -> Array (Tuple Item MonkeyId)
itemsToThrow m = zip inspected (map m.testOperation inspected)
  where
    inspected = map (flip div 3 <<< m.operation) m.items

items :: Parser String (Array Int)
items = do
  skipSpaces
  _ <- string "Starting items: "
  items_ <- many do
    item <- intDecimal
    _ <- optional $ string ", "
    pure (item)
  _ <- char '\n'
  pure (items_)

operation :: Parser String (Int -> Int)
operation = do
  skipSpaces
  _ <- string "Operation: new = old "
  operator <- Mul <$ char '*' <|>
              Add <$ char '+'
  skipSpaces
  value <- Value <$> intDecimal <|>
           Old  <$ string "old"
  pure (eval $ operator value)

testOperation :: Parser String (Int -> Int)
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
  pure (\x -> if mod x divisor == 0 then trueThrow else falseThrow)

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
        monkeyBusiness = product $ take 2 $ reverse $ sort $ map _.inspected fin
      log $ "Monkey business: " <> (show monkeyBusiness)
    Left  parseErr -> logShow parseErr
