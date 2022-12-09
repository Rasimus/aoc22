module Main where

import Prelude

import Data.Array (concatMap, drop, foldl, head, length, many, nub, reverse, tail, transpose, unzip, zipWith, (..), (:))
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (greaterThan, lessThanOrEq)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, Position, position, runParser)
import Parsing.Combinators (try)
import Parsing.String (char)
import Parsing.String.Basic (digit)


type TreeHeight = Int
type TreeIdentifier = Position
type Tree = Tuple TreeIdentifier TreeHeight
type TreeRow = Array Tree
type TreeGrid = Array TreeRow

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "input.txt"
  case runParser input inputParser of
    Right treeGrid -> do
      log "Part 1"
      log $ "Number of visible trees: " <> (show $ length $ visible $ treeGrid)
      log "Part 2"
      log $ "Scenic score: " <> (show $ fromMaybe 0 $ maximum $ concatMap identity $ scenicScores treeGrid)
    Left msg -> logShow msg

visible :: TreeGrid -> Array Tree
visible treeGrid = nub (visibleFromLeft <>
                        visibleFromTop <>
                        visibleFromRight <>
                        visibleFromBottom)
  where
  visibleFromLeft = concatMap visible' treeGrid
  visibleFromTop = concatMap visible' $ transpose treeGrid
  visibleFromRight = concatMap visible' $ map reverse treeGrid
  visibleFromBottom = concatMap visible' $ map reverse $ transpose treeGrid
  visible' :: TreeRow -> TreeRow
  visible' row = foldl insertIfGreater [] row

insertIfGreater :: Array Tree -> Tree -> Array Tree
insertIfGreater xs x
  | fromMaybe true $ (greaterThan $ snd x) <$> (maximum $ snd $ unzip xs) = x : xs
  | otherwise = xs


ewProd :: Array (Array Int) -> Array (Array Int) -> Array (Array Int)
ewProd a b = zipWith ewProdVec a b
  where
    ewProdVec a' b' = zipWith (*) a' b'

scenicScores :: TreeGrid -> Array (Array Int)
scenicScores grid = foldl ewProd leftScore [upScore, rightScore, downScore]
  where
  leftScore = scoreRow heightGrid
  upScore = transpose $ scoreRow $ transpose heightGrid
  rightScore = map reverse $ scoreRow $ map reverse heightGrid
  downScore = transpose $ map reverse $ scoreRow $ map reverse $ transpose heightGrid
  scoreRow = map ((map sight) <<< subRows)
  heightGrid :: Array (Array Int)
  heightGrid = map snd $ map unzip grid
  subRows row = map (flip drop row) $ 0 .. ((length row)-1)

takeUntil :: forall a. (a -> Boolean) -> Array a -> Array a
takeUntil cond xs = case head xs of
  Just x | cond x -> x:(takeUntil cond (fromMaybe [] $ tail xs))
         | otherwise -> [x]
  Nothing -> []

sight :: Array TreeHeight -> Int
sight row | fromMaybe false (lessThanOrEq <$> (head row) <*> (tail row >>= head)) = 1
sight row = (length $ takeUntil ((>) $ fromMaybe 0 $ head row) (fromMaybe [] $ tail row))

inputParser :: Parser String TreeGrid
inputParser = do
  rows <- many do
    row <- many $ try treeParser
    _ <- char '\n'
    pure row
  pure rows

treeParser :: Parser String Tree
treeParser = do
  p <- position
  height <- digit
  pure (Tuple p (charToInt height))
  where
    charToInt :: Char -> Int
    charToInt c = fromMaybe 0 $ fromString $ singleton c
