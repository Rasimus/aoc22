module Main where

import Prelude

import Control.Apply (lift2)
import Data.Array (concat, dropEnd, elem, filter, find, index, mapWithIndex, unzip, zip)
import Data.Char (toCharCode)
import Data.Foldable (minimum)
import Data.Graph (Graph, fromMap, outEdges)
import Data.List (List(..), singleton)
import Data.List as L
import Data.Map (Map, filterKeys, fromFoldable, lookup, member, union, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (T3)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


type GridIndex = Tuple Int Int
type Grid = Array (Array Elevation)
type Elevation = Char
type GridGraph = Graph GridIndex GridIndex

bfs :: GridGraph -> GridIndex -> Map GridIndex Int
bfs graph start = bfs' graph queue discovered
  where
    queue = singleton start
    discovered = fromFoldable $ [Tuple start 0]

bfs' :: GridGraph -> List GridIndex -> Map GridIndex Int -> Map GridIndex Int
bfs' _ Nil discovered = discovered
bfs' graph (Cons curr rest) discovered = bfs' graph queue discovered'
  where
    depth = 1 + fromMaybe 0 (lookup curr discovered)
    queue :: List GridIndex
    queue = rest <> L.filter (not <<< flip member discovered) neighbors
    discovered' :: Map GridIndex Int
    discovered' = union discovered (fromFoldable $ map (\x -> Tuple x depth) neighbors)
    neighbors :: List GridIndex
    neighbors = fromMaybe Nil $ outEdges curr graph

cmpElev :: Elevation -> Elevation -> Boolean
cmpElev a b = (toCharCode a) - (toCharCode b) >= -1
isReachable :: Grid -> GridIndex -> GridIndex -> Boolean
isReachable g a b  = case (lift2 cmpElev) (getElevation g a) (getElevation g b) of
                          Just bool -> bool
                          Nothing -> false

getEdges :: GridIndex -> Grid -> Array GridIndex
getEdges curr@(Tuple x y) grid = filter ((flip $ isReachable grid) curr) [up, right, down, left]
  where
    up    = Tuple x   (y+1)
    right = Tuple (x+1) y
    down  = Tuple x   (y-1)
    left  = Tuple (x-1) y

getElevation :: Grid -> GridIndex -> Maybe Elevation
getElevation grid (Tuple x y) = index grid y >>= flip index x

substitute :: forall a. Eq a => a -> a -> Array a -> Array a
substitute x y xs = map (\z -> if z == x then y else z) xs

withIndex :: forall a. Array (Array a) -> Array (Array (T3 a Int Int))
withIndex grid = mapWithIndex (\row rows ->
                                mapWithIndex (\col elem' -> Tuple elem' (Tuple col row))
                                rows) grid
findElem :: forall a. Eq a => a -> Array (Array a) -> Maybe (Tuple Int Int)
findElem elem grid = snd <$> (find (eq elem <<< fst) $ concat gridWithIndex)
  where gridWithIndex = withIndex grid

findElems :: forall a. Eq a => a -> Array (Array a) -> Array (Tuple Int Int)
findElems elem grid = map snd $ (filter (eq elem <<< fst) $ concat gridWithIndex)
  where gridWithIndex = withIndex grid

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  let rows = dropEnd 1 $ split (Pattern "\n") input
      start = fromMaybe (Tuple 0 0) $ findElem 'S' $ map toCharArray rows
      end = fromMaybe (Tuple 0 0) $ findElem 'E' $ map toCharArray rows
      grid = map (substitute 'E' 'z'
                  <<< substitute 'S' 'a'
                  <<< toCharArray) rows
      indices :: Array GridIndex
      indices = snd $ unzip $ concat $ withIndex grid
      edges :: Array (List GridIndex)
      edges = map (L.fromFoldable <<< (flip getEdges grid)) indices
      gridMap :: Map GridIndex (Tuple GridIndex (List GridIndex))
      gridMap = fromFoldable $ zip indices (zip indices edges)
      graph :: GridGraph
      graph = fromMap gridMap
      distances = bfs graph end
      endDistance = lookup start distances
      aSquares = findElems 'a' grid
      aShortest =  minimum $ values $ filterKeys (flip elem aSquares) distances
  log "Part 1"
  log $ "Minimal path length: " <> fromMaybe "No available path" (show <$> endDistance)
  log "Part 2"
  log $ "Minimal pat length from an 'a' square: " <> fromMaybe "No available path" (show <$> aShortest)
