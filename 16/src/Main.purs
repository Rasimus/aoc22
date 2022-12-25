module Main
  where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.State (State, evalState, gets, modify_)
import Data.Array (fromFoldable, many, uncons)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T3)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String (anyChar, char, string)
import Parsing.String.Basic (intDecimal)
import ValveGraph (Edge, Label, ValveGraph, Valve, pressure, pruneValve, shortestPaths)

type LookupKey = {opened :: Set Label, actor :: Actor}
type ValueLookup = M.Map LookupKey Int
data ActorDuo = ActorDuo Actor Actor


derive instance eqActorDuo :: Eq ActorDuo
derive instance ordActorDuo :: Ord ActorDuo

type MultiLookupKey = {toVisit :: Array Valve, actors :: ActorDuo}
type MultiValueLookup = M.Map MultiLookupKey Int
type PathMap = M.Map Label (M.Map Label Int)
type Actor = {t :: Int, vertex :: Valve}

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  case runParser input parseInput of
    Right valveGraph -> do
      case M.lookup "AA" valveGraph of
        Nothing -> log "No start valve labeled AA found"
        Just (Tuple startValve _) ->
          let
            zeroValvesSinStart = M.keys $
              M.filter (\(Tuple v _) -> v /= startValve && v.pressure == 0) valveGraph
            -- Remove all zero vertices except "AA"
            prunedGraph = foldl pruneValve valveGraph zeroValvesSinStart
            pathMap :: PathMap
            pathMap = M.fromFoldable $ map (\x -> Tuple x (shortestPaths prunedGraph x)) (fromFoldable $ M.keys prunedGraph)
            actor = ActorDuo {t: 30, vertex: startValve} {t: 0, vertex: startValve}
            actors = ActorDuo {t: 26, vertex: startValve} {t: 26, vertex: startValve}
            toVisit :: Array Valve
            toVisit = A.fromFoldable $ map fst $ M.values $ M.filterKeys (not $ eq "AA") prunedGraph
            sortedToVisit = A.sortBy (compareDistance pathMap startValve) toVisit
          in do
          log "Part 1:"
          logShow $ evalState (bestPath actor pathMap sortedToVisit) M.empty
          log "Part 2:"
          logShow $ evalState (bestPath actors pathMap sortedToVisit) M.empty
    Left msg -> logShow msg

parseInput :: Parser String ValveGraph
parseInput = do
  valves <- many valve
  pure $ M.fromFoldable $ map (\t@(Tuple v _) -> Tuple v.label t) valves

valve :: Parser String (Tuple Valve (List Edge))
valve = do
  name <- string "Valve " *> label
  rate <- string " has flow rate=" *> intDecimal <* string "; "
  edges <- tunnelPrefix *> label `sepBy` string ", "
  _ <- char '\n'
  pure $ Tuple {label: name, pressure: rate} (map ({from: name, to:_ , cost: 1}) edges)
  where
    label = do
      a <- anyChar
      b <- anyChar
      pure (fromCharArray [a, b])
    tunnelPrefix = string "tunnels lead to valves " <|> string "tunnel leads to valve "

compareDistance :: PathMap -> Valve -> Valve -> Valve -> Ordering
compareDistance paths from a b =
  let
    distances = M.lookup from.label paths # fromMaybe M.empty
  in
  compare (M.lookup a.label distances # fromMaybe 0)
          (M.lookup b.label distances # fromMaybe 0)


heuristic :: Int -> ActorDuo -> Array Valve -> Int
heuristic val (ActorDuo a1 a2) toVisit =
  val + heuristic' a1.t a2.t pressures
  where
  pressures :: Array Int
  pressures = map _.pressure toVisit
  heuristic' :: Int -> Int -> Array Int -> Int
  heuristic' t1 t2 xs
    | t2 > t1 = heuristic' t2 t1 xs
    | t1 <= 2 && t2 <= 2 = 0
    | otherwise = case uncons xs of
      Nothing -> 0
      Just {head, tail} -> (t1-2) * head + heuristic' (t1-2) t2 tail

memoizedBestPath :: ActorDuo -> PathMap -> Array Valve -> State MultiValueLookup Int
memoizedBestPath actors paths toVisit = do
  memoizedVal <- gets (M.lookup {actors, toVisit})
  case memoizedVal of
    Nothing -> do
      calculatedVal <- bestPath actors paths toVisit
      modify_ $ M.insert {actors, toVisit} calculatedVal
      pure calculatedVal
    Just v -> pure v

bestPath :: ActorDuo -> PathMap -> Array Valve -> State MultiValueLookup Int
bestPath (ActorDuo a1 a2) paths toVisit
  | a1.t <= 1 && a2.t <= 1 = pure 0
  | a1.t < a2.t = bestPath (ActorDuo a2 a1) paths toVisit
  | otherwise = do
    A.foldM hasPotential 0 $ map (\x-> Tuple x $ heuristicCandidate x) candidates
    where
      candidates :: Array (Tuple Int (Tuple ActorDuo (Array Valve)))
      candidates = do
        -- Optimize by picking edges in order of shortest distance
        nextVertex <- A.sortBy (compareDistance paths a1.vertex) toVisit
        isA1 <- [true]
        let
          actor = if isA1 then a1 else a2
          distance :: Int
          distance = (fromMaybe 0 $
                      M.lookup nextVertex.label (fromMaybe M.empty $ M.lookup actor.vertex.label paths))
          nextT :: Int
          nextT = actor.t - 1 - distance
        guard $ nextT >= 0
        let
          nextToVisit :: Array Valve
          nextToVisit = A.delete nextVertex toVisit
          nextActors :: ActorDuo
          nextActors | isA1 = ActorDuo {t: nextT, vertex: nextVertex} a2
                     | otherwise = ActorDuo a1 {t: nextT, vertex: nextVertex}
          nextVal :: Int
          nextVal = nextT * nextVertex.pressure
        pure $ Tuple nextVal (Tuple nextActors nextToVisit)

      hasPotential :: Int
                      -> Tuple (T3 Int ActorDuo (Array Valve)) Int
                      -> State MultiValueLookup Int
      hasPotential bestSoFar (Tuple cand heuristicValue)
        | bestSoFar < heuristicValue = do
          actualValue <- evalCandidate cand
          pure $ max bestSoFar actualValue
        | otherwise = do
          pure bestSoFar

      heuristicCandidate :: T3 Int ActorDuo (Array Valve) -> Int
      heuristicCandidate (Tuple val (Tuple actors _)) = heuristic val actors toVisit

      evalCandidate :: T3 Int ActorDuo (Array Valve)
                       -> State MultiValueLookup Int
      evalCandidate (Tuple val (Tuple actors toVisit')) = do
        evaluation <- memoizedBestPath actors paths toVisit'
        pure $ val + evaluation
