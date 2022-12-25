module ValveGraph where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), concatMap, filter, find, singleton)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)

type Label = String
type ValveGraph = Map Label (Tuple Valve (List Edge))
type Valve = {label :: Label, pressure :: Int}
type Edge = {to :: Label, from :: Label, cost :: Int}

edgesOut :: Label -> ValveGraph -> List Edge
edgesOut c g = fromMaybe Nil $ snd <$> M.lookup c g

edgesIn :: Label -> ValveGraph -> List Edge
edgesIn c g = filter (eq c <<< _.to) $ concatMap snd $ M.values g

insertEdge :: ValveGraph ->  Edge -> ValveGraph
insertEdge graph edge@{to, from, cost} =
  let edges = edgesOut from graph in
  case find (eq to <<< _.to) edges of
    Just oldEdge | oldEdge.cost < cost -> graph
    _ -> let newEdges = (Cons edge $ filter ((/=) to <<< _.to) edges)
         in M.update (\(Tuple a _) -> Just $ Tuple a newEdges) from graph

removeValve :: Label -> ValveGraph -> ValveGraph
removeValve c g = deleteEdgesTo $ M.delete c g
  where
  deleteEdgesTo :: ValveGraph -> ValveGraph
  deleteEdgesTo = map ((<$>) filterTo)
  filterTo :: List Edge -> List Edge
  filterTo = filter ((/=) c <<< _.to)-- Update edges

pressure :: Label -> ValveGraph -> Maybe Int
pressure c g = (_.pressure <<< fst) <$> M.lookup c g

pruneValve :: ValveGraph -> Label -> ValveGraph
pruneValve g c = foldl insertEdge (removeValve c g) newEdges
  where
    newEdges :: List Edge
    newEdges = do
      from <- edgesOut c g
      to   <- edgesIn c g
      pure {to: from.to, from: to.from, cost: from.cost + to.cost}

edgeCost :: Label -> Label -> ValveGraph -> Maybe Int
edgeCost a b g = _.cost <$> (find (eq b <<< _.to) $ edgesOut a g)

shortestPaths :: ValveGraph -> Label -> Map Label Int
shortestPaths graph start = bfs graph queue discovered
  where
    queue = singleton start
    discovered = M.fromFoldable $ [Tuple start 0]

bfs :: ValveGraph -> List Label -> Map Label Int -> Map Label Int
bfs _ Nil discovered = discovered
bfs graph (Cons curr rest) discovered = bfs graph queue discovered'
  where
    depth to = (edgeCost curr to graph # fromMaybe 0) +
               (M.lookup curr discovered # fromMaybe 0)
    queue :: List Label
    queue = rest <> filter (not <<< flip M.member discovered) neighbors
    discovered' :: Map Label Int
    discovered' = M.union discovered (M.fromFoldable $ map (\x -> Tuple x (depth x)) neighbors)
    neighbors :: List Label
    neighbors = map _.to $ edgesOut curr graph
