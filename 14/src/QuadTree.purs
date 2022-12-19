module QuadTree where

import Prelude

type Point = {x :: Int, y :: Int}

data QuadTree = QuadTree {topLeft :: QuadTree,
                          topRight :: QuadTree,
                          bottomLeft :: QuadTree,
                          bottomRight :: QuadTree,
                          key :: Point} |
                Empty

data Direction = TopLeft | TopRight | BottomRight | BottomLeft
derive instance eqDirection :: Eq Direction

direction :: Point -> Point -> Direction
direction a b | a.x <= b.x && a.y <= b.y = TopLeft
              | a.x > b.x && a.y <= b.y = TopRight
              | a.x <= b.x && a.y > b.y = BottomLeft
              | otherwise = BottomRight
leaf :: Point -> QuadTree
leaf p = QuadTree {topLeft: Empty, topRight: Empty, bottomLeft: Empty, bottomRight: Empty, key: p}


insert :: QuadTree -> Point -> QuadTree
insert Empty p =  leaf p
insert (QuadTree qt) p
  | direction p qt.key == TopLeft = QuadTree {topLeft: insert qt.topLeft p,
                                              topRight: qt.topRight,
                                              bottomLeft: qt.bottomLeft,
                                              bottomRight: qt.bottomRight,
                                              key: qt.key}
  | direction p qt.key == TopRight = QuadTree {topLeft: qt.topLeft,
                                               topRight: insert qt.topRight p,
                                               bottomLeft: qt.bottomLeft,
                                               bottomRight: qt.bottomRight,
                                               key: qt.key}
  | direction p qt.key == BottomRight = QuadTree {topLeft: qt.topLeft,
                                                  topRight: qt.topRight,
                                                  bottomLeft: qt.bottomLeft,
                                                  bottomRight: insert qt.bottomRight p,
                                                  key: qt.key}
  | otherwise = QuadTree {topLeft: qt.topLeft,
                          topRight: qt.topRight,
                          bottomLeft: insert qt.bottomLeft p,
                          bottomRight: qt.bottomRight,
                          key: qt.key}

exists :: QuadTree -> Point -> Boolean
exists Empty _ = false
exists (QuadTree qt) p | qt.key == p = true
                       | direction p qt.key == TopLeft = exists qt.topLeft p
                       | direction p qt.key == TopRight = exists qt.topRight p
                       | direction p qt.key == BottomLeft = exists qt.bottomLeft p
                       | otherwise = exists qt.bottomRight p
