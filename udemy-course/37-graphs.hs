module Graph where

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [Edge]

graph :: Graph
graph = [(1, 2), (1, 3), (1, 4), (1, 5), (2, 6), (2, 7), (4, 8), (5, 9)]

adjoins :: Graph -> Vertex -> [Vertex]
adjoins [] _ = []
adjoins ((f, s):xs) v
  | f == v = s : adjoins xs v
  | s == v = f : adjoins xs v
  | otherwise = adjoins xs v
