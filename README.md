
THINGS TO LEARN
---------------

1. how to walk a graph and avoid loops
2. how to read and implement an algorithm
  - a bunch of algorithms and when to apply them
  - dikistra's
  - that flow one
  - something else?

  Dijkstra: shortest path
  Bellman-Ford: shortest path (bails on negative cycle)


Chromakey (2) super simple

Fibonacci (3) tricky (unique 1,1), memoize

Hexcode (6) harder, memoize, difficult cases. could have been solved burning through it once with 2 accumulators

Minesweeper (2) GRID. I struggled until I noticed I was generating my grid wrong. Verify your data!

X Roads (6) DIRECTED GRAPH (distance) visited problem? 

X Spelunking (6) DIRECTED GRAPH (flow)

X Wormholes (6) DIRECTED GRAPH (distance)

X The Claw (4)



TODO
----

Learn Dijkstra
Learn Bellman-Ford
Learn Ford Fulkerson

Implement these and winnnn


Graph Theory
------------

Cycle: a loop
Directed vs Undirected
Negative Cycle
Strongly Connected (??)
Simple Cycle: no repeated vertices or edges other than the starting and ending ones (like, loops within loops.. is dumb)


http://en.wikipedia.org/wiki/Cycle_(graph_theory)

  A Directed Graph has a cycle if and only if a "Depth First Search" finds a back edge. Forward and Cross Edges do not indicate. 

  Divided into "Strongly Connected Component"


http://en.wikipedia.org/wiki/Depth-first_search

  output: spanning tree
    forward edges: point from a node to one of its decendants
    back edges: point from a node to one of its ancestors
    cross edges: do neither


http://en.wikipedia.org/wiki/Iterative_deepening_depth-first_search

  do a depth-first search, but visit the nodes in breadth-first order
  optimal when path cost is a non-decreasing function of the depth of the node
  (so if the cost increases with each step, you don't really have to worry about this)

  you check for solutions at each iteration. if you find one, you quit
  if you don't find you, you increase the max depth and run it again


http://en.wikipedia.org/wiki/Breadth-first_search
  
  Checks all the children of the node before going another level deep. Implemented with a queue
    - Ford–Fulkerson method (flow!)
    - shorted path (with path length measured by number of edges)
