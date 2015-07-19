
# setup -------------------------------------------------------------------

library(igraph)

# to dot ------------------------------------------------------------------

g <- graph.ring(3)
# write.graph(g, file = "test.dot", format = "dot")  # generates many errors
# write.graph(g, file = "test.graphml", format = "graphml")


# depth first traversal ---------------------------------------------------

g <- graph_from_literal(A-B-D-E, B-C, D-F, D-C)
plot(g)
g <- g + edge(V(g)["E"], V(g)["F"])
plot(g)

#' do a depth first traversal of g starting at v
depth_first <- function(g, v) {
  get_nbd <- function(x) neighbors(g, x)
  q <- v
  added <- v
  proc <- V(g)[FALSE]   # empty list of vertices from this graph
  while(length(q) > 0) {
    n <- q[1]
    q <- q[-1]

    proc <- c(proc, n)
    
    nbd <- get_nbd(n)
    # only add any element once (or finite no times if multiple edges)
    nbd <- nbd[!nbd %in% added]
    if(length(nbd) != 0) {
      q <- c(nbd, q)
      added <- c(added, nbd)
    }
    
  }
  proc
}

depth_first(g, V(g)["B"])

#' do a breadth first traversal of g starting at v
breadth_first <- function(g, v) {
  q <- v
  added <- v
  proc <- V(g)[FALSE]  # empty list of vertices from this graph
  while(length(q) > 0) {
    n <- q[1]
    q <- q[-1]
    
    proc <- c(proc, n)
    
    nbd <- neighbors(g, n)
    # only add any element once (or finite no times if multiple edges)
    nbd <- nbd[!nbd %in% added]
    if(length(nbd) != 0) {
      q <- c(q, nbd)
      added <- c(added, nbd)
    }
    
  }
  proc
}

breadth_first(g, V(g)["A"])
