
# setup -------------------------------------------------------------------

library(igraph)

# to dot ------------------------------------------------------------------

g <- make_ring(3)
# write.graph(g, file = "test.dot", format = "dot")  # generates many errors
# write.graph(g, file = "test.graphml", format = "graphml")

xx <- set_vertex_attr(g, "name", value = c("a", "b", "c"))
as_data_frame(xx, what = "both")

# creating graphs ---------------------------------------------------------

edge_list_1 <- matrix(c(1,2,1,3,1,4,2,3), ncol = 2, byrow = TRUE)
g1 <- graph_from_edgelist(edge_list_1)

plot(graph_from_data_frame(data.frame(from = c("a", "b", "c"), to = c("b", "c", "a")),
                           vertices = data.frame(name = c("a", "b", "c"))))
plot(graph_from_data_frame(data.frame(from = c("a", "b", "c"), to = c("b", "c", "a"))))
plot(g2 <- graph_from_data_frame(data.frame(from = c("a", "b", "c"), to = c("b", "c", "a")),
                                 vertices = data.frame(name = c("a", "b", "c", "d"),
                                                       val = 2 * 1:4)))

g2 <- set_edge_attr(g2, "name", value = c("e1", "e2", "e3"))
E(g2)["e2"]
delete_edges(g2, "e2")
plot(g2, edge.label = E(g2)$name)

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
