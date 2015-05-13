
# setup -------------------------------------------------------------------

library(igraph)

# to dot ------------------------------------------------------------------

g <- graph.ring(10)
g_textC <- textConnection("g_text", open = "w")
write.graph(g, file = g_textC, format = "dot")
close(g_textC)

write.graph(g, file = "test.dot", format = "dot")

zz <- textConnection("foo", "w")
writeLines(c("testit11", "testit21"), zz)
close(zz)

foo

g <- graph.ring(3)
write.graph(g, file = "test.dot", format = "dot")
