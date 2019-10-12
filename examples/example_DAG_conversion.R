library(orientDAG)
library(dagitty)
library(bnlearn)
library(igraph)
g <- dagitty("dag {
  bn [pos=\"0,0\"]
  dagitty [pos=\"0.5,-1\"]
  adjmatrix [pos=\"1,0\"]
  adjmatrix -> dagitty
  dagitty -> adjmatrix
  bn -> adjmatrix
}")

# Below we can see the conversion directions implemented in this package:
plot(g)

# dagitty -> adjmatrix
adjmatrix <- dagitty_to_adjmatrix(g)
print(adjmatrix)

# adjmatrix -> dagitty
dagitty_obj <- adjmatrix_to_dagitty(adjmatrix)
plot(graphLayout(dagitty_obj))

# bn -> adjmatrix
bn_obj <- empty.graph(c("adjmatrix", "bn", "dagitty"))
amat(bn_obj) <- adjmatrix
plot(bn_obj)

adjmatrix <- bn_to_adjmatrix(bn_obj)
plot(graph_from_adjacency_matrix(adjmatrix))
