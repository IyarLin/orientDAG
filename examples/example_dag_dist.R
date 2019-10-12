library(orientDAG)
library(dagitty)

true_dag <- dagitty("dag {
  ageGroup [pos=\"0,0\"]
  vocab [pos=\"1,-1\"]
  nativeBorn [pos=\"2,-2\"]
  educ [pos=\"3,-1\"]
  gender [pos=\"4,0\"]
  nativeBorn -> educ
  nativeBorn -> vocab
  educ -> vocab
  gender -> educ
  ageGroup -> vocab
}")

estimated_dag <- dagitty("dag {
  ageGroup [pos=\"0,0\"]
  vocab [pos=\"1,-1\"]
  nativeBorn [pos=\"2,-2\"]
  educ [pos=\"3,-1\"]
  gender [pos=\"4,0\"]
  gender -> ageGroup
  nativeBorn -> vocab
  educ -> vocab
  educ -> gender
  ageGroup -> vocab
}")


par(mfrow = c(2, 1))
plot(true_dag)
text(0.3, 1.3, "true", cex = 2, col = "red")
plot(estimated_dag)
text(0.3, 1.3, "estimated", cex = 2, col = "red")

# edge distance: number of edges present in the true DAG
# and missing in the estimated DAG
## this won't work: dag_dist(true_dag, estimated_dag)
## Need to convert to adjacency matrices first
true_dag_adjmatrix <- dagitty_to_adjmatrix(true_dag)
estimated_dag_adjmatrix <- dagitty_to_adjmatrix(estimated_dag)

dag_dist(
  true_dag_adjmatrix = true_dag_adjmatrix,
  estimated_dag_adjmatrix = estimated_dag_adjmatrix,
  distance_measure = "edge"
)

# Hamming distance: counts the number of edges
# in which the graphs do not coincide (including edge direction)

dag_dist(
  true_dag_adjmatrix = true_dag_adjmatrix,
  estimated_dag_adjmatrix = estimated_dag_adjmatrix,
  distance_measure = "hamming"
)

# Structural intervention distance: number of wrongly estimated adjustment sets
sid <- dag_dist(
  true_dag_adjmatrix = true_dag_adjmatrix,
  estimated_dag_adjmatrix = estimated_dag_adjmatrix,
  distance_measure = "sid"
)

print(sid)
# The fraction of wrongly estimated adjustment sets out of all possible
# treatment-exposure pairs is:
number_of_nodes <- length(names(true_dag))
print(sid / (number_of_nodes * (number_of_nodes - 1)))
