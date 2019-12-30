library(orientDAG)
library(dagitty)
library(simMixedDAG)
library(carData)
library(bnlearn)

# load dataset and define underlying DAG
data("GSSvocab")
GSSvocab <- GSSvocab %>%
  filter(complete.cases(.)) %>%
  mutate(year = as.numeric(as.character(year)))

true_dag_dagitty <- dagitty("dag{
                            age -> educGroup;
                            age -> nativeBorn;
                            nativeBorn -> ageGroup;
                            nativeBorn -> vocab;
                            educ -> age;
                            educ -> gender;
                            educ -> year;
                            vocab -> gender;
                            vocab -> year
                            }")

# DAG adjacency matrix representation for distance calculations
true_dag <- dagitty_to_adjmatrix(true_dag_dagitty)

# Fit a non-parametric DAG model 
non_param_dag_model <- non_parametric_dag_model(true_dag_dagitty, GSSvocab)

# Generate a dataset from the above model
sim_data <- sim_mixed_dag(non_param_dag_model, N = 20000)

# First pass - estimate DAG using bnlearn::tabu function
est_dag <- tabu(sim_data)
est_dag <- bn_to_adjmatrix(est_dag)
est_dag <- est_dag[
  match(rownames(true_dag), rownames(est_dag)),
  match(colnames(true_dag), colnames(est_dag))
  ]
tabu_dist <- dag_dist(true_dag, est_dag, distance_measure = "sid")
tabu_dist

# Improve on our first pass by re-orienting edges using the orient_dag function

est_dag_orient_dag <- orient_dag(
  adjmatrix = est_dag,
  x = sim_data, 
  max_continuous_pairs_sample = 5000) # continuous pairs re-orientation takes time so sample size is kept small)
orient_dag_dist <- orientDAG::dag_dist(true_dag, est_dag_orient_dag, distance_measure = "sid")
orient_dag_dist