#' @title Orient DAG edges
#'
#' @description \code{orient_dag} learns from input data the correct input DAG
#' edge orientations.
#'
#' @param adjmatrix An adjacency matrix representing the input DAG. Edges can be both oriented
#' or bi-directional.
#' @param x A data.frame with columns corresponding to nodes in input DAG
#' @param orient_continuous
#' @param max_continuous_pairs_sample Estimating the causal direction between two
#' numeric variables can be costly in time. For this reason one can cap the number
#' of measurements used for it using this argument. A value of NULL indicates not to re-orient
#' edges between 2 continuous variables.
#' @return A string denoting whether \code{vec_1} causes \code{vec_2} or vice versa
#' @example examples/orient_dag.R
#' @details Depending on the 2 variables encoding (each is either numeric or discrete)
#' a specific method is dispatched to determine the causal direction between them.
#' When the 2 variables are continuous, we can use several the general correlation measure
#' and related criteria by calling \code{\link[generalCorr]{some0pairs}}
#' (see also \href{https://cran.r-project.org/web/packages/generalCorr/vignettes/generalCorr-vignette.pdf}{Vinod 2017})
#'
#' When the 2 variables are discrete, we can use the distance correlation measure by calling
#' \code{\link[energy]{dcor}} (see also {https://arxiv.org/pdf/1803.07712.pdf}{Liu and Chan 2016}).
#'
#' When one of the variables is discrete, and the other is continuous we can discretisize the continuous
#' variable by calling \code{\link[infotheo]{discretize}} and use the method for two discrete variables.
#' @export

orient_dag <- function(adjmatrix, x, max_continuous_pairs_sample = 5000, whitelist = NULL) {
  if (!is.matrix(adjmatrix)) {
    stop("Input DAG must be represented as adjacency matrix")
  } else {
    DAG_rownames <- rownames(adjmatrix)
    DAG_colnames <- colnames(adjmatrix)
    if (!identical(DAG_rownames, DAG_colnames)) {
      stop("DAG adjacency matrix rownames and colnames must be identical")
    }
    if (mean(DAG_rownames %in% names(x)) < 1) {
      stop("Some nodes are missing from the input data x")
    }
  }
  whitelist_pos <- DAG_rownames %in% whitelist
  adjmatrix[!whitelist_pos, !whitelist_pos] <- adjmatrix[!whitelist_pos, !whitelist_pos] +
    t(adjmatrix[!whitelist_pos, !whitelist_pos])

  adjmatrix[adjmatrix == 2] <- 1 # turn to symetric matrix
  edges <- which(adjmatrix == 1, arr.ind = T)[!whitelist_pos, ]
  adjmatrix[edges[edges[, 1] > edges[, 2], ]] <- 0 # remove lower triangle such that vec_1 always causes vec_2
  edges <- which(adjmatrix == 1, arr.ind = T)
  for (i in 1:nrow(edges)) {
    vec_1 <- x[[DAG_rownames[edges[i, 1]]]]
    vec_2 <- x[[DAG_rownames[edges[i, 2]]]]
    if (class(vec_1) == "numeric" & class(vec_2) == "numeric" & is.null(max_continuous_pairs_sample)) next
    cause <- causal_direction(vec_1, vec_2, max_continuous_pairs_sample)
    if (cause == "vec 2 causes vec 1") { # flip arrow direction
      adjmatrix[edges[i, 1], edges[i, 2]] <- 0
      adjmatrix[edges[i, 2], edges[i, 1]] <- 1
    }
  }
  return(adjmatrix)
}
