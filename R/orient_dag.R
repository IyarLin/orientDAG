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
#' @param continuous_thresh minimum absolute sum magnitude required to re-orient a continuous-continuous pair edge 
#' @param discrete_thresh minimum absolute distance correlation magnitude required to re-orient a discrete-continuous/discrete pair edge 
#' @return A string denoting whether \code{vec_1} causes \code{vec_2} or vice versa
#' @example examples/example_orient_dag.R
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

orient_dag <- function(adjmatrix, x, max_continuous_pairs_sample = 5000, continuous_thresh = 1, discrete_thresh = 0.3) {
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
  
  edges <- which(adjmatrix == 1, arr.ind = T)
  i = 1
  while(i <= nrow(edges)){
    dup_idx <- integer(0)
    dup_idx <- which(apply(edges[, c(2,1)], 1, function(x) x[1] == edges[i, 1] & x[2] == edges[i, 2]))
    if(length(dup_idx) == 1) {
      adjmatrix[edges[dup_idx, 1], edges[dup_idx, 2]] <- 0 # remove one of the duplicates
      edges <- edges[-dup_idx, ]
    }
    vec_1 <- x[[DAG_rownames[edges[i, 1]]]]
    vec_2 <- x[[DAG_rownames[edges[i, 2]]]]
    if (class(vec_1) == "numeric" & class(vec_2) == "numeric"){
      if(is.null(max_continuous_pairs_sample)){
        next # dont change orientation
      } else {
        samples <- sample.int(length(vec_1), min(max_continuous_pairs_sample, length(vec_1)))
        vec_1 <- vec_1[samples]
        vec_2 <- vec_2[samples]
      }
    } 
    
    cause <- causal_direction(vec_1, vec_2, 
                              continuous_thresh = ifelse(length(dup_idx) == 1, 0, continuous_thresh), # if bi-directed take whichever has higher score
                              discrete_thresh = ifelse(length(dup_idx) == 1, 0, discrete_thresh) # if bi-directed take whichever has higher score)
    )
    if (cause == "vec 1 causes vec 2") { 
      adjmatrix[edges[i, 1], edges[i, 2]] <- 1
      adjmatrix[edges[i, 2], edges[i, 1]] <- 0
    } else if(cause == "vec 2 causes vec 1"){
      adjmatrix[edges[i, 1], edges[i, 2]] <- 0
      adjmatrix[edges[i, 2], edges[i, 1]] <- 1
    }
    i <- i + 1
  }
  
  return(adjmatrix)
}
