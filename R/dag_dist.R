#' @title Calculate distance measure between true and estimated DAGs
#'
#' @description \code{dag_dist} calculates a user specified
#' distance measure between a supplied true DAG and an
#' estimated DAG
#'
#' @param true_dag_adjmatrix An adjacency matrix of the true DAG
#' @param estimated_dag_adjmatrix An adjacency matrix of the estimated DAG
#' @param distance_measure One of: 'edge', 'hamming' or 'sid'
#' @return The distance between true and estimated DAGs
#' @example examples/example_dag_dist.R
#' @details The 'edge' distance counts the number of edges present in the true DAG
#' and missing in the estimated DAG, as well as the number of edges present in the
#' estimated DAG and missing in the drue DAG. For details on the 'hamming' option
#' see \code{\link[SID]{hammingDist}} and for 'sid' see \code{\link[SID]{structIntervDist}}
#' @export

dag_dist <- function(true_dag_adjmatrix, estimated_dag_adjmatrix, distance_measure) {
  if (!is.matrix(true_dag_adjmatrix) | !is.matrix(estimated_dag_adjmatrix)) {
    stop("Input DAG must be represented as adjacency matrices")
  } else if (is.null(rownames(true_dag_adjmatrix)) | is.null(colnames(true_dag_adjmatrix)) |
    is.null(rownames(estimated_dag_adjmatrix)) | is.null(colnames(estimated_dag_adjmatrix))) {
    stop("Both adjacency matrices must have node names recorded in the row and column names")
  } else {
    estimated_dag_adjmatrix <- estimated_dag_adjmatrix[
      match(rownames(true_dag_adjmatrix), rownames(estimated_dag_adjmatrix)),
      match(colnames(true_dag_adjmatrix), colnames(estimated_dag_adjmatrix))
    ]
    if (!identical(rownames(true_dag_adjmatrix), rownames(estimated_dag_adjmatrix)) |
      !identical(colnames(true_dag_adjmatrix), colnames(estimated_dag_adjmatrix))) {
      stop("DAG nodes recorded in rownames and column names differ")
    }
  }
  if (distance_measure == "edge") {
    symetric_true <- true_dag_adjmatrix + t(true_dag_adjmatrix)
    symetric_estimated <- estimated_dag_adjmatrix + t(estimated_dag_adjmatrix)
    symetric_true[which(symetric_true == 2)] <- 1
    symetric_estimated[which(symetric_estimated == 2)] <- 1
    ans <- sum(symetric_true != symetric_estimated) / 2
  } else if (distance_measure == "hamming") {
    ans <- SID:::hammingDist(true_dag_adjmatrix, estimated_dag_adjmatrix)
  } else if (distance_measure == "sid") {
    ans <- SID:::structIntervDist(true_dag_adjmatrix, estimated_dag_adjmatrix)$sid
  } else {
    stop("distance_measure must be one of 'edge', 'hamming' or 'sid'")
  }
  return(ans)
}
