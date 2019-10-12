#' @title Convert a bn object to an adjacency matrix
#'
#' @description \code{bn_to_adjmatrix} takes as
#' input a \code{bn} object and converts it to
#' an adjacency matrix.
#'
#' @param bn_obj An object of class \code{bn}.
#' @return A named adjacency matrix where a 1 in the \code{i,j}
#' position denotes an arrow from node \code{i} to node \code{j}.
#' The matrix row and column names denote the node names.
#' @example examples/example_DAG_conversion.R
#' @seealso \code{\link{dagitty_to_adjmatrix}} and \code{\link{adjmatrix_to_dagitty}}
#' @export

bn_to_adjmatrix <- function(bn_obj) {
  edg <- as.data.frame(bn_obj$arcs)
  node_names <- names(bn_obj$nodes)
  ans_mat <- matrix(
    data = 0, nrow = length(node_names),
    ncol = length(node_names),
    dimnames = list(node_names, node_names)
  )

  ans_mat[as.matrix(edg[c("from", "to")])] <- 1
  return(ans_mat)
}
