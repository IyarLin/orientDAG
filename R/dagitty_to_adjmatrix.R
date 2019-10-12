#' @title Convert a dagitty object to an adjacency matrix
#'
#' @description \code{dagitty_to_adjmatrix} takes as
#' input a \code{dagitty} object and converts it to
#' an adjacency matrix.
#'
#' @param daggity_obj An object of class \code{dagitty}.
#' @return A named adjacency matrix where a 1 in the \code{i,j}
#' position denotes an arrow from node \code{i} to node \code{j}.
#' The matrix row and column names denote the node names.
#' @example examples/example_DAG_conversion.R
#' @seealso \code{\link{adjmatrix_to_dagitty}} and \code{\link{bn_to_adjmatrix}}
#' @export

dagitty_to_adjmatrix <- function(daggity_obj) {
  edg <- dagitty:::edges(daggity_obj)
  node_names <- dagitty:::names.dagitty(daggity_obj)
  ans_mat <- matrix(
    data = 0, nrow = length(node_names),
    ncol = length(node_names),
    dimnames = list(node_names, node_names)
  )

  ans_mat[as.matrix(edg[c("v", "w")])] <- 1
  return(ans_mat)
}
