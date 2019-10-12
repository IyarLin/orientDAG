#' @title Convert an adjacency matrix to a dagitty object
#'
#' @description \code{adjmatrix_to_dagitty} takes as
#' input an adjacency matrix and converts it to
#' a \code{dagitty} object.
#'
#' @param adjmatrix A named adjacency matrix where a 1 in the \code{i,j}
#' position denotes an edge from node \code{i} to node \code{j}.
#' The matrix row and column names should denote the node names.
#' @return A \code{dagitty} object with the same edge orientations.
#' @example examples/example_DAG_conversion.R
#' @seealso \code{\link{dagitty_to_adjmatrix}} and \code{\link{bn_to_adjmatrix}}
#' @export

adjmatrix_to_dagitty <- function(adjmatrix) {
  if (is.null(rownames(adjmatrix)) | is.null(colnames(adjmatrix)) | !identical(rownames(adjmatrix), colnames(adjmatrix))) {
    warning("Matrix column names or rownames are either missing or not compatible. They will be replaced by numeric node names")
    nodes <- 1:nrow(adjmatrix)
  } else {
    nodes <- rownames(adjmatrix)
  }

  from_to <- which(adjmatrix == 1, arr.ind = T)

  dag_string <- paste0(
    "dag { \n",
    paste0(nodes, collapse = "\n"),
    "\n",
    paste0(apply(from_to, 1, function(x) paste0(nodes[x[1]], " -> ", nodes[x[2]])),
      collapse = "\n"
    ),
    "\n } \n"
  )
  return(dagitty:::dagitty(dag_string))
}
