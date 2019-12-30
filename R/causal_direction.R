#' @title Determine the causal direction between 2 variables
#'
#' @description \code{causal_direction} determines the causal
#' direction between 2 variables based on input measurements
#' assuming a causal relationship exists and there are no
#' hidden confounders.
#'
#' @param vec_1 Measurements of the first variable
#' @param vec_2 Measurements of the second variable
#' numeric variables can be costly in time. For this reason one can cap the number
#' of measurements used for it using this argument.
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
#' \code{\link[energy]{dcor}} (see also \href{https://arxiv.org/pdf/1803.07712.pdf}{Liu and Chan 2016}).
#'
#' When one of the variables is discrete, and the other is continuous we can discretisize the continuous
#' variable by calling \code{\link[infotheo]{discretize}} and use the method for two discrete variables.
#' @export

causal_direction <- function(vec_1, vec_2, continuous_thresh, discrete_thresh) {
  if (class(vec_1) == "character") vec_1 <- factor(vec_1)
  if (class(vec_2) == "character") vec_2 <- factor(vec_2)
  y_cond_x <- function(x, y) {
    ans <- sapply(levels(x), function(x_val) {
      table(y[x == x_val]) / sum(x == x_val)
    })
    ans[is.nan(ans)] <- 0
    ans
  }
  if (class(vec_1) == "factor" & class(vec_2) == "numeric"){ 
    vec_2 <- factor(infotheo::discretize(vec_2)$X)
  }
  if (class(vec_1) == "numeric" & class(vec_2) == "factor"){ 
    vec_1 <- factor(infotheo::discretize(vec_1)$X)
  }
  if (class(vec_1) == "factor" & class(vec_2) == "factor") {
    p_vec_2_given_vec1 <- y_cond_x(x = vec_1, y = vec_2)
    p_vec_1 <- table(vec_1) / length(vec_1)
    dist_vec_1_causes_vec_2 <- energy:::dcor(p_vec_1, t(p_vec_2_given_vec1))

    p_vec_1_given_vec2 <- y_cond_x(x = vec_2, y = vec_1)
    p_vec_2 <- table(vec_2) / length(vec_2)
    dist_vec_2_causes_vec_1 <- energy:::dcor(p_vec_2, t(p_vec_1_given_vec2))

    if (dist_vec_2_causes_vec_1 - dist_vec_1_causes_vec_2 > discrete_thresh) {
      return("vec 1 causes vec 2")
    } else if(dist_vec_1_causes_vec_2 - dist_vec_2_causes_vec_1 > discrete_thresh){
      return("vec 2 causes vec 1")
    } else {
      return("not sure")
    }
  } else {
    cause_sum <- as.numeric(generalCorr:::some0Pairs(data.frame(vec_1, vec_2), verbo = F)$outVote[7])
    if (cause_sum > continuous_thresh) {
      return("vec 1 causes vec 2")
    } else if (cause_sum < -continuous_thresh){
      return("vec 2 causes vec 1")
    } else {
      return("not sure")
    }
  }
}
