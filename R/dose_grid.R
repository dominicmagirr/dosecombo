#' Create matrix of all possible dose combinations
#'
#' Provided with the possible levels of each dose, this function gives you all possible combinations.
#' @param d_orig A list of two vectors. The first vector contains the possible dose levels for drug 1. The second vector contains the possible dose levels for drug 2.
#' @param log_transform If TRUE, then will output log-transformed dose levels.
#' @param d_star When log_transform is true, you must provide refence dose levels -- a vector of length 2.
#' @return A matrix with two columns (dose1, dose2) and number of rows equal to the number of combinations
#' @export


dose_grid <- function(d_orig, log_transform = FALSE, d_star = NULL){

  if (log_transform){

    u_1 = log(d_orig[[1]] / d_star[1])
    u_2 = log(d_orig[[2]] / d_star[2])

  }
  else {
    u_1 = d_orig[[1]]
    u_2 = d_orig[[2]]
  }
  matrix(c(rep(u_1, each = length(u_2)),
           rep(u_2, length(u_1))),
         ncol = 2)
}
