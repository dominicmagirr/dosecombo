#' Combine original and log doses in a single object
#'
#' @param dose_matrix Doses of interest on original scale.
#' @param d_star Reference doses for scaling.
#' @return A list containing the doses of interest on the original and log scale.
#' @export
#'

doses = function(dose_matrix, d_star){

  log_scale = log(cbind(dose_matrix[,1] / d_star[1], dose_matrix[,2] / d_star[2]))

  doses = list(orig = dose_matrix, log_scale = log_scale)

  doses

}
