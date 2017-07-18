#' Find the row number of a dose combination of interest.
#'
#' @param doses  A list containing doses on the original and log scale.
#' @param dose The dose combination of interest.
#' @param orig_scale A logical. Default is FALSE, meaning that 'dose' is provided on log scale. Set to TRUE if 'dose' is on original scale.
#' @return The row number of the dose combination of interest.
#' @export
#'

dose_row = function(doses, dose, orig_scale = FALSE){
  
  if (orig_scale) return(which(apply(doses$orig, 1, function(x,y)all(x==y), dose)))
  which(apply(doses$log_scale, 1, function(x,y)all(x==y), dose))
}
