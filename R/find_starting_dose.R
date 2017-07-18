#' Find the id number of the recommended starting dose
#'
#' @param doses A list containing combination doses.
#' @return Starting dose.
#' @export

find_starting_dose = function(doses){
  
  prior_info =  apply(doses$log_scale, 1, get_prior_info, prior = doses$prior)
  
  admissible = which(prior_info[6,] < doses$over_limit)
  starting_dose = admissible[which.max(prior_info[5,admissible])]
  doses$orig[starting_dose,]
}

# find_starting_dose = function(prior_info, over_limit = 0.25){
# 
#   admissible = which(prior_info[6,] < over_limit)
#   starting_dose = admissible[which.max(prior_info[5,admissible])]
#   starting_dose
# }