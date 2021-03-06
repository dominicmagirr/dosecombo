#' Get the next dose
#'
#' This function takes into account the probability of underdosing and overdosing at each dose combination
#' to choose the dose for the next cohort.
#' @param info_mat Matrix containing information about underdosing and overdosing at each dose combination
#' @param doses A list containing combination doses.
#' @param current_dose Row number of 'doses' corresponding ot current dose.
#' @param add_random Should some randomness be added to choice of next dose? Default is FALSE.
#' @param frac_target If randomness is added, how close to the 'best' dose is acceptable. Default is 75 percent of P(target toxicity).
#' @return The row number corresponding to the dose combination with the largest probability of target
#' toxicity out of all admissible doses. If all dose combinations are inadmissible then NA is returned.
#' @export

get_next_dose = function(info_mat, doses, current_dose, add_random = FALSE, frac_target = 0.75){

  #data_vis = info_mat %>%
  #  t() %>%
  #  as.data.frame()

  #names(data_vis) = c("dose_1",
  #                    "dose_2",
  #                    "lower",
  #                    "median",
  #                    "upper",
  #                    "under",
  #                    "target",
  #                    "over")

  ewoc_true = admissible_doses(current_dose = current_dose, doses = doses$orig)

  admissible_i = which((info_mat[6,] < doses$over_limit) & ewoc_true)

  if (length(admissible_i) == 0){
    next_dose = NA
  }
  else {
    max_t = which.max(info_mat[5,][admissible_i])

    max_t_value = max(info_mat[5,][admissible_i])

    possible_doses = which(info_mat[5,][admissible_i] >= frac_target * max_t_value)

    if (add_random == TRUE) {
      next_dose = admissible_i[sample(possible_doses,1)]
    }
    else {
      next_dose = admissible_i[max_t]
    }
  }
  next_dose
}


# get_next_dose = function(info_mat, over_limit, doses, current_dose){
#
#   #data_vis = info_mat %>%
#   #  t() %>%
#   #  as.data.frame()
#
#   #names(data_vis) = c("dose_1",
#   #                    "dose_2",
#   #                    "lower",
#   #                    "median",
#   #                    "upper",
#   #                    "under",
#   #                    "target",
#   #                    "over")
#
#   ewoc_true = admissible_doses(current_dose = current_dose, doses = doses)
#
#   admissible_i = which((info_mat[6,] < over_limit) & ewoc_true)
#
#   if (length(admissible_i) == 0){
#     next_dose = NA
#   }
#   else {
#     max_t = which.max(info_mat[5,][admissible_i])
#
#     next_dose = admissible_i[max_t]
#   }
#   next_dose
# }
#
#
#
