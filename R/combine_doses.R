#' Choose dose combinations to be investigated, and plot the prior probabilities of toxicity.
#'
#' @param doses_1 The doses of drug 1 to be tested.
#' @param doses_2 The doses of drug 2 to be tested.
#' @param ref_dose_1 The reference dose of drug 1.
#' @param ref_dose_2 The reference dose of drug 2.
#' @param prior A list of prior parameters.
#' @param over_limit A number between 0 and 1. If the probability of overdosing is more than 'over_limit'
#' then this dose will be declared inadmissible.
#' @return A list of 4 items: dose combinations on original scale, dose combinations on log scale, plot of prior distribution, plot of prior distribution including marginals.
#' @export
#'

combine_doses = function(doses_1, doses_2, ref_dose_1, ref_dose_2, prior, over_limit){

  doses_with_zero = DoseCombo::doses(dose_grid(list(c(0,doses_1), c(0,doses_2))), d_star = c(ref_dose_1,ref_dose_2))
  combo_doses = DoseCombo::doses(dose_grid(list(doses_1, doses_2)), d_star = c(ref_dose_1,ref_dose_2))



  # return combo_doses and plot

  doses_with_zero_list = list(orig = doses_with_zero$orig,
                               log_scale = doses_with_zero$log_scale,
                               ref_dose_1 = ref_dose_1,
                               ref_dose_2 = ref_dose_2,
                               prior = prior,
                               over_limit = over_limit,
                              prior_plot = NULL,
                              prior_plot_with_zero = NULL)

  combo_doses_list = list(orig = combo_doses$orig,
                          log_scale = combo_doses$log_scale,
                          ref_dose_1 = ref_dose_1,
                          ref_dose_2 = ref_dose_2,
                          prior = prior,
                          over_limit = over_limit,
                          prior_plot = NULL,
                          prior_plot_with_zero = NULL)

  # plot prior with zero

  prior_info_with_zero = apply(doses_with_zero$log_scale, 1, get_prior_info, prior = prior)
  prior_plot_with_zero = plot_info(prior_info_with_zero, doses_with_zero_list)

  # plot prior

  prior_info = apply(combo_doses$log_scale, 1, get_prior_info, prior = prior)
  prior_plot = plot_info(prior_info, combo_doses_list)

  list(orig = combo_doses$orig,
       log_scale = combo_doses$log_scale,
       ref_dose_1 = ref_dose_1,
       ref_dose_2 = ref_dose_2,
       prior = prior,
       over_limit = over_limit,
       prior_plot = prior_plot,
       prior_plot_with_zero = prior_plot_with_zero)
}
