#' Choose dose combinations to be investigated, and plot the prior probabilities of toxicity.
#'
#' @param mono_model_1 An object of class "LogisticLogNormal".
#' @param mono_model_2 An object of class "LogisticLogNormal".
#' @param eta_mean Prior mean for interaction parameter.
#' @param eta_prec Prior precision for interaction parameter.
#' @param doses_1 The doses of drug 1 to be tested.
#' @param doses_2 The doses of drug 2 to be tested.
#' @param over_limit A number between 0 and 1. If the probability of overdosing is more than 'over_limit'
#' then this dose will be declared inadmissible.
#' @return A list of 8 items:
#' - dose combinations on original scale,
#' - dose combinations on log scale,
#' - reference dose 1,
#' - reference dose 2,
#' - over_limit (maximum acceptable probability of overdosing)
#' - plot of prior distribution,
#' - plot of prior distribution including marginals.
#' @export
#'

combine_doses = function(mono_model_1,
                         mono_model_2,
                         eta_mean,
                         eta_prec,
                         doses_1,
                         doses_2,
                         over_limit){


  prior = list(mu_1 = mono_model_1@mean,
               mu_2 = mono_model_2@mean,
               omega_1 = solve(mono_model_1@cov),
               omega_2 = solve(mono_model_2@cov),
               eta_mean = eta_mean,
               eta_prec = eta_prec)

  ref_dose_1 <- mono_model_1@refDose
  ref_dose_2 <- mono_model_2@refDose

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
