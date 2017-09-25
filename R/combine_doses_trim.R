#' Choose dose combinations to be investigated, and plot the prior probabilities of toxicity.
#'
#' @param combo_doses A list from "combine_doses" function.
#' @param drop_ids Which elements of combo_doses$orig (and combo_doses$log_scale) should be dropped.
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

combine_doses_trim = function(combo_doses,drop_ids){


  combo_doses$orig = combo_doses$orig[-drop_ids,]
  combo_doses$log_scale = combo_doses$log_scale[-drop_ids,]

  combo_doses_list = list(orig = combo_doses$orig,
                          log_scale = combo_doses$log_scale,
                          ref_dose_1 = combo_doses$ref_dose_1,
                          ref_dose_2 = combo_doses$ref_dose_2,
                          prior = combo_doses$prior,
                          over_limit = combo_doses$over_limit,
                          prior_plot = NULL,
                          prior_plot_with_zero = NULL)


  # plot prior

  prior_info = apply(combo_doses$log_scale, 1, get_prior_info, prior = prior)
  prior_plot = plot_info(prior_info, combo_doses_list)

  list(orig = combo_doses$orig,
       log_scale = combo_doses$log_scale,
       ref_dose_1 = combo_doses$ref_dose_1,
       ref_dose_2 = combo_doses$ref_dose_2,
       prior = combo_doses$prior,
       over_limit = combo_doses$over_limit,
       prior_plot = prior_plot,
       prior_plot_with_zero = combo_doses$prior_plot_with_zero)

}
