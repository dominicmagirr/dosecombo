#' Simulate model-based and rule-based trials, and compare results.
#'
#' @param truth A vector of 'true' p(DLT)'s, corresponding to the rows of 'doses'.
#' @param doses  A list containing doses on the original and log scale.
#' @param starting_dose The row number of 'doses' corresponding to the starting dose.
#' @param route_through_doses The row numbers of 'doses' corresponding to the doses used in the '3+3' design.
#' @param max_n_cohort The maximum number of cohorts.
#' @param cohort_size The number of patients per cohort.
#' @param n_sims The number of simulations.
#' @param seed The random number seed.
#' @param max_cohort_size Maximum size of cohort. If this is reached, and the max_n or max_p_target is reached then the trial can stop.
#' @param max_n Maximum total sample size. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param max_p_target Maximum probability of targeted toxicity. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param add_random Should some randomness be added to choice of next dose? Default is FALSE.
#' @param frac_target If randomness is added, how close to the 'best' dose is acceptable. Default is 75 percent of P(target toxicity).
#' @param over_limit_end The safety limit to be applied at the end of the trial
#' @param n_iter Number of iterations in MCMC.
#' @return A list of 4 items: a plot of the true p(DLTs), a graphical comparison of the 2 methods, a summary of model-based simulations, and a summary of rule-based simulations.
#' @export
#'

comparison_sim = function(truth,
                          doses,
                          starting_dose,
                          route_through_doses,
                          max_n_cohort,
                          cohort_size,
                          n_sims,
                          seed,
                          max_cohort_size,
                          max_n,
                          max_p_target,
                          add_random = FALSE,
                          frac_target = 0.75,
                          over_limit_end = NA, 
                          n_iter = 2500){

  ##################################

  starting_dose_model = dose_row(doses, starting_dose, orig_scale = TRUE)

  starting_dose_rule = which(route_through_doses == starting_dose_model)
  if (length(starting_dose_rule) == 0) stop("starting dose not in 'route through doses'")

  ###################################

  p_truth = plot_truth(doses = doses, truth = truth)

  ###################################

  model_sims = sim_study(doses = doses,
                         truth = truth,
                         max_n_cohort = max_n_cohort,
                         cohort_size = cohort_size,
                         n_sims = n_sims,
                         starting_dose = starting_dose,
                         max_cohort_size = max_cohort_size,
                         max_n = max_n,
                         max_p_target = max_p_target,
                         add_random = add_random,
                         frac_target = frac_target,
                         over_limit_end = over_limit_end, 
                         n_iter = n_iter)

  model_sims_sum = sum_sims(model_sims, truth = truth)

  ###################################

  emptydata <- Data(doseGrid = 1:length(route_through_doses))

  nominal_truth <- function(dose) truth[route_through_doses][dose]

  rule_design <- RuleDesign(nextBest = NextBestThreePlusThree(),
                            cohortSize = CohortSizeConst(size=3L),
                            data = emptydata,
                            startingDose = starting_dose_rule)

  rule_sims <- crmPack::simulate(rule_design,nsim=n_sims,seed=seed,truth=nominal_truth,parallel=FALSE)
  rule_sims_sum <- crmPack::summary(rule_sims,truth=nominal_truth)

  ####################################

  p_comparison = compare_all(model_sims_sum, rule_sims_sum)

  ####################################

  p_selected = plot_selected(doses = doses,
                             route_through_doses = route_through_doses,
                             truth = truth,
                             model_sim = model_sims_sum,
                             rule_sim = rule_sims_sum)

  ####################################

  list(p_truth = p_truth,
       p_selected = p_selected,
       p_comparison = p_comparison,
       model_sims_sum = model_sims_sum,
       rule_sims_sum = rule_sims_sum)

}
