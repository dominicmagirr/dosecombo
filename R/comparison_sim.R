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
#' @return A list of 4 items: a plot of the true p(DLTs), a graphical comparison of the 2 methods, a summary of model-based simulations, and a summary of rule-based simulations.
#' @export
#'

comparison_sim = function(truth,
                          doses, starting_dose, route_through_doses, 
                          max_n_cohort,cohort_size,
                          n_sims, seed){
  
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
                         starting_dose = starting_dose)
  
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
  
  list(p_truth = p_truth, 
       p_comparison = p_comparison, 
       model_sims_sum = model_sims_sum,
       rule_sims_sum = rule_sims_sum)
  
}
