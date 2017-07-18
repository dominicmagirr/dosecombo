#' Simulate multiple studies and collect results
#'
#' @param doses  A list containing doses on the original and log scale.
#' @param truth A vector of 'true' p(DLT)'s, corresponding to the rows of 'log_doses'.
#' @param max_n_cohort The maximum number of cohorts.
#' @param cohort_size The number of patients per cohort.
#' @param n_sims The number of simulations.
#' @return A matrix of results. 3 columns: n, n_dlt, mtd. One row per simulation.
#' @export
#'

sim_study = function(doses,
                     truth,
                     max_n_cohort,
                     cohort_size,
                     n_sims,
                     starting_dose){


  prior_info = apply(doses$log_scale, 1, get_prior_info, prior = doses$prior)


  results = as.list(1:n_sims)

    for (i in 1:n_sims){

  results[[i]] = sim_trial(doses = doses,
                          truth = truth,
                          max_n_cohort = max_n_cohort,
                          cohort_size = cohort_size,
                          starting_dose = starting_dose)

  }
  results
}



