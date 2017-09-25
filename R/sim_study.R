#' Simulate multiple studies and collect results
#'
#' @param doses  A list containing doses on the original and log scale.
#' @param truth A vector of 'true' p(DLT)'s, corresponding to the rows of 'log_doses'.
#' @param max_n_cohort The maximum number of cohorts.
#' @param cohort_size The number of patients per cohort.
#' @param n_sims The number of simulations.
#' @param max_cohort_size Maximum size of cohort. If this is reached, and the max_n or max_p_target is reached then the trial can stop.
#' @param max_n Maximum total sample size. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param max_p_target Maximum probability of targeted toxicity. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param add_random Should some randomness be added to choice of next dose? Default is FALSE.
#' @param frac_target If randomness is added, how close to the 'best' dose is acceptable. Default is 75 percent of P(target toxicity).
#' @param over_limit_end The safety limit to be applied at the end of the trial
#' @param n_iter Number of iterations in MCMC.
#' @return A matrix of results. 3 columns: n, n_dlt, mtd. One row per simulation.
#' @export
#'

sim_study = function(doses,
                     truth,
                     max_n_cohort,
                     cohort_size,
                     n_sims,
                     starting_dose,
                     max_cohort_size,
                     max_n,
                     max_p_target,
                     add_random = FALSE,
                     frac_target = 0.75,
                     over_limit_end = NA, 
                     n_iter = 2500){


  prior_info = apply(doses$log_scale, 1, get_prior_info, prior = doses$prior)


  results = as.list(1:n_sims)

    for (i in 1:n_sims){

  results[[i]] = sim_trial(doses = doses,
                          truth = truth,
                          max_n_cohort = max_n_cohort,
                          cohort_size = cohort_size,
                          starting_dose = starting_dose,
                          max_cohort_size = max_cohort_size,
                          max_n = max_n,
                          max_p_target = max_p_target,
                          add_random = add_random,
                          frac_target = frac_target,
                          over_limit_end = over_limit_end, 
                          n_iter = n_iter)



    }

  results
}



