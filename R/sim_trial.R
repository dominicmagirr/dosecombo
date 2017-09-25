#' Simulate a combination dose finding trial.
#'
#'
#' @param doses A list containing doses on the original and log scale.
#' @param truth A vector of 'true' p(DLT)'s, corresponding to the rows of 'log_doses'.
#' @param max_n_cohort The maximum number of cohorts.
#' @param cohort_size Cohort size.
#' @param starting_dose Starting dose.
#' @param max_cohort_size Maximum size of cohort. If this is reached, and the max_n or max_p_target is reached then the trial can stop.
#' @param max_n Maximum total sample size. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param max_p_target Maximum probability of targeted toxicity. If this is reached and the max_cohort_size is reached for recommended dose then the trial can stop.
#' @param add_random Should some randomness be added to choice of next dose? Default is FALSE.
#' @param frac_target If randomness is added, how close to the 'best' dose is acceptable. Default is 75 percent of P(target toxicity).
#' @param over_limit_end The safety limit to be applied at the end of the trial
#' @param n_iter Number of iterations in MCMC.
#' @return A vector containing the number of patients dosed, the number of dlts, and the recommended MTD.
#' @export

sim_trial <- function(doses,
                      truth,
                      max_n_cohort,
                      cohort_size,
                      starting_dose,
                      max_cohort_size,
                      max_n,
                      max_p_target,
                      add_random = FALSE,
                      frac_target = 0.75,
                      over_limit_end = NA, 
                      n_iter = 2500){

  sim_dat = data.frame(dose_1 = doses$log_scale[,1],
                       dose_2 = doses$log_scale[,2],
                       n = 0,
                       n_dlt = 0)

  next_dose = dose_row(doses, starting_dose, orig_scale = TRUE)

  n_cohort = 0
  
  doses_tried   = rep(NA, max_n)
  observed_dlts = rep(NA, max_n)
  
  while(n_cohort < max_n_cohort){
    
    doses_tried[(n_cohort * cohort_size + 1):((n_cohort + 1) * cohort_size)] = next_dose
    
    cohort_dlts = rbinom(1,cohort_size,truth[next_dose])
    
    observed_dlts[(n_cohort * cohort_size + 1):((n_cohort + 1) * cohort_size)] = c(rep(0, cohort_size - cohort_dlts), rep(1, cohort_dlts))

    sim_dat[next_dose,"n"] <- sim_dat[next_dose,"n"] + cohort_size
    
    sim_dat[next_dose,"n_dlt"] <- sim_dat[next_dose,"n_dlt"] + cohort_dlts


    s = get_samples(sim_dat,  prior = doses$prior, n_iter = n_iter)
    new_info = apply(doses$log_scale, 1, get_info, s = s)

    current_dose = next_dose
    next_dose = get_next_dose(new_info,
                              doses = doses,
                              current_dose = current_dose,
                              add_random = add_random,
                              frac_target = frac_target)

    ######################################
    ## Old combination of stopping rules
    # stop if no doses safe:
    ##if (is.na(next_dose)) return(list(sim_dat = sim_dat, mtd = next_dose))
    # stop if already max_cohort_size patients at current dose AND already max_n patients in total
    ##if ((sim_dat[next_dose,"n"] >= max_cohort_size) && (sum(sim_dat[,"n"]) >= max_n)) return(list(sim_dat = sim_dat, mtd = next_dose))
    # stop if already max_cohort_size patients at current dose AND already a max_p_target * 100% chance of target toxicity
    ##if ((sim_dat[next_dose,"n"] >= max_cohort_size) && (new_info[5,next_dose] > max_p_target)) return(list(sim_dat = sim_dat, mtd = next_dose))
    ######################################

    ######################################
    ## New combination of stopping rules
    # stop if no doses safe:
    if (is.na(next_dose)) return(list(sim_dat = sim_dat, mtd = next_dose))
    # stop if already max_cohort_size patients at current dose
    if (sim_dat[next_dose,"n"] >= max_cohort_size){
      if (!is.na(over_limit_end)){
        doses$over_limit = over_limit_end
        next_dose = get_next_dose(new_info,
                                  doses = doses,
                                  current_dose = current_dose,
                                  add_random = add_random,
                                  frac_target = frac_target)
      }
      return(list(sim_dat = sim_dat, 
                  mtd = next_dose,
                  doses_tried = doses_tried,
                  observed_dlts = observed_dlts))
    }
    # stop if max n already reached
    if (sum(sim_dat[,"n"]) >= max_n){
      if (!is.na(over_limit_end)){
        doses$over_limit = over_limit_end
        next_dose = get_next_dose(new_info,
                                  doses = doses,
                                  current_dose = current_dose,
                                  add_random = add_random,
                                  frac_target = frac_target)
      }
      return(list(sim_dat = sim_dat, 
                  mtd = next_dose,
                  doses_tried = doses_tried,
                  observed_dlts = observed_dlts))
    }
    # stop if already a max_p_target * 100% chance of target toxicity
    if (new_info[5,next_dose] > max_p_target) {
      if (!is.na(over_limit_end)){
        doses$over_limit = over_limit_end
        next_dose = get_next_dose(new_info,
                                  doses = doses,
                                  current_dose = current_dose,
                                  add_random = add_random,
                                  frac_target = frac_target)
      }
      return(list(sim_dat = sim_dat,
                  mtd = next_dose,
                  doses_tried = doses_tried,
                  observed_dlts = observed_dlts))
    }
    ######################################
    n_cohort = n_cohort + 1


  }
  #return(c(n = sum(sim_dat$n), n_dlt = sum(sim_dat$n_dlt), mtd = next_dose))
  return(list(sim_dat = sim_dat, 
              mtd = next_dose,
              doses_tried = doses_tried,
              observed_dlts = observed_dlts))

}
#
# ############
# sim_trial <- function(doses,
#                       truth,
#                       max_n_cohort,
#                       over_limit,
#                       prior,
#                       cohort_size,
#                       starting_dose = 1){
#
#   sim_dat = data.frame(dose_1 = doses$log_scale[,1],
#                        dose_2 = doses$log_scale[,2],
#                        n = 0,
#                        n_dlt = 0)
#
#   next_dose = starting_dose
#
#   n_cohort = 0
#   while(n_cohort < max_n_cohort){
#
#     sim_dat[next_dose,"n"] <- sim_dat[next_dose,"n"] + cohort_size
#     sim_dat[next_dose,"n_dlt"] <- sim_dat[next_dose,"n_dlt"] + rbinom(1,cohort_size,truth[next_dose])
#
#
#
#     s = get_samples(sim_dat,  prior = prior)
#     new_info = apply(doses$log_scale, 1, get_info, s = s)
#
#     current_dose = next_dose
#     next_dose = get_next_dose(new_info, over_limit = over_limit, doses = doses$orig, current_dose = current_dose)
#     # stop if no doses safe:
#     if (is.na(next_dose)) return(list(sim_dat = sim_dat, mtd = next_dose))
#     # stop if already 6 patients at current dose AND already 15 patients in total
#     if ((sim_dat[next_dose,"n"] >= 6) && (sum(sim_dat[,"n"]) >= 15)) return(list(sim_dat = sim_dat, mtd = next_dose))
#     # stop if already 6 patients at current dose AND already a 50% chance of target toxicity
#     if ((sim_dat[next_dose,"n"] >= 6) && (new_info[5,next_dose] > 0.5)) return(list(sim_dat = sim_dat, mtd = next_dose))
#
#     n_cohort = n_cohort + 1
#
#
#   }
#   #return(c(n = sum(sim_dat$n), n_dlt = sum(sim_dat$n_dlt), mtd = next_dose))
#   return(list(sim_dat = sim_dat, mtd = next_dose))
#
# }
#
# ############
