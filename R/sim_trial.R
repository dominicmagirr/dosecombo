#' Simulate a combination dose finding trial.
#'
#'
#' @param doses A list containing doses on the original and log scale.
#' @param truth A vector of 'true' p(DLT)'s, corresponding to the rows of 'log_doses'.
#' @param max_n_cohort The maximum number of cohorts.
#' @param cohort_size Cohort size.
#' @param starting_dose Starting dose.
#' @return A vector containing the number of patients dosed, the number of dlts, and the recommended MTD.
#' @export

sim_trial <- function(doses,
                      truth,
                      max_n_cohort,
                      cohort_size,
                      starting_dose){

  sim_dat = data.frame(dose_1 = doses$log_scale[,1],
                       dose_2 = doses$log_scale[,2],
                       n = 0,
                       n_dlt = 0)

  next_dose = dose_row(doses, starting_dose, orig_scale = TRUE)

  n_cohort = 0
  while(n_cohort < max_n_cohort){

    sim_dat[next_dose,"n"] <- sim_dat[next_dose,"n"] + cohort_size
    sim_dat[next_dose,"n_dlt"] <- sim_dat[next_dose,"n_dlt"] + rbinom(1,cohort_size,truth[next_dose])



    s = get_samples(sim_dat,  prior = doses$prior)
    new_info = apply(doses$log_scale, 1, get_info, s = s)

    current_dose = next_dose
    next_dose = get_next_dose(new_info, doses = doses, current_dose = current_dose)
    # stop if no doses safe:
    if (is.na(next_dose)) return(list(sim_dat = sim_dat, mtd = next_dose))
    # stop if already 6 patients at current dose AND already 15 patients in total
    if ((sim_dat[next_dose,"n"] >= 6) && (sum(sim_dat[,"n"]) >= 15)) return(list(sim_dat = sim_dat, mtd = next_dose))
    # stop if already 6 patients at current dose AND already a 50% chance of target toxicity
    if ((sim_dat[next_dose,"n"] >= 6) && (new_info[5,next_dose] > 0.5)) return(list(sim_dat = sim_dat, mtd = next_dose))

    n_cohort = n_cohort + 1


  }
  #return(c(n = sum(sim_dat$n), n_dlt = sum(sim_dat$n_dlt), mtd = next_dose))
  return(list(sim_dat = sim_dat, mtd = next_dose))

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
