#' Find prior parameters for an increasing logistic curve
#'
#' @param drug_1_dose_1 A dose level for drug 1. On original scale.
#' @param drug_1_dose_1_best_guess The best guess for p(DLT) at 'dose_1'.
#' @param drug_1_dose_2 A dose level for drug 2. On original scale. Must be the same as 'd_star[1]'.
#' @param drug_1_dose_2_best_guess The best guess for p(DLT) at 'dose_2'.
#' @param drug_1_n_prior The number of prior observations that is roughly equivalent to strength of belief.
#' @param drug_2_dose_1 A dose level for drug 1. On original scale.
#' @param drug_2_dose_1_best_guess The best guess for p(DLT) at 'dose_1'.
#' @param drug_2_dose_2 A dose level for drug 2. On original scale. Must be the same as 'd_star[2]'.
#' @param drug_2_dose_2_best_guess The best guess for p(DLT) at 'dose_2'.
#' @param drug_2_n_prior The number of prior observations that is roughly equivalent to strength of belief.
#' @param interaction_sd The standard deviation of the interaction parameter.
#' @return A vector of prior parameters.
#' @export

choose_prior <- function(drug_1_dose_1,
                  drug_1_dose_1_best_guess,
                  drug_1_dose_2,
                  drug_1_dose_2_best_guess,
                  drug_1_n_prior,
                  drug_2_dose_1,
                  drug_2_dose_1_best_guess,
                  drug_2_dose_2,
                  drug_2_dose_2_best_guess,
                  drug_2_n_prior,
                  interaction_sd){

  drug_1_params = prior_for_curve(dose_1 = drug_1_dose_1,
                                  dose_2 = drug_1_dose_2,
                                  n_prior = drug_1_n_prior,
                                  best_guess_1 = drug_1_dose_1_best_guess,
                                  best_guess_2 = drug_1_dose_2_best_guess)

  drug_2_params = prior_for_curve(dose_1 = drug_2_dose_1,
                                  dose_2 = drug_2_dose_2,
                                  n_prior = drug_2_n_prior,
                                  best_guess_1 = drug_2_dose_1_best_guess,
                                  best_guess_2 = drug_2_dose_2_best_guess)

  prior = list(mu_1 = c(drug_1_params["b_0"], drug_1_params["log_b_1"]),
               mu_2 = c(drug_2_params["b_0"], drug_2_params["log_b_1"]),
               omega_1 = matrix(c(drug_1_params["b_0_p"],0,0,drug_1_params["log_b_1_p"]), nrow = 2, ncol = 2),
               omega_2 = matrix(c(drug_2_params["b_0_p"],0,0,drug_2_params["log_b_1_p"]), nrow = 2, ncol = 2),
               eta_mean = 0,
               eta_prec = 1 / interaction_sd ^ 2)

}

prior_for_curve <- function(dose_1,
                            dose_2,
                            n_prior = 2,
                            best_guess_1 = 0.05,
                            best_guess_2 = 0.5){

  log_dose_1 = log(dose_1/dose_2)

  upper_1 = qbeta(0.975, best_guess_1 * n_prior, (1 - best_guess_1) * n_prior)
  upper_2 = qbeta(0.975, best_guess_2 * n_prior, (1 - best_guess_2) * n_prior)

  b_0 <- log(best_guess_2 / (1 - best_guess_2))


  b_0_u <- log(upper_2 / (1 - upper_2))
  b_0_s <- abs(b_0_u - b_0) / 1.96
  b_0_p <- 1 / b_0_s ^ 2

  log_b_1 <- log((-b_0 + log(best_guess_1 / (1 - best_guess_1))) / log_dose_1)


  log_b_1_u <- log((-b_0 + log(upper_1 / (1 - upper_1))) / log_dose_1)
  log_b_1_s <- abs(log_b_1_u - log_b_1) / 1.96
  log_b_1_p <- 1 / log_b_1_s ^ 2

  c(b_0 = b_0, b_0_p = b_0_p, log_b_1 = log_b_1, log_b_1_p = log_b_1_p)

}
