#' Use JAGS to get simulations from posterior
#'
#' Given data and a prior distribution, this function gives back samples from the posterior distribution.
#' @param df Data frame containing the data
#' @param d_star A vector of reference levels. Length 2.
#' @param prior A list of prior parameters.
#' @param n_iter Number of iterations in MCMC.
#' @return A list, containing samples from the posterior distribution.
#' @export

get_samples = function(df, prior, n_iter = 2500){

  df_long = binom_2_bern(df)

  dose_1 = df_long$dose_1
  dose_2 = df_long$dose_2
  dlt = df_long$dlt
  n = length(dlt)

  data <- list("dose_1" = dose_1,
               "dose_2" = dose_2,
               "dlt" = dlt,
               "n" = n,
               "mu_1" = prior$mu_1,
               "mu_2" = prior$mu_2,
               "omega_1" = prior$omega_1,
               "omega_2" = prior$omega_2,
               "eta_mean" = prior$eta_mean,
               "eta_prec" = prior$eta_prec)

  inits <- list(b_1 = c(0,0),
                b_2 = c(0,0),
                eta = 0)

  m <- jags.model(system.file("extdata",
                              "jags_combo_odds_bivariate.txt",
                              package = "DoseCombo"),
                  data = data,
                  inits = inits,
                  n.chains = 1,
                  n.adapt = 2000, quiet = TRUE)

  #update(m, 1000)

  s <- coda.samples(m,
                    c("b_1", "b_2", "eta"),
                    n.iter = n_iter,
                    thin = 5,
                    progress.bar = "none")

  s

}





