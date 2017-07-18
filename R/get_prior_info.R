#' Find quantiles of p(DLT) and probabilites of toxicity
#'
#' Provided with a matrix of dose combinations and the prior, this function returns the
#' quantiles of p(DLT), the probability that p(DLT) < 0.2, the probability that p(DLT) in (0.2,0.35);
#' and the  probability that p(DLT) > 0.35.
#' @param dose A vector:  dose combination. Log-transformed doses: must match what's used in the JAGS model.
#' @param prior A list of prior parameters
#' @param n_sim The number of times to simulate from the prior
#' @param seed Random number seed.
#' @return A vector of length 8. The first 2 elements simply return the dose levels. The 3rd -- 5th elements
#' are the 0.1,0.5 and 0.9 quantiles of the distribution of p(DLT). The 6th -- 8th elements are the probabilities
#' of underdosing, target-dosing and overdosing, respectively.
#' @export

get_prior_info = function(dose, prior, n_sim = 1000, seed = 1987){

  set.seed(seed)

  b_1 = rmvnorm(n_sim, mean = prior$mu_1, sigma = solve(prior$omega_1))
  b_2 = rmvnorm(n_sim, mean = prior$mu_2, sigma = solve(prior$omega_2))
  eta = rnorm(n_sim, mean = prior$eta_mean, sd = sqrt(1 / prior$eta_prec))

  odds_1 = exp(b_1[,1] + exp(b_1[,2]) * dose[1])
  odds_2 = exp(b_2[,1] + exp(b_2[,2]) * dose[2])

  odds_12 = (odds_1 + odds_2 + odds_1 * odds_2) * exp(eta * exp(dose[1]) * exp(dose[2]))
  p_12 = odds_12 / (1 + odds_12)

  quants = quantile(p_12, probs = c(0.1, 0.5, 0.9))

  p_under = mean(p_12 < 0.2)
  p_over = mean(p_12 > 0.35)
  p_target = 1 - p_under - p_over

  c(quants, p_under, p_target, p_over)
}
