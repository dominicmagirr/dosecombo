#' Find quantiles of p(DLT) and probabilites of toxicity
#'
#' Provided with a matrix of dose combinations and JAGS output from model, this function returns the
#' quantiles of p(DLT), the probability that p(DLT) < 0.2, the probability that p(DLT) in (0.2,0.35);
#' and the  probability that p(DLT) > 0.35.
#' @param dose A vector: dose combination. Log-transformed, and must match what's used in the JAGS model.
#' @param s JAGS output from fitting the model.
#' @return A vector of length 8. The first 2 elements simply return the dose levels. The 3rd -- 5th elements
#' are the 0.1,0.5 and 0.9 quantiles of the distribution of p(DLT). The 6th -- 8th elements are the probabilities
#' of underdosing, target-dosing and overdosing, respectively.
#' @export

get_info <- function(dose, s){

  odds_1 = exp(s[,"b_1[1]"][[1]] + exp(s[,"b_1[2]"][[1]]) * dose[1])
  odds_2 = exp(s[,"b_2[1]"][[1]] + exp(s[,"b_2[2]"][[1]]) * dose[2])

  odds_12 = (odds_1 + odds_2 + odds_1 * odds_2) * exp(s[,"eta"][[1]] * exp(dose[1]) * exp(dose[2]))

  p_12 = odds_12 / (1 + odds_12)

  quants = quantile(p_12, probs = c(0.1, 0.5, 0.9))

  p_under = mean(p_12 < 0.2)
  p_over = mean(p_12 > 0.35)
  p_target = 1 - p_under - p_over

  c(quants, p_under, p_target, p_over)
}
