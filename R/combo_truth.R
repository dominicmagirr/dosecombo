#' Produce a vector of "true" p(DLT) at each dose combination
#'
#'
#' @param doses A list containing doses on the original and log scale.
#' @param ref_doses Vector of length 2. Doses of agent 1 and agent 2 used as reference for standardization. Does not have to match the reference doses in the 'doses' object.
#' @param ref_probs Vector of length 2. Probability of DLT at "ref_doses".
#' @param other_doses Vector of length 2. Doses of agent 1 and agent 2. Any choices valid, apart from the "ref_doses".
#' @param other_probs Vector of length 2. Probability of DLT at "other_doses".
#' @param odds_multiplier Interaction effect, equivalent to exp(eta). In the model, the odds of DLT at "ref_doses" gets multiplied by "odds_multiplier".
#' @return A vector containing p(DLT) at each of the dose combinations in "doses".
#' @export
#'

combo_truth <- function(doses,
                        ref_doses = c(75, 40),
                        ref_probs = c(0.25, 0.2),
                        other_doses = c(25, 10),
                        other_probs = c(0.005, 0.005),
                        odds_multiplier = 9){

  alpha_0_1 = qlogis(ref_probs[1])
  alpha_0_2 = qlogis(ref_probs[2])

  alpha_1_1 = (qlogis(other_probs[1]) - qlogis(ref_probs[1])) / log(other_doses[1] / ref_doses[1])
  alpha_1_2 = (qlogis(other_probs[2]) - qlogis(ref_probs[2])) / log(other_doses[2] / ref_doses[2])

  p_dlt <- function(d){

    odds_1 = exp(alpha_0_1 + alpha_1_1 * log(d[1] / ref_doses[1]))
    odds_2 = exp(alpha_0_2 + alpha_1_2 * log(d[2] / ref_doses[2]))

    odds_12 <- (odds_1 + odds_2 + odds_1 * odds_2) * exp(log(odds_multiplier) * (d[1] / ref_doses[1]) * (d[2] / ref_doses[2]))

    p_12 <- odds_12 / (1 + odds_12)

    p_12
  }

  apply(doses$orig, 1, p_dlt)

}
