#' Create a prior for combination dose response model from 2 crmPack model objects
#'
#' @param model_1 First crmPack model: prior for drug 1.
#' @param model_2 Second crmPack model: prior for drug 2.
#' @param eta_mean Prior mean for the interaction parameter
#' @param eta_prec Prior precision for the interaction parameter.
#' @return A list of prior means/variances for parameters in the combination dose-response model.
#' @export


create_combo_prior = function(model_1,
                              model_2,
                              eta_mean = 0,
                              eta_prec = 0.5){
  
  
  prior = list(mu_1 = model_1$model@mean,
               mu_2 = model_2$model@mean,
               omega_1 = solve(model_1$model@cov),
               omega_2 = solve(model_2$model@cov),
               eta_mean = eta_mean,
               eta_prec = eta_prec)
 
  
}