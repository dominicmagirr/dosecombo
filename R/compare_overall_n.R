#' Plot the sample sizes of a series of simulated trials.
#'
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export
#'

compare_overall_n <- function(model_sum, rule_sum){
  
  n_sims = length(model_sum$mtds)
  n_sims_rule = length(rule_sum@doseSelected)
  
  if (n_sims != n_sims_rule) stop("Need same number of simulations: model vs. 3 + 3")
  
  
  
  model_n = data.frame(table(model_sum$overall_n),method = "model")
  rule_n = data.frame(table(rule_sum@nObs),method = "3 + 3")
  
  full_n = rbind(model_n, rule_n)
  
  
  full_n = rbind(full_n, cbind(expand.grid(Var1 = levels(full_n$Var1), method = c("model", "3 + 3")), Freq = NA))
  
  #full_n$Var1 = factor(full_n$Var1,
  #                       levels = as.character(sort(as.numeric(as.character(levels(full_n$Var1))))))
  
  full_n$Var1 = as.numeric(as.character(full_n$Var1))
  
  p = ggplot(full_n, aes(x = Var1, y = Freq, fill = method)) +
    geom_bar(stat = "identity",position = "dodge") +
    xlab("n") +
    theme(legend.position="top") 
  
  
  p
  
}
