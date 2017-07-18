#' Plot the number of patients treated above the 'target' toxicity level
#'
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export
#'

compare_over_target <- function(model_sum, rule_sum){
  
  n_sims = length(model_sum$mtds)
  n_sims_rule = length(rule_sum@doseSelected)
  
  if (n_sims != n_sims_rule) stop("Need same number of simulations: model vs. 3 + 3")
  
  
  model_above = data.frame(table(model_sum$over_target),method = "model")
  rule_above = data.frame(table(rule_sum@nAboveTarget),method = "3 + 3")
  
  full_above = rbind(model_above, rule_above)
  
  
  full_above = rbind(full_above, cbind(expand.grid(Var1 = levels(full_above$Var1), method = c("model", "3 + 3")), Freq = NA))
  
  #full_above$Var1 = factor(full_above$Var1,
  #                       levels = as.character(sort(as.numeric(as.character(levels(full_above$Var1))))))
  full_above$Var1 = as.numeric(as.character(full_above$Var1))
  
  p = ggplot(full_above, aes(x = Var1, y = Freq, fill = method)) +
    geom_bar(stat = "identity",position = "dodge") +
    xlab("n treated above target") +
    theme(legend.position="top") +
    scale_x_continuous(breaks = unique(full_above$Var1))
  
  
  p
  
}
