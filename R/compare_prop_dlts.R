#' Plot the proportion of DLTs in a series of simulated trials.
#'
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export
#'
compare_prop_dlts <- function(model_sum, rule_sum){
  
  n_sims = length(model_sum$mtds)
  n_sims_rule = length(rule_sum@doseSelected)
  
  if (n_sims != n_sims_rule) stop("Need same number of simulations: model vs. 3 + 3")
  
  model_nearest_10 = round(model_sum$prop_dlts * 100 / 10) * 10 / 100
  rule_nearest_10 = round(rule_sum@propDLTs * 100 / 10) * 10 / 100
  
  
  model_prop = data.frame(table(model_nearest_10),method = "model")
  names(model_prop)[1] = "prop"
  rule_prop = data.frame(table(rule_nearest_10),method = "3 + 3")
  names(rule_prop)[1] = "prop"
  
  full_prop = rbind(model_prop, rule_prop)
  
  
  full_prop = rbind(full_prop, cbind(expand.grid(prop = levels(full_prop$prop), method = c("model", "3 + 3")), Freq = NA))
  
  #full_prop$Var1 = factor(full_prop$Var1,
  #                         levels = as.character(sort(as.numeric(as.character(levels(full_prop$Var1))))))
  
  full_prop$prop = as.numeric(as.character(full_prop$prop))
  
  p = ggplot(full_prop, aes(x = prop, y = Freq, fill = method)) +
    geom_bar(stat = "identity",position = "dodge") +
    xlab("Proportion of DLTs") +
    theme(legend.position="top") +
    scale_x_continuous(breaks = unique(full_prop$prop))
  
  
  
  p
  
  
  
}

