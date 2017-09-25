#' Plot how often each dose combination is selected
#'
#' @param doses List of doses on original and log scales.
#' @param route_through_doses A vector showing order to go through doses.
#' @param truth The 'true' probabilities of DLT at the dose combinations
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export
#'

plot_selected = function(doses, route_through_doses, truth, model_sim, rule_sim){
  
  
  model_df = data.frame(dose_1 = doses$orig[model_sim$mtds,1],
                        dose_2 = doses$orig[model_sim$mtds,2],
                        method = "model")
  
  rule_dose_1_manual = rep(NA, length(rule_sim@doseSelected)) 
  rule_dose_1_manual[rule_sim@doseSelected != 0] = doses$orig[route_through_doses,1][rule_sim@doseSelected]
  
  rule_dose_2_manual = rep(NA, length(rule_sim@doseSelected)) 
  rule_dose_2_manual[rule_sim@doseSelected != 0] = doses$orig[route_through_doses,2][rule_sim@doseSelected]
  
  rule_df = data.frame(dose_1 = rule_dose_1_manual,
                       dose_2 = rule_dose_2_manual,
                       method = "3 + 3")
  
  y = max(c(max(table(model_df)),max(table(rule_df)))) / 2
  
  
  df = rbind(model_df, rule_df)
  
  truth_table = as.data.frame(cbind(doses$orig, round(truth, 2), 1.5, y))
  names(truth_table) = c("dose_1", "dose_2", "truth", "x", "y")
  
  ##########################
  ## order on plot
  
  levels_1 = sort(unique(doses$orig[,1]))
  levels_2 = sort(unique(doses$orig[,2]))
  
  truth_table$dose_1 = factor(truth_table$dose_1, levels = rev(levels_1))
  truth_table$dose_2 = factor(truth_table$dose_2, levels = levels_2)
  
  df$dose_1 = factor(df$dose_1, levels = rev(levels_1))
  df$dose_2 = factor(df$dose_2, levels = levels_2)
  
  
  p = ggplot() +
    geom_bar(data = df,
             mapping = aes(x=method,fill = method),
             stat = "count", 
             position= "dodge") +
    facet_grid(dose_1 ~ dose_2) +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.y = element_text(angle = 0)) +
    geom_text(data = truth_table,
              mapping = aes(x = x, y = y, label = truth))
  
  
  p
  
}
