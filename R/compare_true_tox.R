#' Plot the true toxicity probabilities at the selected doses
#'
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export
#'
compare_true_tox <- function(model_sum, rule_sum){
  
  tox_category <- function(tox){
    tox[is.na(tox)] = 0
    result = character(length(tox))
    for (i in seq_along(tox)){
      if (tox[i] == 0) result[i] = "0"
      else if (tox[i] <= 0.1) result[i] = "(0,10]"
      else if (tox[i] <= 0.2) result[i] = "(10,20]"
      else if (tox[i] <= 0.35) result[i] = "(20,35]"
      else if (tox[i] <= 0.5) result[i] = "(35,50]"
      else result[i] = "(50,100]"
    }
    result
  }
  
  n_sims = length(model_sum$mtds)
  n_sims_rule = length(rule_sum@doseSelected)
  
  if (n_sims != n_sims_rule) stop("Need same number of simulations: model vs. 3 + 3")
  
  no_mtd_model = sum(is.na(model_sum$mtds))
  no_mtd_rule = sum(rule_sum@doseSelected == 0)
  
  
  
  model_tox = data.frame(table(tox_category(model_sum$tox_at_selected)),method = "model")
  rule_tox = data.frame(table(tox_category(rule_sum@toxAtDosesSelected)),method = "3 + 3")
  
  full_tox = rbind(model_tox, rule_tox)
  
  
  full_tox = rbind(full_tox, cbind(expand.grid(Var1 = c("0",
                                                        "(0,10]",
                                                        "(10,20]",
                                                        "(20,35]",
                                                        "(35,50]",
                                                        "(50,100]"), method = c("model", "3 + 3")), Freq = NA))
  
  #full_tox$Var1 = factor(full_tox$Var1,
  #                       levels = as.character(sort(as.numeric(as.character(levels(full_tox$Var1))))))
  
  full_tox$Var1 = factor(full_tox$Var1, levels = c("0",
                                                   "(0,10]",
                                                   "(10,20]",
                                                   "(20,35]",
                                                   "(35,50]",
                                                   "(50,100]"))
  
  p = ggplot(full_tox, aes(x = Var1, y = Freq, fill = method)) +
    geom_bar(stat = "identity",position = "dodge") +
    xlab("'True'  %DLT at selected dose") +
    theme(legend.position="top") + 
    theme(axis.text.x = element_text(face=ifelse(levels(full_tox$Var1) == "(20,35]","bold","plain")))
  
  
  
  p
  
}
######