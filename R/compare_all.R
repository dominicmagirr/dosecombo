#' Graphical comparison of a model-based design versus a rule-based design
#'
#' @param model_sim a summary of the model based simulation -- from 'sum_sims'.
#' @param rule_sim a summary of the model based simulation -- an object from 'crmPack'.
#' @return A ggplot object.
#' @export


compare_all = function(model_sum, rule_sum){
  
  #########################################################
  p_true_tox = compare_true_tox(model_sum, rule_sum)
  p_overall_n = compare_overall_n(model_sum, rule_sum)
  p_prop_dlts = compare_prop_dlts(model_sum, rule_sum)
  p_over_target = compare_over_target(model_sum, rule_sum)
  #########################################################
  
  p_all = arrangeGrob(p_true_tox,
                       p_overall_n,
                       p_prop_dlts,
                       p_over_target,
                       ncol = 2)
  
  p_all
}