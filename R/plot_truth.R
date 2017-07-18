#' Plot the 'true' p(DLT) at all dose combinations
#'
#' @param doses List of doses on original and log scales.
#' @param truth A vector. The 'true' p(DLT), corresponding to the rows of 'doses'.
#' @return A ggplot object.
#' @export

plot_truth = function(doses, truth){

  truth_table = as.data.frame(cbind(doses$orig, round(truth, 2), "1", "1"))
  names(truth_table) = c("dose_1", "dose_2", "truth", "x", "y")

  ##########################
  ## order on plot

  levels_1 = sort(unique(doses$orig[,1]))
  levels_2 = sort(unique(doses$orig[,2]))

  truth_table$dose_1 = factor(truth_table$dose_1, levels = rev(levels_1))
  truth_table$dose_2 = factor(truth_table$dose_2, levels = levels_2)

  truth_table = truth_table %>%
    mutate(truth_numeric = as.numeric(as.character(truth)),
           target = ifelse(truth_numeric < 0.2, "under", ifelse(truth_numeric > 0.35, "over", "target")))


  p = ggplot(truth_table,aes(x=x,y=y,label=truth, colour = target)) +
    geom_text() +
    facet_grid(dose_1 ~ dose_2) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())


  p

}


