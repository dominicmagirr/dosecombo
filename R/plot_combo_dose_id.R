#' Display the id number of each dose combination.
#'
#' @param doses List of doses on original and log scales.
#' @return A ggplot object.
#' @export

plot_combo_dose_id = function(doses){

  route_table = as.data.frame(cbind(doses$orig, 1:(dim(doses$orig)[1]), "1", "1"))
  names(route_table) = c("dose_1", "dose_2", "route", "x", "y")

  ##########################
  ## order on plot

  levels_1 = sort(unique(doses$orig[,1]))
  levels_2 = sort(unique(doses$orig[,2]))

  route_table$dose_1 = factor(route_table$dose_1, levels = rev(levels_1))
  route_table$dose_2 = factor(route_table$dose_2, levels = levels_2)

  p = ggplot(route_table,aes(x=x,y=y,label=route)) +
    geom_text() +
    facet_grid(dose_1 ~ dose_2) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())


  p

}
