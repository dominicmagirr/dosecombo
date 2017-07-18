#' Display the id number of each dose combination with route through doses.
#'
#' @param doses List of doses on original and log scales.
#' @param route_through_doses A vector showing order to go through doses.
#' @param start_rule Index of 'route_through_doses' corresponding to the starting dose.
#' @return A ggplot object.
#' @export

plot_route_through_doses = function(doses, route_through_doses, start_rule){
  
  my_z = route_through_doses
  
  n_1 = length(unique(doses$orig[,1]))
  n_2 = length(unique(doses$orig[,2]))
  
  p <- plot_combo_dose_id(doses)
  pb <- ggplot_build(p)
  pg <- ggplot_gtable(pb)
  
  str(pg$layout)
  panels = pg$layout %>% filter(str_detect(name, "panel"))
  ###############
  my_l = min(panels$l)
  my_r = max(panels$r)
  my_b = min(panels$b)
  my_t = max(panels$t)
  ###############
  xs = 1:n_1 / n_1 - 1 / (n_1 * 2)
  ys = 1:n_2 / n_2 - 1 / (n_2 * 2)
  
  my_x = xs[ifelse(my_z %% n_2,my_z %% n_2,4)]
  my_y = ys[ceiling(my_z / n_2)]
  
  
  pg <- gtable_add_grob(pg, segmentsGrob(x0 = my_x[-length(my_x)], 
                                         x1 = my_x[-1], 
                                         y0 = my_y[-length(my_y)], 
                                         y1 = my_y[-1],
                                         arrow = arrow(ends = c(rep('first', start_rule - 1),
                                                                rep('last', length(my_z) - start_rule))), 
                                         gp=gpar(lty=2, col = 2)), 
                        t=my_t, b=my_b, l=my_l, r = my_r,
                        z = 1)
  
  
  pg$layout$clip <- "off"
  grid.newpage()
  grid.draw(pg)
  
  pg
}
