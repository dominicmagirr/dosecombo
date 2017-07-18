#' Plot contours of p(overdose) and p(target toxicity).
#'
#'
#' @param doses A list containing doses on the original and log scale.
#' @param s MCMC output...from 'get_samples'
#' @param n_points The number of points (on each axis) to use in the contour plot.
#' then this dose will be declared inadmissible.
#' @return A list of three contour plots: for Pr(overdosing), Pr(target toxicity) and 'best guess' at p(DLT).
#' @export

plot_contour = function(doses, s, n_points = 50){
  
  d_1_star = doses$orig[1,1] / exp(doses$log_scale[1,1])
  d_2_star = doses$orig[1,2] / exp(doses$log_scale[1,2])
  
  dose_1_max = max(doses$orig[,1])
  dose_2_max = max(doses$orig[,2])
  
  doses_dense = DoseCombo::doses(dose_grid(list(seq(0.001, dose_1_max, length.out = n_points),
                                                seq(0.001, dose_2_max, length.out = n_points))),
                                 d_star = c(d_1_star, d_2_star))
  
  dense_info = apply(doses_dense$log_scale, 1, get_info, s = s)
  dose_info = apply(doses$log_scale, 1, get_info, s = s)
  
  dose_density = as.data.frame(cbind(doses_dense$orig, dense_info[5,], dense_info[6,], dense_info[2,]))
  names(dose_density) = c("dose_1", "dose_2", "target", "over", "median")
  
  over_doses = as.data.frame(doses$orig[dose_info[6,] > doses$over_limit, , drop = FALSE])
  under_doses = as.data.frame(doses$orig[dose_info[6,] <= doses$over_limit, , drop = FALSE])
  
  
  if (dim(over_doses)[1]>0) names(over_doses) = c("dose_1", "dose_2")
  if (dim(under_doses)[1]>0) names(under_doses) = c("dose_1", "dose_2")
  
  v <- ggplot(dose_density)
  
  ########################
  v_med = v +
    geom_contour(mapping = aes(dose_1,dose_2, z = median, colour = ..level..),
                 breaks = c(0.2, 0.3, 0.4, 0.5),
                 size = 2) +
    scale_x_continuous(breaks = c(0,25,50,75,100), minor_breaks = c(0,25,50,75,100)) +
    scale_y_continuous(breaks = c(0,1,2,4,8), minor_breaks = c(0,1,2,4,8))
  
  med_contour = direct.label(v_med, method = "bottom.pieces") 
  
  if (dim(over_doses)[1]>0) {
    med_contour = med_contour + geom_point(data = over_doses,
                                           mapping = aes(x=dose_1,y=dose_2),
                                           colour = "red",
                                           size = 3)
  }  
  if (dim(under_doses)[1]>0){
    med_contour = med_contour + geom_point(data = under_doses,
                                           mapping = aes(x=dose_1,y=dose_2),
                                           colour = "green",
                                           size = 3)
  }
  ########################
  v2 = v +
    geom_contour(mapping = aes(dose_1,dose_2, z = target, colour = ..level..),
                 breaks = c(0.2, 0.3, 0.4, 0.5),
                 size = 2) +
    scale_x_continuous(breaks = c(0,25,50,75,100), minor_breaks = c(0,25,50,75,100)) +
    scale_y_continuous(breaks = c(0,1,2,4,8), minor_breaks = c(0,1,2,4,8))
  
  
  target_plot = direct.label(v2, method = "bottom.pieces") 
  
  if (dim(over_doses)[1]>0) {
    target_plot = target_plot + geom_point(data = over_doses,
                                           mapping = aes(x=dose_1,y=dose_2),
                                           colour = "red",
                                           size = 3)
  }  
  if (dim(under_doses)[1]>0){
    target_plot = target_plot + geom_point(data = under_doses,
                                           mapping = aes(x=dose_1,y=dose_2),
                                           colour = "green",
                                           size = 3)
  }
  
  
  v3 = v +
    geom_contour(mapping = aes(dose_1,dose_2, z = over, colour = ..level..),
                 breaks = c(0.2, 0.3, 0.4, 0.5),
                 size = 2) +
    scale_x_continuous(breaks = c(0,25,50,75,100), minor_breaks = c(0,25,50,75,100)) 
  scale_y_continuous(breaks = c(0,1,2,4,8), minor_breaks = c(0,1,2,4,8))
  
  over_plot = direct.label(v3, method = "bottom.pieces") 
  if (dim(over_doses)[1]>0) {
    over_plot = over_plot +  geom_point(data = over_doses,
                                        mapping = aes(x=dose_1,y=dose_2),
                                        colour = "red",
                                        size = 3) 
  }  
  if (dim(under_doses)[1]>0){
    over_plot = over_plot +  geom_point(data = under_doses,
                                        mapping = aes(x=dose_1,y=dose_2),
                                        colour = "forestgreen",
                                        size = 3)
    
    
  }
  
  list(over_plot = over_plot,
       target_plot = target_plot,
       med_contour = med_contour)
  
  
}
