#' Plot the results of a simulated trial
#'
#'
#' @param sim The results of a simulated trial. A list. Output of "sim_trial"
#' @param doses A list containing doses on the original and log scale.
#' @return A ggplot of the trial.
#' @export

plot_trial <- function(sim, doses){
  
  df1 = data.frame(dose_id  = sim[[3]][!is.na(sim[[3]])],
                   toxicity = sim[[4]][!is.na(sim[[4]])])
  
  df1$patient_id = 1:length(df1$dose_id)
  
  df2 = data.frame(dose_1 = doses$orig[,1],
                   dose_2 = doses$orig[,2],
                   dose_id = 1:length(doses$orig[,1]))
  
  
  
  df3 = data.frame(dose_1 = doses$orig[,1],
                   dose_2 = doses$orig[,2],
                   patient_id = NA,
                   toxicity = NA,
                   dose_id = NA)
  
  
  df = rbind(merge(df1, df2, by = "dose_id"), df3)
  
  df$dose_1 = paste("Dose 1 =", df$dose_1)
  df$dose_1 = factor(df$dose_1, 
                     levels = rev(paste("Dose 1 =", unique(doses$orig[,1]))))
  
  #df$dose_1 = factor(df$dose_1, levels = rev(unique(doses$orig[,1])))
  df
  
  df$dose_2 = factor(df$dose_2, levels = unique(doses$orig[,2]))
  
  df$toxicity[df$toxicity == 0] = "No"
  df$toxicity[df$toxicity == 1] = "Yes"
  df$toxicity = factor(df$toxicity)
  
  cols <- c("No" = "black", "Yes" = "red")
  
  p = ggplot(data = df,
             mapping = aes(x = patient_id,
                           y = dose_2,
                           colour = toxicity)) +
    geom_point() +
    facet_grid(dose_1 ~ .) +
    scale_x_continuous(breaks = 1:length(df1$dose_id)) +
    theme(panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          strip.text.y = element_text(angle = 0)) +
    ylab("Dose 2") +
    scale_color_manual(values = cols)
  
  p
  
}