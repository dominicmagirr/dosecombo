#' Plot the prior/posterior distribution for p(DLT) at all dose combinations
#'
#' @param info A matrix with a row for each dose combination, where each row is a vector of length 8.  The 1st -- 3rd elements
#' are the 0.1,0.5 and 0.9 quantiles of the distribution of p(DLT). The 4th -- 6th elements are the probabilities
#' of underdosing, target-dosing and overdosing, respectively.
#' @param doses A list containing doses on the original and log scale.
#' @param df The data.
#' @return A ggplot object.
#' @export

plot_info = function(info, doses, df = NULL){


  data_vis = info %>%
    t() %>%
    as.data.frame()

  names(data_vis) = c("lower",
                      "median",
                      "upper",
                      "under",
                      "target",
                      "over")

  data_vis = cbind(data_vis,
                   dose_1 = doses$orig[,1],
                   dose_2 = doses$orig[,2])


  data_vis = data_vis %>%
    mutate(dose_1_orig = paste("Dose 1 =", dose_1),
           dose_2_orig = paste("Dose 2 =", dose_2))

  ###


  levels_1 = paste("Dose 1 =", sort(unique(doses$orig[,1])))
  levels_2 = paste("Dose 2 =", sort(unique(doses$orig[,2])))


  ##########################
  ## wide to long
  data_vis$interval = 0
  data_bar = data_vis %>%
    select(dose_1_orig,dose_2_orig,interval,under,target,over) %>%
    melt(id = c("dose_1_orig", "dose_2_orig")) %>%
    mutate(admissible = ifelse((variable == "over") & (value > doses$over_limit), FALSE, TRUE))




  ##########################
  ## order on plot
  data_bar$dose_1_orig = factor(data_bar$dose_1_orig, levels = rev(levels_1))
  data_bar$dose_2_orig = factor(data_bar$dose_2_orig, levels = levels_2)

  data_vis$dose_1_orig = factor(data_vis$dose_1_orig, levels = rev(levels_1))
  data_vis$dose_2_orig = factor(data_vis$dose_2_orig, levels = levels_2)




  p = ggplot() +
    geom_bar(data = data_bar,
             aes(x = variable, y = value,
                 fill = admissible),
             stat = "identity") +
    facet_grid(dose_1_orig ~ dose_2_orig) +
    geom_errorbar(data = data_vis,
                  aes(x = 1,
                      ymin = lower,
                      ymax = upper),
                  width = 0.25) +
    geom_point(data = data_vis,
               aes(x = 1,
                   y = median))  +
    theme(legend.position = "none") +
    ylab(" ") +
    xlab(" ")

  if (!is.null(df)){

    data_text = data_vis %>% cbind(df$n) %>% cbind(df$n_dlt)
    data_text$text = paste(df$n_dlt,"/",df$n)
    data_text$text_x = 2.5
    data_text$text_y = 0.875

    data_text$dose_1_orig = factor(data_text$dose_1_orig, levels = rev(levels_1))
    data_text$dose_2_orig = factor(data_text$dose_2_orig, levels = levels_2)

    p = p + geom_text(data = data_text,
                      aes(x = text_x,
                          y = text_y,
                          label = text),
                      colour = "blue")
  }

  p

}


