#' Turn a short DLT data frame with n_dlt/n into a longer version
#'
#' Convert a data frame with binomial data into a longer data frame of binary responses.
#' This is useful to give to JAGS.
#' @param df A data frame with columns 'dose_1', 'dose_2', 'n' and 'n_dlt'

binom_2_bern <- function(df){
  data.frame(dose_1 = rep(df$dose_1, df$n),
             dose_2 = rep(df$dose_2, df$n),
             dlt = rep(rep(1:0, length(df$dose_1)),
                       c(t(cbind(df$n_dlt, df$n - df$n_dlt)))))
}
