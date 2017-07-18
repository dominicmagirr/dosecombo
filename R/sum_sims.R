#' Summarize results from simulation
#'
#' @param sims list coming from 'sim_study'.
#' @param truth a vector of the true p(DLT) at each dose combination.
#' @return A list of information.
#' @export
#'

sum_sims <- function(sims, truth){

  n_sims = length(sims)

  mtds = numeric(n_sims)
  over_target = numeric(n_sims)
  overall_n = numeric(n_sims)
  prop_dlts = numeric(n_sims)


  for (i in seq_along(mtds)){

    res = sims[[i]][[1]]

    mtds[i] = sims[[i]][[2]]

    over_target[i] = sum(res$n[truth > 0.35])
    overall_n[i] = sum(res$n)
    prop_dlts[i] = sum(res$n_dlt) / sum(res$n)
  }

  tox_at_selected = truth[mtds]

  return(list(mtds = mtds,
              over_target = over_target,
              overall_n = overall_n,
              prop_dlts = prop_dlts,
              tox_at_selected = tox_at_selected))

}
