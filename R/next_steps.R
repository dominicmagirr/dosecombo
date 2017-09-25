#' Plot what posterior would look like after first cohort.
#'
#' @param doses List of doses on original and log scales.
#' @param doses_1_so_far The doses of drug 1 tried so far. Vector, on original scale.
#' @param doses_2_so_far The doses of drug 2 tried so far (matching positions with doses_1_so_far). Vector, on original scale.
#' @param n_so_far The number of patients on each dose combination so far (matching positions with doses_1_so_far). Vector.
#' @param n_dlt_so_far The number of dlts on each dose combination so far (matching positions with doses_1_so_far). Vector.
#' @param last_dose The most recent dose. Vector of length 2 on the original scale.
#' @param sim_result Option to pass the result of a simulated trial, instead of other arguments.
#' @return A list of length 2. The first element is the recommended next dose. The second element is a plot of posterior.
#' @export
#'


next_steps = function(doses,
                      doses_1_so_far = 1,
                      doses_2_so_far = 1,
                      n_so_far = 3,
                      n_dlt_so_far = 0,
                      last_dose = c(1,1),
                      sim_result = NULL){

  if (!is.null(sim_result)){
    sim_tab = table(sim_result$doses_tried, sim_result$observed_dlts)
    doses_1_so_far = doses$orig[as.numeric(rownames(sim_tab)),1]
    doses_2_so_far = doses$orig[as.numeric(rownames(sim_tab)),2]
    n_so_far = unname(sim_tab[,"0"]) + unname(sim_tab[,"1"])
    n_dlt_so_far = unname(sim_tab[,"1"])
    last_dose = doses$orig[rev(sim_result$doses_tried)[1],]

  }

  dose_rows = numeric(length(doses_1_so_far))

  for (i in seq_along(doses_1_so_far)){
    dose_rows[i] = dose_row(doses, c(doses_1_so_far[i], doses_2_so_far[i]), orig_scale = TRUE)
  }


  dat = data.frame(dose_1 = doses$log_scale[,1],
                   dose_2 = doses$log_scale[,2],
                   n = 0,
                   n_dlt = 0)

  dat[dose_rows,"n"] = n_so_far
  dat[dose_rows,"n_dlt"] = n_dlt_so_far

  dose_id = dose_row(doses, last_dose, orig_scale = TRUE)

  s = get_samples(dat, prior = doses$prior, n_iter = 50000)

  new_info = apply(doses$log_scale, 1, get_info, s = s)

  next_dose = get_next_dose(new_info, doses = doses, current_dose = dose_id)

  p = plot_info(new_info,doses,df = dat)

  list(next_dose = doses$orig[next_dose,], p = p)

}
