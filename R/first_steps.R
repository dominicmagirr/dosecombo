#' Plot what posterior would look like after first cohort.
#'
#' @param doses List of doses on original and log scales.
#' @param cohort_size The size of the first cohort.
#' @param starting_dose The starting dose on the original scale.
#' @return A list of ggplot objects.
#' @export
#'


first_steps = function(doses, cohort_size, starting_dose){
  
  # empty data frame
  dat = data.frame(dose_1 = doses$log_scale[,1],
                   dose_2 = doses$log_scale[,2],
                   n = 0,
                   n_dlt = 0)
  
  dose_id = dose_row(doses, starting_dose, orig_scale = TRUE)
  # treat 'cohort_size' at starting dose:
  dat[dose_id, "n"] = cohort_size
  
  results = as.list(0:cohort_size)
  
  for (i in 0:cohort_size){
    
    results[[i + 1]] = as.list(1:2)
    
    dat[dose_id, "n_dlt"] = i
    
    s = get_samples(dat, prior = doses$prior, n_iter = 50000)
    
    new_info = apply(doses$log_scale, 1, get_info, s = s)
    
    next_dose = get_next_dose(new_info, doses = doses, current_dose = dose_id)
    results[[i + 1]][[1]] = next_dose
    p = plot_info(new_info,doses,df = dat)
    results[[i + 1]][[2]] = p
  }
  
  results
  
}