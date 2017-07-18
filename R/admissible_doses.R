#' Find doses which would not be 'too big' an increase
#' 
#' @param current_dose Row number of 'doses' corresponding ot current dose.
#' @param doses A matrix with 2 columns, where each row is a unique dose combination (on original scale)

admissible_doses = function(current_dose, doses){
  
  dose_1 = doses[current_dose,1]
  dose_2 = doses[current_dose,2]
  
  both_over = (doses[,1] > dose_1) & (doses[,2] > dose_2)
  too_much_1 = doses[,1] > 2 * dose_1
  too_much_2 = doses[,2] > 2 * dose_2
  
  !(both_over | too_much_1 | too_much_2)
}


