#' Create a prior for monotherapy dose response, similar to crmPack approach
#'
#' @param doses set of doses considered
#' @param ref_dose reference dose for standardizing 'doses'.
#' @param prior_medians Guesstimates for p(DLT) at 'doses'.
#' @param psuedo_n the equivalent number of observations at each dose. The larger this is, the more certainty in prior.
#' @return A crmPack model object.
#' @export


create_mono_prior = function(doses,
                        ref_dose,
                        prior_medians,
                        psuedo_n){



  a = psuedo_n * prior_medians + 1/3
  b = psuedo_n * (1 - prior_medians) + 1/3


  model_1 = crmPack::Quantiles2LogisticNormal(dosegrid = doses,
                                               refDose = ref_dose,
                                               lower = qbeta(0.025, a, b),
                                               median = qbeta(0.5, a, b),
                                               upper = qbeta(0.975, a, b),
                                               level = 0.95, # don't think it's possible to change this
                                               logNormal = TRUE,
                                               control=list(threshold.stop=0.03,maxit=100))



  matplot(x=doses,
          y=model_1$required,
          type="b", pch=19, col="blue", lty=1,
          xlab="dose",
          ylab="prior probability of DLT",
          ylim = c(0,1))

  matlines(x=doses,
           y=model_1$quantiles,
           type="b", pch=19, col="red", lty=1)

  legend("topright",legend=c("quantiles", "approximation"),col=c("blue", "red"),lty=1,bty="n")

  return(model_1$model)

}
