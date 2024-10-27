#' Title ORs & 95% CIs
#'
#' @param coef beta coefficient
#' @param se standard error
#' @param siglevel significance level
#' @param roundto round to decimal places
#'
#' @return ORresult
#' @author Syeda Aiman Fatima
#' @export
#'
#' @examples
#' OR_95CI(m1coefs, m1ses, siglevel = 0.05, roundto = 2)
#'

OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}

