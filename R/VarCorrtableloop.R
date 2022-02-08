#' VarCorr table loop
#'
#' @param d a data frame.
#' @param m a lmer model i.e "tr + p + (1|a)".
#' @param dv a vector with the dependent variable of interest i.e c("var1","var2","var3")
#' @param r type of residual to display.
#'  empty:  will return both random effect and error residuals
#'  "residuals" or "residual" or "res": will return only error residuals.
#'  "effect" or "eff": will return random error residuals.
#' @param s significant digits of least square means.
#'  empty:  will round the number to 3 decimals.
#'  number 1 to 6: will return the desired significant digits.
#' @return A data frame with the standard error of the random effect and error residuals. Use the VarCorr function to return the output for a serie of variable in one linear mixed model.
#'
#' @export
VarCorrtableloop <- function(d, m, dv, s, r){

  DATA <- NULL

  for (i in dv) {
    lmer_results <- lmer(as.formula(paste(i, m, sep = "~")), d)
    a <- as.data.frame(VarCorr(lmer_results))
    a <- a[,c(1,5)]
    a$dv <- i

    DATA <- rbind(a, DATA)
  }

  colnames(DATA) <- c("raneff", "sd", "dep.var")


  # significant digits se

  DATA[, "sd"] <-  if(missing(s)){
    round(DATA[, "sd"], 2)
  }
  else if (s == 'full'){
    DATA[, "sd"]
  }
  else if (s >= 1 & s <= 6){
    sesignif <-  formatC(DATA[, "sd"], digits=s-1, format = "fg", flag="#")
    sub("\\.$", "", sesignif)
  } else {
    stop(sQuote(s), " not implemented")
  }


  if(missing(r)){
    return(DATA)
  }
  else if (r == 'residuals' | r == 'residual' | r == 'res'){
    return(subset(DATA, raneff == "Residual"))
  }
  else if (r  == "effect" | r == "eff"){
    return(subset(DATA, raneff != "Residual"))
  } else {
    stop(sQuote(r), " not implemented")
  }

}
