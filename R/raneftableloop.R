#' raneftableloop
#'
#' @param d a data frame.
#' @param m a lmer model i.e "tr + p + (1|a)".
#' @param dv a vector with the dependent variable of interest i.e c("var1","var2","var3").
#' @param s significant digits of least square means.
#'  empty:  will round the number to 3 decimals.
#'  number 1 to 6: will return the desired significant digits.
#' @return A data frame with the individual random effects from the ranef function for a serie of variables.
#'
#' @export
raneftableloop <- function(d, m, dv, s){

  DATA <- NULL

  for (i in dv) {
    lmer_results <- lmer(as.formula(paste(i, m, sep = "~")), d)
    a <-as.data.frame(ranef(lmer_results))
    a <- a[,c(1,3,4)]
    a$dv <- i

    DATA <- rbind(a, DATA)
  }

  colnames(DATA) <- c("raneff", "ID", "value", "var")


  # significant digits se

  DATA[, "value"] <-  if(missing(s)){
    round(DATA[, "value"], 2)
  }
  else if (s == 'full'){
    DATA[, "value"]
  }
  else if (s >= 1 & s <= 6){
    sesignif <-  formatC(DATA[, "value"], digits=s-1, format = "fg", flag="#")
    sub("\\.$", "", sesignif)
  } else {
    stop(sQuote(s), " not implemented")
  }


    return(DATA)
}
