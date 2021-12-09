#' @export
lmertableloop <- function(d, m, iv, dv, p, s){

  DATA <- NULL

  for (i in dv) {
    lmer_results <- lmer(as.formula(paste(i, m, sep = "~")), d)
    pval <- as.data.frame(anova(lmer_results, type = 2))
    pval <- pval %>% dplyr::filter(row.names(pval) %in% iv) %>%
      dplyr::select("Pr(>F)") %>%
      dplyr::rename("P" = "Pr(>F)")
    pval <- t(pval)

    emm <- t(as.data.frame(emmeans(lmer_results, iv)))
    colnames(emm) <- emm[1,]
    emm <- as.data.frame(emm[-1,])
    emm <- as.data.frame(sapply(emm, as.numeric))
    se <- signif(max(emm[2,]), 2)

    emmc <- emm[1,]

    line <- cbind(Response = i ,cbind(emmc, se, pval))
    DATA <- rbind(line, DATA)
  }

  # significant digits p values

  DATA[, iv] <-  if(missing(p)){
    round(DATA[, iv], 3)
  }
  else if (p == 'full'){
    DATA[, iv]
  }
  else if (p == 'std1'){
    sapply(DATA[, iv], function(x) ifelse(x >= 0.01, round(x, digits = 2), ifelse(x < 0.001, "<0.001",
                                                   ifelse(x < 0.01 & x >= 0.001, "<0.01", as.character(x) ))))
  } else {
    stop(sQuote(p), " not implemented")
  }

  # significant digits means

  DATA[, colnames(emmc)] <-  if(missing(s)){
    round(DATA[, colnames(emmc)], 3)
  }
  else if (s == 'full'){
    DATA[, colnames(emmc)]
  }
  else if (s >= 1 & s <= 6){
    emmsignif <- lapply(DATA[, colnames(emmc)], function(x) formatC(x, digits = s, format = "fg", flag = "#"))
    lapply(emmsignif, function(x) sub("\\.$", "", x))
  }
  else {
    stop(sQuote(s), " not implemented")
  }

  # significant digits se

  DATA[, "se"] <-  if(missing(s)){
    round(DATA[, "se"], 2)
  }
  else if (s == 'full'){
    DATA[, "se"]
  }
  else if (s >= 1 & s <= 6){
    sesignif <-  formatC(DATA[, "se"], digits=s-1, format = "fg", flag="#")
    sub("\\.$", "", sesignif)
  } else {
    stop(sQuote(s), " not implemented")
  }



  return(DATA)
}
