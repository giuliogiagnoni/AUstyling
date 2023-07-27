#' lmertableloop
#'
#' @param d a data frame.
#' @param m a lmer model i.e "tr + p + (1|a)" or "tr * p + (1|a)".
#' @param ivm indepent variable of the model to compute the emmeans i.e. "tr" or c("tr","p")
#' @param ivm indepent variable of the model to compute the p-values i.e. "tr" or c("tr","p", "tr:p").
#'   Important, when interaction between 2 or more variables, they have to be in the order they are displayed in the anova function,
#' @param dv a vector with the dependent variable of interest i.e c("var1","var2","var3")
#' @param p significant digits of p-values.
#'  empty:  will round the number to 3 decimals.
#'  "full": will leave all the digits.
#'  "std1": will round to 2 digits, if above 0.01, or 3, if above 0.001, otherwise if will print <0.001.
#'  "std2": will round to 2 digits, or print <0.01 or <0.001.
#' @param s significant digits of least square means.
#'  empty:  will round the number to 3 decimals.
#'  number 1 to 6: will return the desired significant digits.
#'  "full": will leave all the digits.
#' @param con the list of contrast... need to specify 1 independent variable.
#' @param ivc independent variable of the model to computer the contrasts
#' @param let TRUE if you want to include the letter of the tuckey's test difference next to the least square means.
#' @param aov select number 1, 2, or 3, for the anova type
#'
#' @return A data frame with variable names, least square means (emmeans), maximim SEM, p-values, and p-values of contrasts.
#'
#' @export
#'
lmertableloop <- function(d, m, ivm, ivp, dv, p, s, con, ivc, let = TRUE, aov){

  DATA <- NULL
  DATAletters <- NULL

  for (i in dv) {
    lmer_results <- eval(parse(text = paste("lmer(", i, "~", m, ",", deparse(substitute(d)), ")")))
    pval <- as.data.frame(anova(lmer_results, type =  if(missing(aov)){2}else{aov}))
    pval <- pval %>% dplyr::filter(row.names(pval) %in% ivp) %>%
      dplyr::select("Pr(>F)") %>%
      dplyr::rename("P" = "Pr(>F)")
    pval <- t(pval)

    # unique(sub("\\*", ":", c(ivp, paste(ivp, collapse = "*"))))

    emmbase <- eval(parse(text = paste("emmeans(lmer_results, ~", paste(ivm, collapse = "*"), ")")))

    line <- if(missing(con) | missing(ivc)){ NULL }
    else{
      emmbasecon <- eval(parse(text = paste("emmeans(lmer_results, ~", ivc, ")")))
    }

    emm <- t(as.data.frame(emmbase))
    colnames(emm) <-  as.data.frame(emm) %>% dplyr::filter(row.names(emm) %in% ivm) %>%
      summarise_all(~ paste(., collapse = "-"))
    emm <-  as.data.frame(emm) %>% dplyr::filter(!row.names(emm) %in% ivm)

    emm <- as.data.frame(sapply(emm, as.numeric))
    SEM <- max(emm[2,])

    emmc <- emm[1,]

    DATAletters <- if(missing(let) | let == FALSE){ NULL }
    else if (let == TRUE & length(ivm) == 1){
      dataletters <- as.data.frame(multcomp::cld(emmbase, Letters = letters))
      dataletters$.group <- as.character(gsub(" ", "", dataletters$.group))
      dataletters <- t(as.data.frame(dataletters))
      colnames(dataletters) <-  as.data.frame(dataletters) %>% dplyr::filter(row.names(dataletters) %in% ivm) %>%
        summarise_all(~ paste(., collapse = "-"))
      dataletters <-  as.data.frame(dataletters) %>% dplyr::filter(row.names(dataletters) %in% ".group")
      dataletters1 <- dataletters

      rbind(dataletters1, DATAletters) }
    else if (let == TRUE & length(ivm) > 1){

      dataletters <- as.data.frame(multcomp::cld(emmbase, Letters = letters))
      dataletters$.group <- as.character(gsub(" ", "", dataletters$.group))
      dataletters <- t(as.data.frame(dataletters))
      colnames(dataletters) <-  as.data.frame(dataletters) %>% dplyr::filter(row.names(dataletters) %in% ivm) %>%
        summarise_all(~ paste(., collapse = "-"))
      dataletters <-  as.data.frame(dataletters) %>% dplyr::filter(row.names(dataletters) %in% ".group")
      dataletters1 <- dataletters

      rbind(dataletters1, DATAletters) }
    else { stop(sQuote(s), " not implemented") }


    line <- if(missing(con)){
      cbind(Response = i ,cbind(emmc, SEM, pval))
    }
    else{
      cont <- as.data.frame(contrast(emmbasecon, con))
      cont$p.value <- format(cont$p.value, scientific = FALSE)
      cont <- t(cont)
      colnames(cont) <- cont[1,]
      cont <- cont["p.value",]
      cont <- sapply(cont, as.numeric)
      cont <- as.data.frame(t(cont))
      cbind(Response = i ,cbind(emmc, SEM, pval, cont))
    }

    DATA <- rbind(line, DATA)

  }

  pvalcol <- if(missing(con) | missing(ivc)){
    ivp
  }
  else{
    c(ivp, colnames(cont))
  }


  # significant digits p values

  DATA[, pvalcol] <-  if(missing(p)){
    round(DATA[, pvalcol], 3)
  }
  else if (p == 'full'){
    DATA[, pvalcol]
  }
  else if (p == 'std1'){
    sapply(DATA[, pvalcol],
           function(x) ifelse(x >= 0.01, round(x, digits = 2), ifelse(x < 0.001, "<0.001",
                                                                      ifelse(x < 0.01 & x >= 0.001, round(x, digits = 3), as.character(x) ))))
  }
  else if (p == 'std2'){
    sapply(DATA[, pvalcol],
           function(x) ifelse(x >= 0.01, round(x, digits = 2), ifelse(x < 0.001, "<0.001",
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

  DATA[, "SEM"] <-  if(missing(s)){
    round(DATA[, "SEM"], 2)
  }
  else if (s == 'full'){
    DATA[, "SEM"]
  }
  else if (s >= 1 & s <= 6){
    sesignif <-  formatC(DATA[, "SEM"], digits=s-1, format = "fg", flag="#")
    sub("\\.$", "", sesignif)
  } else {
    stop(sQuote(s), " not implemented")
  }


  DATAletters <- as.data.frame(DATAletters)

 DATAletters <-  DATAletters %>% dplyr::select(!!!rlang::syms(colnames(emmc)))

  if(missing(let) | let == FALSE){ DATA[1:nrow(DATA),2:(ncol(DATAletters)+1)] <-  DATA[1:nrow(DATA),2:(ncol(DATAletters)+1)] }
  else if (let == TRUE){ DATA[1:nrow(DATA),2:(ncol(DATAletters)+1)] <- paste(as.matrix(DATA[1:nrow(DATA),2:(ncol(DATAletters)+1)]),
                                                                             as.matrix(DATAletters), sep = "") }
  else { stop(sQuote(s), " not implemented") }

  return(DATA)
}
