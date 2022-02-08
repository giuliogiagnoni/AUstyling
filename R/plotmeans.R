#' plotmeans1
#'
#' @param d a data frame.
#' @param m a lmer model i.e "tr + p + (1|a)".
#' @param iv indepent variable of the model to compute the emmeans and standard error of the mean i.e. "tr".
#' @param ivp in case there are multiple iv for the test, you can select a single one of interest to show significant difference of letters.
#'            If not selected it will use the first one provided.
#' @param dv 1 dependent variable of interest i.e "var1"

#' @return A column plot with least square means (column) and the standard error (error bars) and the statistical difference with a Tukey test (letters). Letters will be black if the anova's p-value for the model is significant, if the anova return p-value between 0.05 and 0.10 letters are orange, and if the p-value is above 0.10 the letters are red.
#'
#' @export
plotmeans <- function(d, m, iv, ivp, dv, plt =TRUE){

  lmer_results <- lmer(as.formula(paste(dv, m, sep = "~")), d)
  emmbase <- eval(parse(text = paste("emmeans(lmer_results, ~", paste(iv, collapse = "*"), ")")))
  dat1 <- as.data.frame(multcomp::cld(emmbase, Letters = letters))

  emmplt <- eval(parse(text = paste("emmeans(lmer_results, ~", iv[1], ")")))
  dat2 <- as.data.frame(multcomp::cld(emmplt, Letters = letters))

  pval <- as.data.frame(anova(lmer_results, type = 2))

  pval1 <- if(missing(ivp)){
    pval %>% dplyr::filter(row.names(pval) %in% iv[1]) %>%
      dplyr::select("Pr(>F)") %>%
      dplyr::pull("Pr(>F)")
  }
  else {
    pval %>% dplyr::filter(row.names(pval) %in% ivp) %>%
      dplyr::select("Pr(>F)") %>%
      dplyr::pull("Pr(>F)")
  }

  dat1$pvalue <- pval1
  dat2$pvalue <- pval1


  dat1 <- dat1 %>% dplyr::arrange(!!! rlang::syms(iv))

  dat2 <- dat2 %>% dplyr::arrange(!!! rlang::syms(iv[1]))

  dat3 <- if(length(iv) > 1){
  dat3 <- dat1
  dat3$var <- apply(dat3[,iv], 1, paste, collapse = "-")
  dat3 %>% dplyr::arrange(!!! rlang::syms(iv))
  } else {NULL}

  if(missing(plt)){ return(dat1) }
  else if(plt == TRUE){
    return(
    ggplot(dat2, aes_string(iv[1], "emmean")) +
      geom_col(fill = au_pal_full[2]) +
      geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE)) +
      geom_text(aes(label = .group), vjust = -3,
                color = ifelse(dat2$pvalue > 0.1, "red",
                               ifelse(dat2$pvalue <= 0.1 & dat2$pvalue > 0.05, "orange", "black"))) +
      labs(x = iv, y = as.character(formula(lmer_results))[2]) +
      coord_cartesian(ylim = c(0,max(dat2$emmean)*1.2)) +
      theme_au_bw()
    )
  }
  else if(plt == "FULL"){
    return(
      ggplot(dat3, aes_string("var", "emmean")) +
        geom_col(fill = au_pal_full[2]) +
        geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE)) +
        geom_text(aes(label = .group), vjust = -3,
                  color = ifelse(dat3$pvalue > 0.1, "red",
                                 ifelse(dat3$pvalue <= 0.1 & dat3$pvalue > 0.05, "orange", "black"))) +
        labs(x = paste(iv, collapse = "-"), y = as.character(formula(lmer_results))[2]) +
        coord_cartesian(ylim = c(0,max(dat3$emmean)*1.2)) +
        theme_au_bw()
    )
  }
  else{ stop(sQuote(s), " not implemented") }


  }
