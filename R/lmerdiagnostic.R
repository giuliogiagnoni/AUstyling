#' lmerdiagnostic
#'
#' @param d a data frame.
#' @param m a lmer model i.e "tr + p + (1|a)".
#' @param iv indepent variable which anova p-value will be displayed i.e. "tr" or c("tr", "p"), the first one will be used to test homogeinity
#' @param dv a vector with the dependent variable of interest i.e c("var1","var2","var3").
#' @return A serie of tables and plots useful for model diagnostic of linear and linear mixed models.
#'
#' @export
lmerdiagnostic <- function(d, m, iv, dv){

  for (i in dv) {
    lmer_results <- lmer(as.formula(paste(i, m, sep = "~")), d)

    pval <- as.data.frame(anova(lmer_results, type = 2))
    pval <- pval %>% dplyr::filter(row.names(pval) %in% iv) %>%
      dplyr::select("Pr(>F)") %>%
      dplyr::rename("p.value" = "Pr(>F)")
    pval$p.value <- formatC(pval$p.value, digits = 4)
    pval <- tableGrob(pval)

    randeff <- as.data.frame(rand(lmer_results))
    randeff <- randeff %>% dplyr::rename("p.value" = "Pr(>Chisq)")
    randeff$p.value <- formatC(randeff$p.value, digits = 4)
    randeff <- tableGrob(randeff)

    a <- as.data.frame(t(do.call(rbind, shapiro.test(residuals(lmer_results)))))
    a$p.value <- formatC(as.numeric(a$p.value), digits = 4)
    a$statistic <- formatC(as.numeric(a$statistic), digits = 4)
    shapirotab <- tableGrob(format(a))

    b <- as.data.frame(t(do.call(rbind, bartlett.test(as.formula(paste(i, iv[1], sep = "~")), data = d))))
    b$p.value <- formatC(as.numeric(b$p.value), digits = 4)
    b$statistic <- formatC(as.numeric(b$statistic), digits = 4)
    bartletttab <- tableGrob(format(b))

    is_outlier <- function(x) {
      return(x < quantile(x, 0.33) - 1.5 * IQR(x) | x > quantile(x, 0.66) + 1.5 * IQR(x))
    }

    outlierqqplot <- data.frame(sample = qqnorm(residuals(lmer_results), plot.it = FALSE)[1],
                                theory = qqnorm(residuals(lmer_results), plot.it = FALSE)[2])

    outlierqqplot$nrow <- row.names(outlierqqplot)
    outlierqqplot$out <- ifelse(is_outlier(outlierqqplot$y), outlierqqplot$nrow, NA)
    outlierqqplot$fitted <- fitted(lmer_results)

    plotnorm <- ggplot() +
      geom_point(outlierqqplot, mapping = aes(x = x, y = y), size = 2,color = "black") +
      geom_text(outlierqqplot, mapping = aes(x = x, y = y, label = out), na.rm = TRUE, hjust = -0.3) +
      labs(x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_au_bw_col()

    plotfitted <- ggplot(outlierqqplot, aes(x = fitted, y = y)) +
      geom_point() +
      geom_hline(yintercept = 0, color = au_pal_red[1]) +
      geom_text(aes(label = out), na.rm = TRUE, hjust = -0.3) +
      labs(x = "Fitted", y = "Residuals") +
      theme_au_bw_col()

    titlegrob <- grid::textGrob(i, gp=grid::gpar(fontsize=20))

    grid.arrange(arrangeGrob(titlegrob,
                                   arrangeGrob(pval, randeff, heights = c(1,1), widths = c(1,2), ncol = 2),
                                   shapirotab, bartletttab,
                                   plotnorm, plotfitted, heights = c(2,3,3,3,5,5), ncol = 1))
  }
}
