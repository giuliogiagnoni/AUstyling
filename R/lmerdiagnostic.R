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

    b <- as.data.frame(t(do.call(rbind, bartlett.test(as.formula(paste(i, iv, sep = "~")), data = d))))
    b$p.value <- formatC(as.numeric(b$p.value), digits = 4)
    b$statistic <- formatC(as.numeric(b$statistic), digits = 4)
    bartletttab <- tableGrob(format(b))

    plotnorm <- ggplot(mapping = aes(sample = residuals(lmer_results))) +
      labs(x = "Theoretical quantiles", y = "Sample quantiles") +
      stat_qq_point(size = 2,color = "black") +
      stat_qq_line(color = au_pal_red[1]) +
      theme_au_bw_col()

    plotfitted <- ggplot(mapping = aes(x = fitted(lmer_results), y = residuals(lmer_results))) +
      geom_point() +
      geom_hline(yintercept = 0, color = au_pal_red[1]) +
      labs(x = "Fitted", y = "Residuals") +
      theme_au_bw_col()


    print(grid.arrange(arrangeGrob(arrangeGrob(pval, randeff, heights = c(1,1), widths = c(1,2), ncol = 2,
                                               top = textGrob(i, gp=grid::gpar(fontsize=20))),
                                   shapirotab, bartletttab,
                                   plotnorm, plotfitted, heights = c(1,1,1,4,4), ncol = 1)))
  }
}
