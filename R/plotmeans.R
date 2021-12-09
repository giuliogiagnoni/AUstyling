#' @export
plotmeans1 <- function(d, m, iv){

  dat <- emmeans::emmeans(lmer(m, d), as.formula(paste("~", iv)))
  dat1 <- as.data.frame(multcomp::cld(dat, Letters = letters))

  pval <- as.data.frame(anova(lmer(m, d), type = 2))
  pval <- pval %>% dplyr::filter(row.names(pval) %in% iv) %>%
    dplyr::select("Pr(>F)") %>%
    dplyr::pull("Pr(>F)")

  dat1$pvalue <- pval

  ggplot(dat1, aes(dat1[,1], dat1[,2])) +
    geom_col(fill = au_pal$"full"[2]) +
    geom_errorbar(aes(ymin = dat1[,2] - dat1[,3], ymax = dat1[,2] + dat1[,3])) +
    geom_text(aes(label = dat1[,7]), vjust = -3,
              color = ifelse(dat1$pvalue > 0.1, "red",
                             ifelse(dat1$pvalue <= 0.1 & dat1$pvalue > 0.05, "orange", "black"))) +
    labs(x = colnames(dat1)[1], y = as.character(formula(m))[2]) +
    theme_au_bw()}
