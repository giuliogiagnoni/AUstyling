#library("devtools")
#library(roxygen2)
#library(flextable)

#############################################################
###### create flextable theme ######
###############################################################

# Flextable guide
# https://ardata-fr.github.io/flextable-book/
# https://davidgohel.github.io/flextable/
# https://mran.revolutionanalytics.com/snapshot/2019-02-07/web/packages/flextable/vignettes/overview.html

#' @export
flextable_au_tidy <- function(x){
  flextable(x)  %>%
    color(color = "black", part = "header") %>%
    color(color = "black") %>%
  bold(part = "header") %>%
    font(fontname = "Georgia", part = "all") %>%
    fontsize(size = 10, part = "header") %>%
    fontsize(size = 10, part = "body") %>%
    fontsize(size = 9, part = "footer") %>%
  bold(part = "header") %>%
#  border_remove() %>%
#  hline_top(border = fp_border(color = "black", style = "solid", width = 1.2)) %>%
#  hline_bottom(border = fp_border(color = "black", style = "solid", width = 1.2))  %>%
  autofit()%>%
    height(height = 0.65, unit = "cm")
}

#' @export
flextable_au_color <- function(x){
  flextable(x)  %>%
  bg(bg =  au_pal$full[2], part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = au_pal$full[2]) %>%
  bold(part = "header") %>%
    font(fontname = "Georgia", part = "all") %>%
    fontsize(size = 10, part = "header") %>%
  fontsize(size = 10, part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_bottom(border = fp_border(color =  au_pal$full[2], style = "solid", width = 1.2))  %>%
  autofit() %>%
    height_all(height = 0.65, unit = "cm")
}


########################################################
################# tableGrob ######################
####################################################################

#' @export
ttheme_au_default <- function(base_size = 11, base_colour = "black", base_family = "AU Passata",
                               parse = FALSE, padding = unit(c(4, 4), "mm"), ...){
  ttheme_default(base_size = 11, base_colour = "black", base_family = "",
                    parse = FALSE, padding = unit(c(4, 4), "mm"), ...)
}

