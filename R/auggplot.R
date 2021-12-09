#library("devtools")
#library(roxygen2)
#library(ggplot2)
#library(ggpubr)
#library(scales)

### UPDATE AND INSTALL
# devtools::document("C:/Users/AU589897/Documents/Rpackages/autheme")
# devtools::install("C:/Users/AU589897/Documents/Rpackages/autheme")

#############################################################
###### Setting colors ######
###############################################################

# Create vector with all AU colors according to
# https://medarbejdere.au.dk/administration/kommunikation/omdesignet/farver/browse

#' @export
au_pal_full <- c("#003d73", "#002546", "#655a9f", "#281c41",
            "#37a0cb", "#003e5c", "#00aba4", "#004543",
            "#8bad3f", "#425821", "#fabb00", "#634b03",
            "#ee7f00", "#5f3408", "#e2001a", "#5b0c0c",
            "#e2007a", "#5f0030", "#878787", "#4b4b4a")

# Vector with only light colors
#' @export
au_pal_light <- c("#003d73", "#655a9f", "#37a0cb", "#00aba4", "#8bad3f",
              "#fabb00", "#ee7f00", "#e2001a", "#e2007a", "#878787")
# Vector with only dark colors
#' @export
au_pal_dark <- c("#002546", "#281c41", "#003e5c", "#004543", "#425821",
             "#634b03", "#5f3408", "#5b0c0c", "#5f0030", "#4b4b4a")

#' @export
au_pal_blue <- c("#002546", "#0A1449", "#183D83",
                    "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_purple <- c("#281C41", "#0A1449", "#183D83",
                    "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_lightblue <- c("#003E5C", "#0A1449", "#183D83",
                         "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_turquoise <- c("#004543", "#0A1449", "#183D83",
                      "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_green <- c("#425821", "#0A1449", "#183D83",
                    "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_yellow <- c("#634B03", "#0A1449", "#183D83",
                      "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_orange <- c("#5F3408", "#0A1449", "#183D83",
                         "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_red <- c("#5B0C0C", "#0A1449", "#183D83",
                         "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_pink <- c("#5F0030", "#0A1449", "#183D83",
                      "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_black <- c("#000000", "#4B4B4A", "#0A1449", "#183D83",
                   "#87D1F4", "#33525F", "#548195", "#C6C6C6")

#' @export
au_pal_rainbow <- c("#002546", "#281C41", "#003E5C", "#004543", "#425821",
                    "#634B03", "#5F3408", "#5B0C0C", "#5F0030", "#000000")



#' @export
au_pal_customset1.4 <- au_pal_full[seq(1, length(au_pal_full), 6)]

#' @export
au_pal_customset2.5 <- au_pal_full[seq(1, length(au_pal_full), 4)]

#' @export
au_pal_customset3.6 <- au_pal_full[seq(1, length(au_pal_full), 3.5)]

#' @export
au_pal_customset4.6 <- au_pal_full[seq(2, length(au_pal_full), 3.5)]


#show_col(scale_color_AU_set2.5)

# The function AU_color range create a continuos color scale (blue)

fun_color_range <- colorRampPalette(c("#002546", "#37a0cb"))
au_range <- fun_color_range(256)

# show_col(AUcolor_all[seq(1, length(AUcolor_all), 4)])

#############################################################
###### Setting ggplot theme ######
###############################################################

#library(extrafont)
#font_import(pattern = c("AU Passata", "AU Passata Light", "AU Peto")  )

#windowsFonts("AU Passata" = windowsFont("AU Passata"))
#windowsFonts("AU Passata Light" = windowsFont("AU Passata Light"))
#windowsFonts("AU Peto" = windowsFont("AU Peto"))

###### Standard ggplot theme with au style ######

#' @export
theme_au_grey <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
                 theme_grey(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
                 theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_gray <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_gray(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_bw <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_bw(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_linedraw <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_linedraw(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_light <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_light(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_dark <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_dark(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(fill = "gray50"),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_minimal <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_minimal(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_classic <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_classic(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_void <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_void(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_test <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_test(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}

#' @export
theme_au_transparent <- function (base_size = 11, base_family = "AU Passata"){
  theme_transparent(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      strip.background =element_rect(fill="black"),
      strip.text = element_text(size = base_size+1, colour = "white", face = "bold"))
}




##### Standard ggplot theme with color au style ######

#' @export
theme_au_grey_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_grey(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      axis.ticks = element_line(color = au_pal_full[2]),
      panel.background = element_rect(fill = alpha(au_pal_full[2], alpha = 0.1)),
      panel.border = element_rect(colour = "white", fill=NA),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_gray_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_gray(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      axis.ticks = element_line(color = au_pal_full[2]),
      panel.background = element_rect(fill = alpha(au_pal_full[2], alpha = 0.1)),
      panel.border = element_rect(colour = "white", fill=NA),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_bw_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_bw(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      axis.ticks = element_line(color = au_pal_full[2]),
      panel.grid = element_line(colour = alpha(au_pal_full[2], alpha = 0.1)),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_linedraw_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_linedraw(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      panel.grid = element_line(colour = au_pal_full[2]),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_light_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_light(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      panel.grid = element_line(colour = alpha(au_pal_full[2], alpha = 0.1)),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_dark_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_dark(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      legend.background = element_rect(fill = alpha(au_pal_full[2], alpha = 0.5)),
      panel.background = element_rect(fill = alpha(au_pal_full[2], alpha = 0.5)),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_minimal_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_minimal(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      panel.grid = element_line(colour = alpha(au_pal_full[2], alpha = 0.1)),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_classic_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_classic(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_void_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_void(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_test_col <- function (base_size = 11, base_family = "AU Passata", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_au_test(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      strip.background =element_rect(fill = au_pal_full[2]))
}

#' @export
theme_au_transparent_col <- function (base_size = 11, base_family = "AU Passata"){
  theme_au_transparent(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text = element_text(size = base_size, color = au_pal_full[2]),
      line = element_line(color = au_pal_full[2]),
      strip.background =element_rect(fill = au_pal_full[2]))
}




################### Scale colors #####################

#' @export
pal_au <- function (palette = names(au_pal), alpha = 1)
{
  palette = match.arg(palette)
  if (alpha > 1L | alpha <= 0L)
    stop("alpha must be in (0, 1]")
  raw_cols = au_pal[[palette]]
  raw_cols_rgb = col2rgb(raw_cols)
  alpha_cols = rgb(raw_cols_rgb[1L, ], raw_cols_rgb[2L, ],
                   raw_cols_rgb[3L, ], alpha = alpha * 255L, names = names(raw_cols),
                   maxColorValue = 255L)
  manual_pal(unname(alpha_cols))
}

#' @export
scale_color_au <- function (palette = names(au_pal), alpha = 1, ...)
{
  palette = match.arg(palette)
  discrete_scale("colour", "au", pal_au(palette,
                                        alpha), ...)
}

#' @export
scale_fill_au <- function(palette = names(au_pal), alpha = 1, ...)
{
  palette = match.arg(palette)
  discrete_scale("colour", "au", pal_au(palette,
                                          alpha), ...)
}



################# Scale shapes ##################

#' @export
scale_shape_au <- function(shapes = "JDS")
{scale_shape_manual(values = c("\u25A1", "\u25A0", "\u25CB", "\u5CF",
                                 "\u25B2", "\u25BC", "\u002B", "\u2715"))}



