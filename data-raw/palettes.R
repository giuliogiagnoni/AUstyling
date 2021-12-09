au_pal <- vector("list")

au_pal$"full" <- c("#003d73", "#002546", "#655a9f", "#281c41",
                 "#37a0cb", "#003e5c", "#00aba4", "#004543",
                 "#8bad3f", "#425821", "#fabb00", "#634b03",
                 "#ee7f00", "#5f3408", "#e2001a", "#5b0c0c",
                 "#e2007a", "#5f0030", "#878787", "#4b4b4a")

au_pal$"light" <- c("#003d73", "#655a9f", "#37a0cb", "#00aba4", "#8bad3f",
                  "#fabb00", "#ee7f00", "#e2001a", "#e2007a", "#878787")

au_pal$"dark" <- c("#002546", "#281c41", "#003e5c", "#004543", "#425821",
                 "#634b03", "#5f3408", "#5b0c0c", "#5f0030", "#4b4b4a")

au_pa$"blue" <- c("#002546", "#0A1449", "#183D83",
                 "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"purple" <- c("#281C41", "#0A1449", "#183D83",
                   "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"lightblue" <- c("#003E5C", "#0A1449", "#183D83",
                      "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"turquoise" <- c("#004543", "#0A1449", "#183D83",
                      "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"green" <- c("#425821", "#0A1449", "#183D83",
                  "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"yellow" <- c("#634B03", "#0A1449", "#183D83",
                   "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"orange" <- c("#5F3408", "#0A1449", "#183D83",
                   "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"red" <- c("#5B0C0C", "#0A1449", "#183D83",
                "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"pink" <- c("#5F0030", "#0A1449", "#183D83",
                 "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"black" <- c("#000000", "#4B4B4A", "#0A1449", "#183D83",
                  "#87D1F4", "#33525F", "#548195", "#C6C6C6")

au_pal$"rainbow" <- c("#002546", "#281C41", "#003E5C", "#004543", "#425821",
                    "#634B03", "#5F3408", "#5B0C0C", "#5F0030", "#000000")


au_pal$"customset1.4" <- au_pal$"full"[seq(1, length(au_pal$"full"), 6)]

au_pal$"customset2.5" <- au_pal$"full"[seq(1, length(au_pal$"full"), 4)]

au_pal$"customset3.6" <- au_pal$"full"[seq(1, length(au_pal$"full"), 3.5)]

au_pal$"customset4.6" <- au_pal$"full"[seq(2, length(au_pal$"full"), 3.5)]



#### Experiment palette

au_pal$"F2238" <- c("#ff0000", "#ed7d31", "#6f30a0", "#a6a6a6")

au_pal$"F2505" <- c("#ffff00", "#ffffff", "#bfbfbf",
                    "#000000", "#0070c0", "#6f30a0")


#### to implement ....

au_shape$"JDS" <- c("\u25A1", "\u25A0", "\u25CB", "\u5CF",
                    "\u25B2", "\u25BC", "\u002B", "\u2715")


usethis::use_data(au_pal, overwrite = TRUE)
