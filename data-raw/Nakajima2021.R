# Effects of Grazing in a Sown Pasture with Forestland on the Health of
# Japanese Black Cows as Evaluated by Multiple Indicators

# Noriaki Nakajima, Kazuya Doi, Sae Tamiya and Masato Yayota

# Dataset public available at:

# https://www.tandfonline.com/doi/full/10.1080/10888705.2020.1813581

# These dataset is only use as an example and none of the analysis from
# these examples should be considered.


library(readxl)

Nakajima2021_bloodparameters <- readxl::read_xlsx("data-raw/Raw data.xlsx", sheet = "Blood parameters")
Nakajima2021_behavioralparameters1 <- readxl::read_xlsx("data-raw/Raw data.xlsx", sheet = "Behavioral parameters 1")
Nakajima2021_behavioralparameters2 <- readxl::read_xlsx("data-raw/Raw data.xlsx", sheet = "Behavioral parameters 2")

usethis::use_data(Nakajima2021_bloodparameters, overwrite = TRUE)
usethis::use_data(Nakajima2021_behavioralparameters1, overwrite = TRUE)
usethis::use_data(Nakajima2021_behavioralparameters2, overwrite = TRUE)
