library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(lme4)

d = read_tsv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l11d1.tsv")

d

# drop NA
d = d |> drop_na()
