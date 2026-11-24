
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(sjPlot)
library(performance)
library(patchwork)

d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/wine.csv')

# rater: rater ID, r01 to r40
# wine: wine ID, w01 to w32
# position: order in which this rater tasted this wine, 1 to 32
# rating: how much the rater liked the wine, 0 to 100
# wine_price: shop price in euros, 3 to 54
# wine_colour: red or white
# wine_alcohol: % ABV, 11.4 to 14.6
# wine_acidity: titratable acidity in g/L, 4.6 to 7.5
# rater_expertise: novice (23 raters) or expert (17)

# -- centralise -- #

d = d |> 
  mutate(
    rater_expertise = fct_relevel(rater_expertise, 'novice'),
    rating_c = rating - 50,
    wine_price_c = wine_price - 30,
    wine_alcohol_c = wine_alcohol - 13,
    wine_acidity_c = wine_acidity - 6
  )

# -- visualise -- #

ggplot(d, aes(wine_price, rating, group = rater, colour = rater_expertise)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4) + scale_x_log10()
ggplot(d, aes(wine_alcohol, rating, group = rater, colour = rater_expertise)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.4)

# ... 

# -- fit -- #