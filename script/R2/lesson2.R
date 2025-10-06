# plot
# predict
# CI / error / t / p

# -- head -- #

library(tidyverse)
library(broom)
library(performance)

# -- functions -- #

plotLM = function(dat,x,y){
  dat |> 
    ggplot(aes({{x}},{{y}})) +
    geom_point() +
    geom_smooth(method = 'lm') +
    theme_bw()
}

# -- read -- #

d1 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/l2d1.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/l2d2.tsv')
d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/l2d3.tsv')
d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/l2d4.tsv')
d5 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/refs/heads/main/dat/R2/l2d5.tsv')

# -- main -- #

# --- d1: heights, not centered --- #

## plot
plotLM(d1,height_father,height_son)

## correlation
with(d1, cor(height_father,height_son))

## linear model fit
lm1 = lm(height_son ~ height_father, data = d1)

## linear model: a and b (or b0 and b1)
lm1

## linear model: summmary
summary(lm1)

## linear model: R2
r2(lm1)

## linear model: tidy, confint
tidy(lm1, conf.int = T)

## the same thing:
confint(lm1)

# --- intercept: a nulla centis apa gyermeke --- #

# --- d2: heights, f centered --- #

# --- d3: heights, f scaled --- #

# --- d4: limbo! --- #

# --- d5: outliers --- #

# -- predicting -- #

str(d1)
predict(lm1)
predict(lm1, tibble(height_father = 195))


predict(lm2)
predict(lm2, tibble(height_father_c = 195))

predict(lm1, tibble(height_father = 195), interval='confidence')
predict(lm2, tibble(height_father_c = 195), interval='confidence')
