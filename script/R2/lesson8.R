# multivariate logistic regression

# -- head -- #

library(tidyverse)
library(broom)
library(performance)
library(sjPlot)
library(GGally)

# -- read -- #

d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/married.csv')

# -- setup -- #

d = d |> 
  mutate(
    age = scale(age),
    pitch = scale(pitch),
    extraversion = scale(extraversion),
    sociability = scale(sociability),
    married_factor = ifelse(married, 'married', 'not married')
  )

# -- plot -- #

d |> 
  ggplot(aes(job,married_factor)) +
  geom_col()

d |> 
  ggplot(aes(age,married_factor)) +
  geom_boxplot()