# -- head -- #

library(tidyverse) # adatmanipuláció, ábrák
library(ggthemes) # ábrák
library(broom) # adattáblák
library(performance) # diagnosztika

# -- maths -- #

# how ordinary least squares work
OLS = function(x, y) {
  # calculate the mean of x and y
  x_bar = mean(x)
  y_bar = mean(y)
  
  # calculate the sum of squares
  SS_xy = sum((x - x_bar) * (y - y_bar))
  SS_xx = sum((x - x_bar) ^ 2)
  
  # calculate the regression coefficients
  b_1 = SS_xy / SS_xx
  b_0 = y_bar - b_1 * x_bar
  
  # return the regression coefficients
  return(c(b_0, b_1))
}

# how it compares to a Pearson correlation
COR_from_OLS = function(x, y) {
  x_bar = mean(x)
  y_bar = mean(y)
  
  # same building blocks as OLS()
  SS_xy = sum((x - x_bar) * (y - y_bar))
  SS_xx = sum((x - x_bar) ^ 2)
  SS_yy = sum((y - y_bar) ^ 2)
  
  # b_1, straight from OLS()
  b_1 = SS_xy / SS_xx
  
  # r, direct definition
  r_direct = SS_xy / sqrt(SS_xx * SS_yy)
  
  # r, recovered by rescaling b_1 (r = b * sd(x)/sd(y))
  r_from_b = b_1 * sqrt(SS_xx) / sqrt(SS_yy)
  
  return(c(b_1 = b_1, r_direct = r_direct, r_from_b = r_from_b, R2 = r_direct^2))
}

# OLS on standardised vars is same as cor on anything
OLS_standardised = function(x, y) {
  # standardise both variables: mean 0, sd 1
  x_std = (x - mean(x)) / sd(x)
  y_std = (y - mean(y)) / sd(y)
  
  # feed into the existing OLS()
  coefs = OLS(x_std, y_std)
  
  b_0_std = coefs[1]
  b_1_std = coefs[2]
  
  return(c(b_0_std = b_0_std, b_1_std = b_1_std, r = cor(x, y)))
}

# -- en -- #

d = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l1d1.tsv')
d
d |> 
  ggplot(aes(x,y)) +
  geom_point() +
  theme_few() +
  geom_smooth(method = 'lm') +
  geom_vline(xintercept = 0)

lm = lm(y ~ 1 + x, data = d)
lm = lm(y ~ x, data = d)
lm
summary(lm)
tidy(lm)
glance(lm)
augment(lm)

OLS(x,y)
OLS(d$x,d$y)
COR_from_OLS(d$x,d$y)
OLS_standardised(d$x,d$y)

d$pred = predict(lm)

d |> 
  ggplot(aes(x,pred)) +
  geom_point() +
  theme_few()

d |> 
  mutate(id = 1:n()) |> 
  ggplot(aes(x,y)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = lm$coefficients[1], slope = lm$coefficients[2])) +
  theme_few() +
  geom_linerange(aes(x = x, ymin = y, ymax = pred, group = id), colour = 'red')

estimate = tidy(lm) |> 
  filter(term == 'x') |> 
  pull(estimate)

std_error = tidy(lm) |> 
  filter(term == 'x') |> 
  pull(std.error)

estimate/std_error

# -- ti -- #

d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l1d2.tsv')
d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l1d3.tsv')
d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l1d4.tsv')

