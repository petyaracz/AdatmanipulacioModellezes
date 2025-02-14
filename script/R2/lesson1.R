# -- head -- #

library(tidyverse)
library(ggthemes)
library(broom)
library(performance)

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

# -- en -- #

d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l1d1.tsv')
d
d %>% 
  ggplot(aes(x,y)) +
  geom_point() +
  theme_few() +
  geom_smooth(method = 'lm') +
  geom_vline(xintercept = 0) +
  coord_cartesian(xlim = c(-50,50), ylim = c(-200,200))

lm = lm(y ~ 1 + x, data = d)
lm = lm(y ~ x, data = d)
lm
summary(lm)
tidy(lm)
glance(lm)
augment(lm)

OLS(x,y)
OLS(d$x,d$y)

d$pred = predict(lm)

d %>% 
  ggplot(aes(x,pred)) +
  geom_point() +
  theme_few()

d %>% 
  mutate(id = 1:n()) %>% 
  ggplot(aes(x,y)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  geom_abline(aes(intercept = lm$coefficients[1], slope = lm$coefficients[2])) +
  theme_few() +
  geom_linerange(aes(x = x, ymin = y, ymax = pred, group = id), colour = 'red') +
  coord_cartesian(ylim = c(-100,100),)

estimate = tidy(lm) %>% 
  filter(term == 'x') %>% 
  pull(estimate)
std_error = tidy(lm) %>% 
  filter(term == 'x') %>% 
  pull(std.error)
estimate/std_error

# -- ti -- #


