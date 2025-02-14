# -- head -- #

library(tidyverse)
library(broom)
library(performance)

# -- read -- #

d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l5d4.tsv')
d5 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l5d5.tsv')
housing = read_csv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/Housing.csv') 

# -- main -- #

# sjPlot

install.packages('sandwich')
install.packages('sjPlot')
library(sjPlot)

# 1. factor

# factor + numeric

d4 %>% 
  ggplot(aes(wage, edu, colour = gender)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw()

lm1 = lm(wage ~ edu, data = d4)
lm2 = lm(wage ~ edu + gender, data = d4)

tidy(lm2, conf.int = T)
compare_performance(lm1,lm2, metrics = 'common')
plot(compare_performance(lm1,lm2, metrics = 'common'))
test_performance(lm1,lm2)
check_model(lm2)

# The Test of Distinguishability (the Omega2 column and its associated p-value) indicates whether or not the models can possibly be distinguished on the basis of the observed data. If its p-value is significant, it means the models are distinguishable.

# The Robust Likelihood Test (the LR column and its associated p-value) indicates whether each model fits better than the reference model. If the models are nested, then the test works as a robust LRT. The code for this function is adapted from the nonnest2 package, and all credit go to their authors.

plot_model(lm2, 'est')
plot_model(lm2, 'pred')
plot_model(lm2, 'pred', terms = 'edu')
plot_model(lm2, 'pred', terms = c('edu','gender'))

# factor * numeric

# look at d5
# what are x? what is y?
# plot y ~ x1 + x2
# fit a linear model
# what is a (the intercept)? what are b? what are their confidence intervals?

# a real dataset!
# HOUSING

summary(housing)
glimpse(housing)

lm5 = lm(price ~ area + bedrooms + bathrooms + guestroom + basement + hotwaterheating + airconditioning + parking + furnishingstatus, data = housing)
lm6 = lm(price ~ bedrooms + bathrooms + guestroom + basement + hotwaterheating + airconditioning + parking + furnishingstatus, data = housing)

tidy(lm5, conf.int = T)

check_model(lm5)
plot_model(lm5, 'est')
plot_model(lm5, 'pred', terms = c('area','furnishingstatus'))
plot(compare_performance(lm5,lm6, metrics = 'common'))
test_performance(lm5,lm6)

# area * hotwaterheating
lm7 = lm(price ~ area * hotwaterheating, data = housing)
lm8 = lm(price ~ area + hotwaterheating, data = housing)

tidy(lm7, conf.int = T)
plot(compare_performance(lm7,lm8, metrics = 'common'))
test_performance(lm7,lm8)
plot_model(lm8, 'pred', terms = c('area','hotwaterheating'))
# area * bathrooms
# area * airconditioning
