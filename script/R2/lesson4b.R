# -- head -- #

set.seed(1337)

install.packages('sjPlot')

library(tidyverse)
library(broom)
library(performance)
library(sjPlot)

# -- read -- #

d1 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d1b.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d2b.tsv')
d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d3b.tsv')
d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d4b.tsv')
d5 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d5b.tsv')
d6 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/l4d6b.tsv')

# -- ex1 -- #

lm1 = lm(y ~ x, data = d1)
summary(lm1)
plot_model(lm1, 'est')
plot_model(lm1, 'pred')
check_model(lm1)
# form groups, 
# visualise data,
# plot_model(name_of_model, 'pred')
# fit a model on d,
# check model
diagnostic_plots = plot(check_model(lm1, panel = FALSE))
diagnostic_plots[[1]]
diagnostic_plots[[2]]
diagnostic_plots[[3]]
diagnostic_plots[[4]]
diagnostic_plots[[5]]

lm2 = lm(y ~ x, data = d2)
summary(lm2)
plot_model(lm2, 'pred')
check_heteroscedasticity(lm2)

lm3 = lm(y ~ x, data = d3)
summary(lm3)
plot_model(lm3, 'pred')
check_heteroscedasticity(lm3)

lm4 = lm(y ~ x, data = d4)
summary(lm4)
plot_model(lm4, 'pred')
check_heteroscedasticity(lm4)
check_normality(lm4)
check_normality(lm3)

# -- glm -- #


# -- ex2 -- #

d5

glm1 = glm(y ~ x, family = binomial, data = d5)
summary(glm1)
plot_model(glm1, 'pred')

d6

glm2 = glm(cbind(a,b) ~ x, family = binomial, data = d6)
summary(glm2)
plot_model(glm2, 'pred')
plots = plot(check_model(glm2, panel = F))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
