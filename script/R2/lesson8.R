library(tidyverse)
library(glue)
library(broom)
library(performance)
library(sjPlot)
library(psych)

d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/lesson8.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/refs/heads/main/dat/credit_risk.tsv')

# use pairs.panel to look at data
pairs.panels(d)
# what are the possible models?
lm1 = glm(Survived ~ Class + Age + Sex, data = d, family = binomial)
lm2 = glm(Survived ~ Class * Age + Sex, data = d, family = binomial)
lm3 = glm(Survived ~ Class + Age * Sex, data = d, family = binomial)
lm4 = glm(Survived ~ Class * Sex + Age, data = d, family = binomial)

# what is the best model?
plot(compare_performance(lm1, lm2, lm3, lm4, metrics = 'common'))
test_wald(lm1,lm4)
check_collinearity(lm4)
count(d,Sex,Class)
# does the best model meet the assumptions?
check_model(lm4)
plot(compare_performance(lm1, lm2, lm3, metrics = 'common'))
check_collinearity(lm3)
tidy(lm3, conf.int = TRUE, conf.level = 0.95)
plot_model(lm3, 'pred', terms = c('Age','Sex','Class'))
# why? why not?
# what is the best model that does meet the assumptions?
# what does this model predict?

d2
