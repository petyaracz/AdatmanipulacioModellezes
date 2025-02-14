library(tidyverse)
library(performance)
library(sjPlot)
library(psych)

d = read_tsv('~/Github/class_advanced_r/dat/lesson8.tsv')

d |> 
  psych::pairs.panels()

lm1 = glm(Survived ~ Class + Age + Sex, data = d)
lm2 = glm(Survived ~ Class * Age + Sex, data = d)
lm3 = glm(Survived ~ Class + Age * Sex, data = d)
lm4 = glm(Survived ~ Class * Sex + Age, data = d)
lm5 = glm(Survived ~ Class * Age * Sex, data = d)

check_collinearity(lm5)
check_collinearity(lm4)
check_collinearity(lm3) # ok
check_collinearity(lm2)

plot(compare_performance(lm1,lm3, metrics = 'common'))
test_performance(lm1,lm3)

plot_model(lm3, 'est', show.intercept = T)
plot_model(lm3)
plot_model(lm3, 'pred', terms = c('Age','Sex','Class')) +
  scale_y_continuous(sec.axis = sec_axis(transform = ~ plogis(.)))

d = d |> 
  mutate(
    Age_centered = Age - median(Age, na.rm = T)
  )

lm3b = lm(Survived ~ Age_centered * Sex + Class, data = d)

plot_model(lm3b, 'est', show.intercept = T)
