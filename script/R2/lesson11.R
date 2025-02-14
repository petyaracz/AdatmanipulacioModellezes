library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(lme4)

d = read_tsv("https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l11d1.tsv")

d

# drop NA
d = d %>% drop_na()

# boxplot rt ~ stimulus_congruent
ggplot(d, aes(x = stimulus_congruent, y = rt)) +
  geom_boxplot()

# boxplot rt ~ response_correct
ggplot(d, aes(x = response_correct, y = rt)) +
  geom_boxplot()

# boxplot rt ~ artist
ggplot(d, aes(x = artist, y = rt)) +
  geom_boxplot()

# boxplot rt ~ stimulus_congruent x artist
ggplot(d, aes(x = stimulus_congruent, y = rt, color = artist)) +
  geom_boxplot()

# mik lehetnek fixed effektek?

# mik lehetnek grouping faktorok?

fit1 = lmer(rt ~ stimulus_congruent + response_correct + artist + (1|subject) + (1|word) + (1|ink), data = d)
tidy(fit1)

fit2 = lmer(rt ~ stimulus_congruent + response_correct + artist + (1|subject), data = d)
tidy(fit2, conf.int = T)

fit3 = lmer(rt ~ stimulus_congruent * response_correct + artist + (1|subject), data = d)
fit4 = lmer(rt ~ stimulus_congruent * artist + response_correct + (1|subject), data = d)
tidy(fit4, conf.int = T)
plot(compare_performance(fit2,fit4))
test_likelihoodratio(fit2,fit4)

# random effekt struktura

fit5 = lmer(rt ~ stimulus_congruent + artist + response_correct + (1 + stimulus_congruent | subject), data = d)

plot(compare_performance(fit2,fit5))

fit6 = lmer(rt ~ stimulus_congruent + artist + response_correct + (1 + stimulus_congruent + response_correct |subject), data = d)
# fit6 with bobyqa
fit7 = lmer(rt ~ stimulus_congruent + artist + response_correct + (1 + stimulus_congruent + response_correct |subject), data = d, control = lmerControl(optimizer = "bobyqa"))

# model health
check_model(fit1)
# specifically
check_collinearity(fit1)
check_normality(fit1)
check_posterior_predictions(fit1)

# model comparison
plot(compare_performance(fit1, fit3))
test_likelihoodratio(fit1, fit3)
plot(compare_performance(fit1, fit4))
test_likelihoodratio(fit1, fit4)

