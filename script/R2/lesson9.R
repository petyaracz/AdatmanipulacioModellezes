# -- intro -- #

# https://mfviz.com/hierarchical-models/
# https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/

# -- setup -- #

# install the lme4 and broom.mixed packages
install.packages("lme4")
install.packages("broom.mixed")

library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(lme4)

# -- read -- #

d = read_tsv("https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/lesson9.tsv")

# -- take a look -- #

glimpse(d)

unique(d$participant)
unique(d$word)

# -- visualise -- #

d |> 
  ggplot(aes(word_familiarity,log_rt)) +
  geom_point() +
  geom_smooth(method = 'lm')

d |> 
  ggplot(aes(participant_vocabulary_size,log_rt)) +
  geom_point() +
  geom_smooth(method = 'lm')

# -- participant-level effects -- #

d |> 
  filter(keep_participant) |> 
  ggplot(aes(word_familiarity,log_rt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ participant)

d |> 
  filter(keep_word) |> 
  ggplot(aes(participant_vocabulary_size,log_rt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ word)

# -- aggregate -- #

## participants

participants = d |> 
  group_by(participant,participant_vocabulary_size) |> 
  summarise(mean_participant_rt = mean(log_rt))

participants |> 
  ggplot(aes(participant_vocabulary_size,mean_participant_rt)) +
  geom_point()

plm1 = lm(log_rt ~ participant_vocabulary_size, data = d)
plm2 = lm(mean_participant_rt ~ participant_vocabulary_size, data = participants)
plm1b = lm(log_rt ~ participant_vocabulary_size * participant, data = d)

tidy(plm1, conf.int = T)
tidy(plm2, conf.int = T)
tidy(plm1b, conf.int = T) # lol

## words

# word, word_familiarity

# wlm1, wlm2

# -- mixed model -- #

plm3 = lmer(log_rt ~ participant_vocabulary_size + (1|participant), data = d)

tidy(plm1, conf.int = T)
tidy(plm2, conf.int = T)
tidy(plm3, conf.int = T)

# wlm3...

# -- nested model -- #

lmm1 = lmer(log_rt ~ participant_vocabulary_size + word_familiarity + (1|participant) + (1|word), data = d)

tidy(lmm1, conf.int = T)

lmm2 = lmer(log_rt ~ participant_vocabulary_size * word_familiarity + (1|participant) + (1|word), data = d)

tidy(lmm2, conf.int = T)

# health
check_model(lmm1)
check_model(lmm2)
# check_collinearity(lmm1)
# check_autocorrelation(lmm1)
# check_heteroscedasticity(lmm1)
# check_residuals(lmm1)
# check_collinearity(lmm2)
# check_autocorrelation(lmm2)
# check_heteroscedasticity(lmm2)
# check_residuals(lmm2)

# comparison
plot(compare_performance(lmm1,lmm2, metrics = 'common'))
test_likelihoodratio(lmm1,lmm2)
test_performance(lmm2,lmm1)

# viz
plot_model(lmm2, 'est')

plot_model(lmm2, 'pred', terms = c('participant_vocabulary_size','word_familiarity'))
plot_model(lmm2, 'pred', terms = c('word_familiarity','participant_vocabulary_size'))
