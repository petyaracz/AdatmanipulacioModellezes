# installs?

install.packages('glmmTLMB')
install.packages('broom.mixed')
install.packages('patchwork')

# -- setup -- #

library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(broom.mixed)
library(sjPlot)
library(performance)
library(patchwork)

# -- read, centralise -- #

d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/lexical_decision.csv') |>
  mutate(age_c = participant_age - 18, freq_c = word_frequency - 4)

# 48 participants x 100 words = 4800 trials. Participants and words are
# crossed: every participant saw every word.

# -- aggregate -- #

participants = d |> 
  summarise(
    log_rt = mean(log_rt),
    .by = c(participant,participant_age,age_c)
  )

words = d |> 
  summarise(
    log_rt = mean(log_rt),
    .by = c(word,word_frequency,freq_c)
  )


# -- eda -- #

p1 = d |> 
  ggplot(aes(participant_age,log_rt)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  ggtitle('participant responses')

p2 = participants |> 
  ggplot(aes(participant_age,log_rt)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  ggtitle('participants, aggregated')

p3 = d |> 
  ggplot(aes(word_frequency,log_rt)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  ggtitle('word responses')

p4 = words |> 
  ggplot(aes(word_frequency,log_rt)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'lm') +
  ggtitle('words, aggregated')

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

d |> 
  ggplot(aes(participant_age,word_frequency, fill = log_rt)) +
  geom_tile() # huha

d |> 
  mutate(
    participant = fct_reorder(participant, participant_age),
    word = fct_reorder(word, word_frequency)
  ) |> 
  ggplot(aes(participant,word, fill = log_rt)) +
  geom_tile() +
  scale_fill_viridis_b() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# -- linear modelling -- #

# everything

lm1 = lm(log_rt ~ freq_c * age_c, d)
tidy(lm1, conf.int = T)

# participants

lm2 = lm(log_rt ~ age_c, participants)
tidy(lm2, conf.int = T)

# words

lm3 = lm(log_rt ~ freq_c, words)
tidy(lm3, conf.int = T)

# -- linear mixed model / hierarchical model -- #

lmm1 = glmmTMB(log_rt ~ freq_c * age_c + (1 | participant) + (1 | word), d)

tidy(lmm1, conf.int = T)
# cf
tidy(lm1, conf.int = T)

plot_model(lmm1, 'pred', terms = c('age_c','freq_c'))
plot_model(lm1, 'pred', terms = c('age_c','freq_c'))

# -- false flags -- #

lm4 = lm(log_rt ~ participant_session, d)
tidy(lm4, conf.int = T)
lmm2 = glmmTMB(log_rt ~ participant_session + (1|participant), d)
tidy(lmm2, conf.int = T)
# hopszika

# -- önök kérték -- #

d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/wine.csv')

# rater: rater ID, r01 to r40
# wine: wine ID, w01 to w32
# position: order in which this rater tasted this wine, 1 to 32
# rating: how much the rater liked the wine, 0 to 100
# wine_price: shop price in euros, 3 to 54
# wine_colour: red or white
# wine_alcohol: % ABV, 11.4 to 14.6
# wine_acidity: titratable acidity in g/L, 4.6 to 7.5
# rater_expertise: novice (23 raters) or expert (17)