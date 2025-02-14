# -- head -- #

install.packages('lme4')
install.packages('broom.mixed')

library(tidyverse)
library(lme4)
library(broom.mixed)
library(performance)
library(sjPlot)
library(knitr)

# -- fun -- #

tidy2 = partial(tidy, conf.int = T)
kable2 = partial(kable, digits = 2)
tidy3 = compose(kable2, tidy2) 

# -- read -- #

d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/lesson9.tsv')

# -- scale -- #

d = d |> 
  mutate(
    scaled_word_familiarity = scale(word_familiarity),
    scaled_participant_vocabulary_size = scale(participant_vocabulary_size)
  )

d |> 
  ggplot(aes(scaled_word_familiarity,word_familiarity)) +
  geom_point()

d |> 
  ggplot(aes(scaled_word_familiarity,word_familiarity)) +
  geom_point() +
  facet_wrap( ~ participant)

# participant vocab x word

# -- vis -- #

glimpse(d)

# outcome: resp.rt
# predictors: word_familiarity, participant_vocabulary_size

d |> 
  ggplot(aes(scaled_word_familiarity,resp.rt)) +
  geom_point()

words = d |> 
  group_by(word,scaled_word_familiarity) |> 
  summarise(mean_rt = mean(resp.rt))

# participants = mean rt x participant, scaled_participant_vocabulary_size

words |> 
  ggplot(aes(scaled_word_familiarity,mean_rt)) +
  geom_point()

# plot: mean rt x scaled_participant_vocabulary_size

# -- model: all data -- #

# model, check, performance -> ...

# -- aggr data -- #

# words

lm2a = lm(mean_rt ~ scaled_word_familiarity, data = words)

# vs all data?

# participants

# model?
# vs all data?

# -- hierarchical model -- #

lm3a = lmer(resp.rt ~ scaled_word_familiarity + (1|word), data = d)

lm3b = lmer(resp.rt ~ scaled_word_familiarity + (1|participant) + (1|word), data = d)

# and participant vocabulary size?

# model, check, performance...