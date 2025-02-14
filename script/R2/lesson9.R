# -- starting questions -- #

# what is the effect of vocabulary size and word familiarity?
# is there an interaction?
# what is the structure of the data?
# what is the result of clustering?

# -- setup -- #

# install the lme4 and broom.mixed packages
install.packages("lme4")
install.packages("broom.mixed")

# load the tidyverse, broom.mixed, performance, sjPlot, psych, janitor, and lme4 libraries
library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(psych)
library(janitor)
library(lme4)

# load table from 'https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/lesson9.tsv' and assign to d
d <- read_tsv("https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/lesson9.tsv")

d %>% View

# take d, scale word familiarity and vocabulary size, assign to d
d <- d %>% mutate(scaled_word_familiarity = scale(word_familiarity), scaled_participant_vocabulary_size = scale(participant_vocabulary_size))

# -- eda: word familiarity -- #

# take d, plot resp.rt ~ word_familiarity with points and a line
d %>% 
  ggplot(aes(x = word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm")

# take d, plot resp.rt ~ scaled_word_familiarity with points and a line
d %>% 
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm")

# -- eda: word familiarity and vocabulary size -- #

# take d, sample 10 participants, filter data for these ten participants, plot resp.rt ~ scaled_word_familiarity with points and a line


# not quite

# take d, sample 10 participant ids, assign to ids
ids = d %>% 
  distinct(participant) %>%
  sample_n(10)

# take d, filter for participant ids in ids, plot resp.rt ~ scaled_word_familiarity with points and a line
d %>% 
  filter(participant %in% ids$participant) %>%
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm")

# repeat the plot with facet wraps for participants
d %>% 
  filter(participant %in% ids$participant) %>%
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ participant)

# -- modelling: word familiarity and vocabulary size -- #

# fit a linear model on d predicting resp.rt from scaled_word_familiarity, assign to lm1
lm1 = lm(resp.rt ~ scaled_word_familiarity, data = d)

# take lm1, get a tidy table of coefficient estimates with confidence intervals
tidy(lm1, conf.int = TRUE)

# take d, group by word and scaled word familiarity, summarise mean resp.rt as mean_rt, assign table to dw
dw = d %>% 
  group_by(word, scaled_word_familiarity) %>%
  summarise(mean_rt = mean(resp.rt))

# take dw, plot mean_rt across scaled_word_familiarity with points and a line
dw %>% 
  ggplot(aes(x = scaled_word_familiarity, y = mean_rt)) +
  geom_point() +
  geom_smooth(method = "lm")

dw %>% 
  ggplot(aes(x = scaled_word_familiarity, y = mean_rt, label = word)) +
  geom_label() +
  geom_smooth(method = "lm")

# fit a second model on the aggregated data, assign to lm2
lm2 = lm(mean_rt ~ scaled_word_familiarity, data = dw)

# get a tidy table of coefficient estimates with confidence intervals
tidy(lm2, conf.int = TRUE)

# take d, plot resp.rt ~ scaled participant vocabulary size with points and a line
d %>% 
  ggplot(aes(x = scaled_participant_vocabulary_size, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = "lm")

# take d, bin scaled vocabulary size into four bins, plot resp.rt ~ scaled word familiarity with binned participant vocabulary size as colour, with points and a line
d %>% 
  mutate(scaled_participant_vocabulary_size_binned = cut(scaled_participant_vocabulary_size, breaks = 4)) %>%
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt, colour = scaled_participant_vocabulary_size_binned)) +
  geom_point() +
  geom_smooth(method = "lm")

# repeat plot but remove points
d %>% 
  mutate(scaled_participant_vocabulary_size_binned = cut(scaled_participant_vocabulary_size, breaks = 4)) %>%
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt, colour = scaled_participant_vocabulary_size_binned)) +
  geom_smooth(method = "lm")

# fit a linear model on d predicting resp.rt from scaled vocabulary size, assign to lm3
lm3 = lm(resp.rt ~ scaled_participant_vocabulary_size, data = d)

# get a tidy table of coefficient estimates with confidence intervals
tidy(lm3, conf.int = TRUE)

# take d, group by participant and scaled vocabulary size, summarise mean resp.rt as mean_rt, assign table to dp
dp = d %>% 
  group_by(participant, scaled_participant_vocabulary_size) %>%
  summarise(mean_rt = mean(resp.rt))

# make plot lol
dp %>% 
  ggplot(aes(x = scaled_participant_vocabulary_size, y = mean_rt)) +
  geom_point() +
  geom_smooth(method = "lm")

# fit a second model on the aggregated data
lm4 = lm(mean_rt ~ scaled_participant_vocabulary_size, data = dp)

# get a tidy table of coefficient estimates with confidence intervals
tidy(lm4, conf.int = TRUE)

# fit a linear model on d predicting resp.rt from scaled vocabulary size and scaled word familiarity
lm5 = lm(resp.rt ~ scaled_participant_vocabulary_size + scaled_word_familiarity, data = d)

# fit a linear model with an interaction
lm6 = lm(resp.rt ~ scaled_participant_vocabulary_size * scaled_word_familiarity, data = d)

# use compare performance to compare lm5 and lm6 and plot the result
plot(compare_performance(lm5, lm6))

# get a tidy table of coefficient estimates with confidence intervals for lm6
tidy(lm6, conf.int = TRUE)

# -- hierarchical modelling -- #

# fit a hierarchical model on d predicting resp.rt from scaled and and scaled vocabulary size with a participant random intercept
lm7 = lmer(resp.rt ~ scaled_participant_vocabulary_size + scaled_word_familiarity + (1 | participant), data = d)

lm7 = lmer(resp.rt ~ 1 + scaled_participant_vocabulary_size + scaled_word_familiarity + (1 | participant), data = d)

# fit the same model with an interaction
lm8 = lmer(resp.rt ~ 1 + scaled_participant_vocabulary_size * scaled_word_familiarity + (1 | participant), data = d)

# compare performance of the two models and plot it
plot(compare_performance(lm7, lm8))

# get a tidy table of coefficient estimates with confidence intervals for lm7 and lm8
tidy(lm7, conf.int = TRUE)
tidy(lm8, conf.int = TRUE)

# get random effects from lm7 and plot them

# fit a hierarchical model on d predicting resp.rt from scaled and and scaled vocabulary size with a participant and a word random intercept
lm9 = lmer(resp.rt ~ 1 + scaled_participant_vocabulary_size + scaled_word_familiarity + (1 | participant) + (1 | word), data = d)

tidy(lm9, conf.int = TRUE) %>% 
  select(term,estimate,std.error,statistic)

# compare performance of the two models and plot it

# plot random effects for lm9