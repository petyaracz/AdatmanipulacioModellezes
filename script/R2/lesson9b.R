install.packages('lme4')
install.packages('broom.mixed')

library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(psych)
library(janitor)
library(lme4)

# load d
d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/lesson9.tsv')
# d
d
# take d, plot resp.rt ~ word_familiarity with points and a line
d %>% 
  ggplot(aes(x = word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()
# take d, scale word familiarity and participant age and vocabulary size
d = d %>% 
  mutate(
    scaled_word_familiarity = scale(word_familiarity),
    scaled_age = scale(participant_age),
    scaled_vocabulary_size = scale(participant_vocabulary_size)
         )
# take d, plot resp.rt ~ scaled_word_familiarity with points and a line
d %>% 
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()
# take d, sample 10 participants, take their id
couple_participants = d %>% 
  distinct(participant) %>% 
  sample_n(10) %>% 
  pull()
# use couple_participants to filter d, plot resp.rt ~ scaled_word_familiarity with points and a line
d %>% 
  filter(participant %in% couple_participants) %>% 
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt, colour = participant)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  guides(colour = FALSE) +
  theme_bw()
# do the same with facet wraps
d %>% 
  filter(participant %in% couple_participants) %>% 
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt, colour = participant)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  guides(colour = FALSE) +
  theme_bw() +
  facet_wrap(~participant)
# fit a linear model for resp.rt ~ scaled_word_familiarity
lm1 = lm(resp.rt ~ scaled_word_familiarity, data = d)
tidy(lm1, conf.int = T)
# aggregate data
dw = d %>% 
  group_by(word,scaled_word_familiarity) %>%
  summarise(mean_rt = mean(resp.rt))
# fit a second model on the aggregated data
lm2 = lm(mean_rt ~ scaled_word_familiarity, data = dw)
tidy(lm2, conf.int = T)
# plot resp.rt ~ participant_age
d %>% 
  ggplot(aes(x = scaled_age, y = resp.rt)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()
# plot resp.rt ~ scaled_age + scaled_word_familiarity
d %>% 
  mutate(ntile_vocabulary_size = as.factor(ntile(scaled_vocabulary_size,4))) %>% 
  ggplot(aes(x = scaled_word_familiarity, y = resp.rt, colour = ntile_vocabulary_size)) +
  # geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

# fit a model
lm3 = lm(resp.rt ~ scaled_vocabulary_size, data = d)
tidy(lm3, conf.int = T)
# aggregate data
dp = d %>% 
  group_by(participant,scaled_vocabulary_size) %>%
  summarise(mean_rt = mean(resp.rt))
# fit a second model on the aggregated data
lm4 = lm(mean_rt ~ scaled_vocabulary_size, data = dp)
tidy(lm4, conf.int = T)

# age + vocabulary size
lm5 = lm(resp.rt ~ scaled_age + scaled_vocabulary_size, data = d)
lm6 = lm(resp.rt ~ scaled_age * scaled_vocabulary_size, data = d)
plot(compare_performance(lm5,lm6))
tidy(lm6, conf.int = T)

glm1 = lmer(resp.rt ~ scaled_age + scaled_vocabulary_size + (1|participant), data = d)
glm2 = lmer(resp.rt ~ scaled_age * scaled_vocabulary_size + (1|participant), data = d)
plot(compare_performance(glm1,glm2))
tidy(glm2, conf.int = T)

ranef(glm1)

glm3 = lmer(resp.rt ~ scaled_age + scaled_vocabulary_size + (1|participant) + (1|word), data = d)
plot(compare_performance(glm1,glm3))

ranef(glm3)$word
ranef(glm3)$participant
