library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(broom.mixed)
library(sjPlot)
library(performance)

d <- d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/lexical_decision.csv') |>
  mutate(age_c = participant_age - 18, freq_c = word_frequency - 4)

# 48 participants x 100 words = 4800 trials. Participants and words are
# crossed: every participant saw every word.

# ---- 1. Trials in an ordinary model -----------------------------------------

m_lm <- lm(log_rt ~ freq_c * age_c, d)                                 # AIC 2657.6
summary(m_lm)
# Estimates are identical to the mixed model (balanced, complete data), but
# the SEs are wrong: age 0.0008 (mixed: 0.0042), frequency 0.0041 (0.0102),
# interaction 0.0007 (0.0005, so here the lm is too cautious).

# ---- 2. Aggregate over words: one row per participant ------------------------

by_participant <- d |>
  group_by(participant, participant_age, participant_session) |>
  summarise(log_rt = mean(log_rt), .groups = "drop") |>
  mutate(age_c = participant_age - 18)
m_bp <- lm(log_rt ~ age_c, by_participant)
summary(m_bp)  # age -0.0444 (SE 0.0042): fine, but frequency and the interaction are gone

# The funnel: children vary far more than young adults
ggplot(by_participant, aes(participant_age, log_rt)) + geom_point() + geom_smooth(method = "lm")

# ---- 3. Aggregate over participants: one row per word ------------------------

by_word <- d |>
  group_by(word, word_frequency, word_class) |>
  summarise(log_rt = mean(log_rt), .groups = "drop") |>
  mutate(freq_c = word_frequency - 4)
m_bw <- lm(log_rt ~ freq_c, by_word)
summary(m_bw)  # frequency -0.0722 (SE 0.0103): fine, but age and the interaction are gone

ggplot(by_word, aes(word_frequency, log_rt)) + geom_point() + geom_smooth(method = "lm")

# ---- 4. The mixed model -------------------------------------------------------

m <- glmmTMB(log_rt ~ freq_c * age_c + (1 | participant) + (1 | word), d)  # AIC 735.3
summary(m)
# Expected: intercept -0.246, frequency -0.071 (SE 0.010), age -0.044 (0.004),
# interaction 0.0048 (0.0005). SDs: participant 0.165, word 0.110, residual 0.25.

m_p <- glmmTMB(log_rt ~ freq_c * age_c + (1 | participant), d)            # AIC 1343.8
m_w <- glmmTMB(log_rt ~ freq_c * age_c + (1 | word), d)                   # AIC 2284.9
AIC(m_lm, m_p, m_w, m)

m_noint <- glmmTMB(log_rt ~ freq_c + age_c + (1 | participant) + (1 | word), d)  # +75.3
AIC(m_noint, m)

# ---- 5. Null predictors: the trial-level lm is fooled, the mixed model is not --

AIC(m_lm, update(m_lm, . ~ . + participant_session))  # -9.2
AIC(m, update(m, . ~ . + participant_session))        # +1.6
AIC(m_lm, update(m_lm, . ~ . + word_class))           # -3.8
AIC(m, update(m, . ~ . + word_class))                 # +1.1

# The aggregates agree with the mixed model on the null predictors
AIC(m_bp, update(m_bp, . ~ . + participant_session))  # +1.6
AIC(m_bw, update(m_bw, . ~ . + word_class))           # +1.1
