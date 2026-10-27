# -- head -- #

install.packages('psych')

library(tidyverse)
library(broom)
library(performance)
library(sjPlot)
library(psych)

# -- quick test -- #

# https://forms.gle/Q1GaLixtpNwVsomd9

# -- read -- #

d = read_csv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/sl_sim.csv')


# sl_interactions.csv: simulated data, 255 children aged 7-14
# One row per participant. Loosely based on Lukics et al. (2026, Cognitive Science).

# participant       ID, P001-P255

# --- outcome ---
# sl_accuracy       statistical learning: proportion correct on an 80-trial
#                   two-alternative forced choice test after exposure to an
#                   artificial grammar; chance = 0.5 (observed 0.35-0.95)

# --- group variables ---
# diagnosis         TD   = typical development (n = 90)
#                   DLD  = developmental language disorder (n = 60)
#                   ASD  = autism spectrum disorder (n = 55)
#                   ADHD = attention deficit hyperactivity disorder (n = 50)
# sex               boy / girl (175 / 80); boys over-represented in the
#                   clinical groups, as in real samples

# --- background ---
# age               age in years, one decimal (7.0-14.0)
# nonverbal_iq      nonverbal IQ, standard score (population mean 100, SD 15);
#                   groups matched on it by design (observed 77-132)

# --- memory and processing tasks ---
# digit_span        verbal working memory: digits recalled, forward + backward
#                   total score (observed 4-17)
# nonword_rep       phonological short-term memory: nonwords repeated
#                   correctly, out of 40 (observed 14-39)
# corsi_span        visuospatial working memory: longest block sequence
#                   tapped back correctly (observed 3-9)
# perceptual_speed  auditory simple reaction time in ms; HIGHER = SLOWER
#                   (observed 217-536)