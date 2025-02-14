library(tidyverse)
library(broom.mixed)
library(performance)
library(sjPlot)
library(lme4)

d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l10d1.tsv')

# structure of d
d
length(unique(d$word))
length(unique(d$subject))
# effects of analogy and rules on regularity
d %>% 
  ggplot(aes(analogy,regular)) +
  geom_jitter(width = 0, height = .1) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) +
  scale_y_continuous(breaks = c(0,1))

d %>% 
  ggplot(aes(rules,regular)) +
  geom_jitter(width = 0, height = .1) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) +
  scale_y_continuous(breaks = c(0,1))

# per subject?
d %>% 
  ggplot(aes(analogy,regular)) +
  geom_jitter(width = 0, height = .1) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'), se = FALSE) +
  scale_y_continuous(breaks = c(0,1)) +
  facet_wrap(~subject)

# "ordinary" GLM?
glm1 = glm(regular ~ analogy, data = d, family = 'binomial')
tidy(glm1)

# "ordinary" GLM with participant factor?
glm2 = glm(regular ~ analogy + subject, data = d, family = 'binomial')
tidy(glm2)

# "ordinary" GLM with participant interaction?
glm3 = glm(regular ~ analogy * subject, data = d, family = 'binomial')
View(tidy(glm3))

# hierarchical GLM with participant grouping factor (random intercept)?
glm4 = glmer(regular ~ analogy + (1|subject), data = d, family = 'binomial')
tidy(glm4)

# hierarchical GLM with participant grouping factor (random intercept) and random slope?
glm5 = glmer(regular ~ analogy + (1 + analogy|subject), data = d, family = 'binomial')
tidy(glm5)
plot(compare_performance(glm4, glm5))

# same with rules as predictor instead of analogy
glm6 = glmer(regular ~ rules + (1 + rules |subject), data = d, family = 'binomial')
tidy(glm6)

plot(compare_performance(glm5, glm6))

# singular fit!
# no convergence!
# gods?
