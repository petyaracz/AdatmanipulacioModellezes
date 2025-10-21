# -- head -- #

set.seed(1337)

library(tidyverse)
library(broom)
library(performance)

# -- read -- #

d1 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l4d1.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l4d2.tsv')

# -- p -- #

## glm

# lm formula: y = a + bx + e
# y can be p

## proportions and probabilities

# Create a 2x2 contingency table
faculty_data = matrix(c(20, 80, 40, 60), 
                       nrow = 2, 
                       byrow = TRUE,
                       dimnames = list(Faculty = c("Engineering", "Arts"),
                                       Gender = c("Women", "Men")))

print(faculty_data)
chisq.test(faculty_data)

# Smaller sample: same proportions
small_data = matrix(c(2, 8, 4, 6), 
                     nrow = 2, 
                     byrow = TRUE,
                     dimnames = list(Faculty = c("Engineering", "Arts"),
                                     Gender = c("Women", "Men")))

print(small_data)
chisq.test(small_data)

# This problem in a GLM

# Reconstruct the data in long format
engineering = data.frame(faculty = "Engineering", 
                          female = c(rep(1, 20), rep(0, 80)))
arts = data.frame(faculty = "Arts", 
                   female = c(rep(1, 40), rep(0, 60)))

students = rbind(engineering, arts)
students$faculty = factor(students$faculty)

# Fit a logistic regression
model = glm(female ~ faculty, data = students, family = binomial)

summary(model)

tidy(model, conf.int = T)

plogis(-.8)
plogis(-.4-1.6)
plogis(-.4-.35)

bad_model = lm(female ~ faculty, data = students)

summary(bad_model)

plots = plot(check_model(bad_model))
plots[[5]]


# Log odds for Arts (reference category)
log_odds_arts = -0.405

# Log odds for Engineering (reference + change)
log_odds_eng = -0.405 + (-0.981)

# Arts
p_arts = plogis(log_odds_arts)
p_arts  # 0.400 (40%)

# Engineering  
p_eng = plogis(log_odds_eng)
p_eng  # 0.200 (20%)

qlogis(p_arts)
qlogis(p_eng)

1 / (1 + exp(-log_odds_arts))
log(p_arts / (1 - p_arts))

## %, p, oddsz, log oddsz

ps = seq(0,.99,.01)
odds = ps / (1 - ps)
log_odds = qlogis(ps)

ps
odds
log_odds

plot(ps,odds)
plot(ps,log_odds)
plot(log_odds,odds)

# -- d2 -- #

ggplot(d2, aes(lfpm10r,as.double(knows))) +
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family="binomial"))

fit1 = glm(knows ~ lfpm10r, data = d2, family = binomial(link = 'logit'))

tidy(fit1, conf.int = T)

plogis(2.28+.82)

# -- d1 -- #

fit2 = glm(cbind(n_known,n_unknown) ~ lfpm10r, data = d1, family = binomial)

summary(fit2)
tidy(fit2, conf.int = T)

check_model(fit2)
