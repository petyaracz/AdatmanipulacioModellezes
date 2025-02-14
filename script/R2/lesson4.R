# -- head -- #

set.seed(1337)

library(tidyverse)
library(broom)
library(performance)

# -- main -- #

# lm formula: y = a + bx + e

# BME/TTK/GTK
# allami/ossz jel.
# 946/1329
# 17/29
# 85/128

# %, p, oddsz, log oddsz

ps = seq(0,.99,.01)
odds = ps / (1 - ps)
log_odds = qlogis(ps)

ps
odds
log_odds

plot(ps,odds)
plot(ps,log_odds)
plot(log_odds,odds)

# school 1 

GTK = rbinom(128, 1, 85/128)
TTK = rbinom(29, 1, 17/29)

GTK = GTK %>% 
  tibble(school = 'GTK', admitted = .)
TTK = TTK %>% 
  tibble(school = 'TTK', admitted = .)
d = bind_rows(GTK,TTK)

d %>% 
  count(school,admitted)

d %>% 
  count(school,admitted) %>% 
  pivot_wider(names_from = admitted, values_from = n)

fit1 = glm(admitted ~ 1 + school, data = d, family = binomial(link = 'logit'))
sum1 = tidy(fit1)
sum1

check_model(fit1) 

fit1b = lm(admitted ~ 1 + school, data = d)
tidy(fit1b)
check_model(fit1b) 

a1 = sum1 %>% 
  filter(term == '(Intercept)') %>% 
  pull(estimate)
b1 = sum1 %>% 
  filter(term == 'schoolTTK') %>% 
  pull(estimate)

plogis(a1)
exp(a1)
85/43
log(85/43)
a1+b1
plogis(a1+b1)
log(17/12)

log(exp(a1)*exp(b1))

# logit glm formula: logit(p) = a + bx + e

# school 2

GTK = rbinom(120, 1, .5)
TTK = rbinom(30, 1, .75)

GTK = GTK %>% 
  tibble(school = 'GTK', graduates = .)
TTK = TTK %>% 
  tibble(school = 'TTK', graduates = .)
d2 = bind_rows(GTK,TTK)

d2 %>% 
  count(school,graduates)

d2 %>% 
  count(school,graduates) %>% 
  pivot_wider(names_from = graduates, values_from = n)

fit2 = glm(graduates ~ 1 + school, data = d2, family = binomial(link = 'logit'))
tidy(fit2)

a2 = sum2 %>% 
  filter(term == '(Intercept)') %>% 
  pull(estimate)
b2 = sum2 %>% 
  filter(term == 'schoolTTK') %>% 
  pull(estimate)

plogis(a2)
exp(a2)
plogis(a2+b2)

# not school

d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l4d1.tsv')

ggplot(d3, aes(lfpm10r,as.double(correct))) +
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family="binomial"))

fit3 = glm(correct ~ lfpm10r, data = d3, family = binomial(link = 'logit'))
tidy(fit3, conf.int = T)

d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l4d2.tsv')

fit4 = glm(cbind(`TRUE`,`FALSE`) ~ lfpm10r, data = d4, family = binomial(link = 'logit'))
tidy(fit4)
