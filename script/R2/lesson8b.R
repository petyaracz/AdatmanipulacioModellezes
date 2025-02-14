# the raw data:

d %>% 
  ggplot(aes(Age, Survived, colour = Sex)) +
  geom_jitter(height = .05, width = 0) +
  geom_smooth(method = 'glm', method.args = list(family = binomial), se = T) +
  theme_bw() +
  facet_wrap( ~ Class, nrow = 1)

# you can do a logistic glm on count ratios

d2 = d %>% 
  count(Survived,Class,Sex) %>% 
  pivot_wider(names_from = Survived, values_from = n, values_fill = 0) %>% 
  rename(Dead = `0`, Survived = `1`) %>% 
  mutate(prop = Survived / (Survived + Dead))

# compare:
fit5 = glm(Survived ~ Class + Sex, data = d, family = binomial)
fit6 = glm(cbind(Survived,Dead) ~ Class + Sex, data = d2, family = binomial)
tidy(fit5)
tidy(fit6)
