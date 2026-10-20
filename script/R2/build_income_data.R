# -- head -- #

library(tidyverse) # data wrangling and plotting

# -- main -- #

# story: can we predict someone's income from four variables related to
# social status? all four predictors are additive (no interactions) so
# students can practice fitting and reading an additive linear model
# before interactions are introduced

set.seed(1337) # fixed seed so the dataset is reproducible

n = 300 # number of simulated people

# years of formal education: human capital, a classic social status proxy
education = rnorm(n, mean = 14, sd = 3) # centred on a university-ish average
education = round(education) # education is measured in whole years
education = pmin(pmax(education, 8), 20) # clamp to a plausible 8-20 year range

# age in years: stands in for work experience, which tends to raise income
age = runif(n, min = 22, max = 65) # uniform across a typical working-age range
age = round(age) # age is measured in whole years

# parent's years of education: family background / inherited social status
parent_education = rnorm(n, mean = 12, sd = 3) # centred one generation below
parent_education = round(parent_education) # also measured in whole years
parent_education = pmin(pmax(parent_education, 6), 20) # same plausible range

# region: access to opportunity, a geographic marker of social status
region = sample(c('village', 'town', 'capital'), n, replace = T, prob = c(.4, .35, .25)) # village is most common, capital least
region = factor(region, levels = c('village', 'town', 'capital')) # village set as the reference level

# true effects used to build the outcome (ground truth for the instructor;
# students only ever see the simulated income column, not these numbers)
b_intercept = 100 # baseline income with zero education, age zero (extrapolated), village, uneducated parent
b_education = 15 # extra income per extra year of own education
b_age = 2 # extra income per extra year of age (experience)
b_parent_education = 8 # extra income per extra year of parent's education
b_town = 20 # extra income for living in a town rather than a village
b_capital = 50 # extra income for living in the capital rather than a village

# region's effect translated from factor levels to a numeric contribution
region_effect = case_when(
  region == 'village' ~ 0, # reference level contributes nothing
  region == 'town' ~ b_town,
  region == 'capital' ~ b_capital
)

# income is the additive sum of all four effects plus unexplained noise
noise = rnorm(n, mean = 0, sd = 60) # unexplained variation (unmeasured factors, luck, etc.)
income = b_intercept + b_education * education + b_age * age + b_parent_education * parent_education + region_effect + noise
income = round(income) # income is reported in whole units (thousand HUF/month)

# assemble the final dataset: only the outcome and the four predictors,
# exactly what a student fitting an additive model would see
d = tibble(income, education, age, parent_education, region)

# quick sanity check: does an additive lm recover something close to the
# true effects above? useful for the instructor, not meant for students
lm1 = lm(income ~ education + age + parent_education + region, data = d)
broom::tidy(lm1, conf.int = T)

# quick visual sanity check of two of the four predictors against income
d |>
  ggplot(aes(education, income, colour = region)) +
  geom_point(alpha = .5) +
  geom_smooth(method = 'lm', se = F) +
  theme_bw()

# write the dataset out for the student-facing lesson script to read in
write_tsv(d, 'dat/R2/income_social_status.tsv')
