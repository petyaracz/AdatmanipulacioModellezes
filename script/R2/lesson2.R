# plot
# predict
# CI / error / t / p

# -- head -- #

library(tidyverse)
library(broom)

# -- functions -- #

plotLM = function(dat,x,y){
  dat %>% 
    ggplot(aes({{x}},{{y}})) +
    geom_point() +
    geom_smooth(method = 'lm') +
    theme_bw()
}

# -- read -- #

d1 = read_tsv('dat/l2d1.tsv')
d2 = read_tsv('dat/l2d2.tsv')
d3 = read_tsv('dat/l2d3.tsv')
d4 = read_tsv('dat/l2d4.tsv')
d5 = read_tsv('dat/l2d5.tsv')

# -- main -- #

# d1: heights, not centered
plotLM(d1,height_father,height_son)
with(d1, cor(height_father,height_son))
lm1 = lm(height_son ~ height_father, data = d1)
lm1
summary(lm1)
tidy(lm1, conf.int = T)
# intercept: a nulla centis apa gyermeke

# d2: heights, f centered
plotLM(d2,height_father_c,height_son)
with(d2, cor(height_father_c,height_son))
lm2 = lm(height_son ~ height_father_c, data = d2)
lm2
tidy(lm2, conf.int = T)

# d3: heights, f scaled
plotLM(d3,height_father_r,height_son_r)
with(d3, cor(height_father_r,height_son_r))
lm3 = lm(height_son_r ~ height_father_r, data = d3)
lm3
tidy(lm3, conf.int = T)

# d4: limbo!
plotLM(d4,height_father,limbo_son)
with(d4, cor(height_father,limbo_son))
lm4 = lm(limbo_son ~ height_father, data = d4)
summary(lm4)
tidy(lm4, conf.int = T)

# d5: outliers
plotLM(d5,height_father,height_son)
with(d5, cor(height_father,height_son))
lm5 = lm(height_son ~ height_father, data = d5)
tidy(lm5, conf.int = T)

# and now we predict
str(d1)
predict(lm1)
predict(lm1, tibble(height_father = 195))


predict(lm2)
predict(lm2, tibble(height_father_c = 195))

# sampling
# confint!
# Confidence intervals tell you about how well you have determined the mean. Assume that the data really are randomly sampled from a Gaussian distribution. If you do this many times, and calculate a confidence interval of the mean from each sample, you'd expect about 95 % of those intervals to include the true value of the population mean. The key point is that the confidence interval tells you about the likely location of the true population parameter.
# 
# Prediction intervals tell you where you can expect to see the next data point sampled. Assume that the data really are randomly sampled from a Gaussian distribution. Collect a sample of data and calculate a prediction interval. Then sample one more value from the population. If you do this many times, you'd expect that next value to lie within that prediction interval in 95% of the samples.The key point is that the prediction interval tells you about the distribution of values, not the uncertainty in determining the population mean. 
# r2
# model sum of squares, or SSM /  total sum of squares, or SST
summary(lm1)
confint(lm1)
predict(lm1, tibble(height_father = 195), interval='confidence')
predict(lm2, tibble(height_father_c = 195), interval='confidence')
