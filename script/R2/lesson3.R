# CI
# R2, RMSE, AIC, BIC
# ppc, homogeneity/homoscedasticity, linearity, outliers
# collinearity...

# -- head -- #

install.packages('broom')
install.packages('performance')
install.packages('see')

library(tidyverse)
library(broom) # !
library(performance) # !

# -- read -- #

d0 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d0.tsv')
d1 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d1.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d2.tsv')
d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d3.tsv')
d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d4.tsv')
d5 = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/l3d5.tsv')

# -- fun -- #

# take data, columns x, y, plot x against y, draw lm
plotData = function(dat,x,y){
  dat %>% 
    ggplot(aes({{x}},{{y}})) + # data masking: {{}} tells the function to look for the names x and y in dat
    geom_point() +
    geom_smooth(method = lm) +
    theme_bw()
}

# -- wrangle -- #

## performance

# always draw data first
plotData(d0,height_father,height_son)

# fit model
lm0 = lm(height_son ~ height_father, data = d0)

# coefficient estimates (from broom)
lm0_coef = tidy(lm0, conf.int = T)
lm0_coef
# model stats (from broom)
lm0_stats = glance(lm0)
lm0_stats
# model performance (from performance, overlaps with model stats)
lm0_perf = model_performance(lm0)
lm0_perf
model_performance(lm0, metrics = 'common')

# check model (from performance)
check_model(lm0)
# return a list of single plots
diagnostic_plots = plot(check_model(lm0, panel = FALSE))
diagnostic_plots[[1]]
diagnostic_plots[[2]]
diagnostic_plots[[3]]
diagnostic_plots[[4]]
diagnostic_plots[[5]]

## ppc

plotData(d1,height_father,height_son) 
  
lm1 = lm(height_son ~ height_father, data = d1)

tidy(lm1, conf.int = T)
glance(lm1)
check_model(lm1)

# let's add more information to the plot
ggplot(d1, aes(height_father,height_son,colour = orig_father)) +
  geom_point() +
  theme_bw()

# let's keep one type of dad
d1ind = d1 %>% 
  filter(orig_father == 'ind')

lm1_ind = lm(height_son ~ height_father, data = d1ind)
check_model(lm1_ind)

## homogeneity

plotData(d2,iq,income) 

lm2 = lm(income ~ iq, data = d2)

tidy(lm2, conf.int = T)
glance(lm2)
check_model(lm2)

## linearity

plotData(d3,size,weight) 

lm3 = lm(weight ~ size, data = d3)

tidy(lm3, conf.int = T)
glance(lm3)
check_model(lm3)

# let's try polynomials
lm3_square = lm(weight ~ poly(size, degree = 2), data = sizes)
lm3_cube = lm(weight ~ poly(size, degree = 3), data = d3)
check_model(lm3_cube)
compare_performance(lm3,lm3_cube)
plot(compare_performance(lm3,lm3_cube))
plot(compare_performance(lm3,lm3_cube), metrics = 'common')

## outliers

plotData(d4,sqm,price)

lm4 = lm(price ~ sqm, data = d4)

tidy(lm4, conf.int = T)
glance(lm4)
check_model(lm4)

# standard error
# mean absolute error
# top/bottom x observation

# create d4l, which is d4 with...
d4l = d4 %>% 
  mutate( # added columns
    median_sqm = median(sqm), # median of sqm
    mad_sqm = mad(sqm), # mean absolute deviation of sqm
    upper_sqm = mad_sqm + 2.5 * median_sqm, # upper bound for observations is median + 2.5 mad
    lower_sqm = mad_sqm - 2.5 * median_sqm, # lower bound is median - 2.5 mad
    median_price = median(price), # same in english
    mad_price = mad(price),
    upper_price = mad_price + 2.5 * median_price,
    lower_price = mad_price - 2.5 * median_price,
    keep = sqm < upper_sqm & price < upper_price
  )

# let's take a look:
d4l

# what did we exclude?
d4l %>% 
  ggplot(aes(sqm,price,colour = keep)) +
  geom_point() +
  theme_bw()

# only keep unruly observations
d4r = d4l %>% 
  filter(keep)

# try again
lm4r = lm(price ~ sqm, data = d4r)

tidy(lm4r, conf.int = T)
glance(lm4r)
check_model(lm4r)

# going back to lm0!
check_model((lm0))
# d0 was generated from two gaussian processes and still looks a tiny bit weird.

# your turn!

# d5
d5
plotData(d5, lfpm10r, resp.rt)
# fit model: resp.rt ~ lfpm10r
lm5 = lm(resp.rt ~ lfpm10r, data = d5)
tidy(lm5)
check_model(lm5)
# get coefs and conf int
# get stats
# check model









lm4 = lm(resp.rt ~ lfpm10r, data = d4)
check_model(lm4)
tidy(lm4)
lm4l = lm(log(resp.rt) ~ lfpm10r, data = d4)
check_model(lm4l)
