# CI
# R2, RMSE, AIC, BIC
# ppc, homogeneity/homoscedasticity, linearity, outliers
# collinearity...

# -- head -- #

install.packages('broom')
install.packages('performance')
install.packages('see')
install.packages('qqplotr')

library(tidyverse)
library(broom) # !
library(performance) # !

# -- read -- #

d0 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d0.tsv')
d1 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d1.tsv')
d2 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d2.tsv')
d3 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d3.tsv')
d4 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d4.tsv')
d5 = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/l3d5.tsv')

# -- fun -- #

# take data, columns x, y, plot x against y, draw lm
plotData = function(dat,x,y){
  dat |> 
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

# your turn!

# d1
plotData(d1,height_father,height_son)

lm1 = lm(height_son ~ height_father, data = d1)
summary(lm1)

plots1 = plot(check_model(lm1))
plots1[[1]]
plots1[[2]]
plots1[[3]]
plots1[[4]]
plots1[[5]]

# d2
plotData(d2,iq,income)

lm2 = lm(income ~ iq, data = d2)
summary(lm2)

plots1 = plot(check_model(lm2))
plots1[[1]]
plots1[[2]]
plots1[[3]]
plots1[[4]]
plots1[[5]]

plotData(d3, weight, size)

lm3 = lm(size ~ weight, data = d3)
summary(lm3)

plots1 = plot(check_model(lm3))
plots1[[1]]
plots1[[2]]
plots1[[3]]
plots1[[4]]
plots1[[5]]
