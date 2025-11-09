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

d = read_tsv('https://raw.githubusercontent.com/petyaracz/AdatmanipulacioModellezes/main/dat/R2/szeged.tsv')

d |> 
  ggplot(aes(ymd,apparent_temperature)) +
  geom_point()

# -- main -- #

# we look at the data
glimpse(d)
# daily averages for daylight hours between:
range(d$ymd)

# correlations
d |> 
  select(apparent_temperature,temperature) |> 
  pairs.panels( 
             method = "pearson", # correlation method
             density = TRUE,  # show density plots
             lm = T
  )
# + humidity
d |> 
  select(apparent_temperature,temperature,humidity) |> 
  pairs.panels( 
             method = "pearson", # correlation method
             density = TRUE,  # show density plots
             lm = T
)
# + wind speed
d |> 
  select(apparent_temperature,temperature,humidity,wind_speed) |> 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )
# + pressure
d |> 
  select(apparent_temperature,temperature,humidity,wind_speed,pressure) |> 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )
# + date
d |> 
  select(apparent_temperature,temperature,humidity,wind_speed,pressure,ymd) |> 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )

# linear models

# test for:

# apparent_temperature ~ pred1 + pred2
lm1 = lm(apparent_temperature ~ b1 + b2, data = d)
lm2 = lm(apparent_temperature ~ b1 * b2, data = d)

# checklist:
# 1. coefficient estimates (tidy, plot_model('est'))
tidy(lm1, conf.int = T)
tidy(lm2, conf.int = T)
plot_model(lm1, 'est')
plot_model(lm2, 'est')
# 2. visualise estimates
plot_model(lm2, 'pred', terms = c('b1','b2'))
plot_model(lm2, 'pred', terms = c('b1','b2 [0, 15, 50]')) # 'term [1,2,3]'
# 3. compare performance (plot)
plot(compare_performance(lm1,lm2))
# 4. test performance
test_wald(lm1,lm2)
# 5. check model
check_model(lm2)
check_collinearity(lm2)
