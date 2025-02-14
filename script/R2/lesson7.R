# -- head -- #

install.packages('psych')

library(tidyverse)
library(broom)
library(performance)
library(sjPlot)
library(psych)

# -- read -- #

d = read_tsv('https://raw.githubusercontent.com/petyaracz/class_advanced_r/main/dat/szeged.tsv')

d |> 
  ggplot(aes(ymd,apparent_temperature)) +
  geom_point()

# -- main -- #

# we look at the data
glimpse(d)
# daily averages for daylight hours between:
range(d$ymd)

# correlations
d %>% 
  select(apparent_temperature,temperature) %>% 
  pairs.panels( 
             method = "pearson", # correlation method
             density = TRUE,  # show density plots
             lm = T
  )
# + humidity
d %>% 
  select(apparent_temperature,temperature,humidity) %>% 
  pairs.panels( 
             method = "pearson", # correlation method
             density = TRUE,  # show density plots
             lm = T
)
# + wind speed
d %>% 
  select(apparent_temperature,temperature,humidity,wind_speed) %>% 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )
# + pressure
d %>% 
  select(apparent_temperature,temperature,humidity,wind_speed,pressure) %>% 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )
# + date
d %>% 
  select(apparent_temperature,temperature,humidity,wind_speed,pressure,ymd) %>% 
  pairs.panels( 
    method = "pearson", # correlation method
    density = TRUE,  # show density plots
    lm = T
  )

# linear models

# test for:

# apparent_temperature ~ temperature + wind_speed / temperature * wind_speed
lm1 = lm(apparent_temperature ~ temperature + wind_speed, data = d)
lm2 = lm(apparent_temperature ~ temperature * wind_speed, data = d)
# apparent_temperature ~ humidity + pressure / humidity * pressure
# apparent_temperature ~ temperature + humidity / temperature * humidity
# apparent_temperature ~ temperature + residual humidity
# apparent_temperature ~ temperature + residual wind_speed

lm1 = lm(apparent_temperature ~ humidity + pressure, data = d)
lm2 = lm(apparent_temperature ~ humidity * pressure, data = d)

# checklist:
# 1. coefficient estimates (tidy, plot_model('est'))
tidy(lm1, conf.int = T)
tidy(lm2, conf.int = T)
plot_model(model, 'est')
# 2. visualise estimates
plot_model(lm2, 'pred', terms = c('humidity','pressure')) # 'term [1,2,3]'
# 3. compare performance (plot)
plot(compare_performance(lm1,lm2))
# 4. test performance
test_wald(lm1,lm2)
# 5. check model
check_model(lm2)
check_collinearity(lm2)
