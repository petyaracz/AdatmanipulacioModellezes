library(tidyverse)
library(ggthemes)

# https://d-place.org/parameters
# Ethnographic Atlas: population size
# ecoClimate Database: annual mean temperature

# plot: log10 population size x annual mean temperature, society name as label, loess smooth

# read

ea_dat = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/data.csv')
ea_var = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/variables.csv')
ea_code = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/codes.csv')
ea_soc = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/societies.csv')

climate_code = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/data.csv')
climate_var = read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/variables.csv')

