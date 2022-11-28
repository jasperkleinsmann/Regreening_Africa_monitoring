
library(data.table)
library(sf)
library(tidyverse)

#### Import
# Plot data
gpm_month <- fread('output/time_series/gpm/Countries_gpm_monthly.csv')
refl_l8 <- fread('output/time_series/Countries_aggr_l8.csv')
plots <- st_read(dsn='output/plot_data/all_countries/Countries_plots_final.GeoJSON')
plots <- data.table(plots)
names(plots)[names(plots) == 'Plot_id'] <- 'plot_ID'

# clean and merge refl datasets
l8_ts <- merge(refl_l8, gpm_month[,c('plotID', 'yearmon','prcp_month','plot_ID','country')],
               by=c('plotID', 'yearmon', 'plot_ID','country'), all.y=T)

l8_ts <- merge(l8_ts, plots[,c('plotID', 'county','region')], by=c('plotID'), all.y=T)

# Write cleaned L8 and S1 ts
fwrite(l8_ts, 'output/time_series/Countries_l8_ts.csv')

