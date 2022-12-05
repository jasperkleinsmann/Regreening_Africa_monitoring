library(terra)
library(sf)
library(lubridate)
library(data.table)
library(tsibble)
library(zoo)
library(tidyverse)
library(scales)

country <- 'Kenya'

load('output/models/Countries_l8_fc_plt.RDS')
l8_ts_plt <- read_csv('output/time_series/Countries_l8_plt.csv')

plots <- st_read('output/plot_data/all_countries/Countries_plots_final.GeoJSON')

###### L8
# Clean dates
l8_fc_plt_dt <- l8_fc_plt[,c('.mean', 'yearmonth', 'plotID', 'upper_95', 'lower_95')]
l8_fc_plt_dt$yearmon <- as.Date(ISOdate(year(l8_fc_plt_dt$yearmonth), month(l8_fc_plt_dt$yearmonth), 15))

# Merge actual ndvi with the forecast
l8_fc_plt_dt <- merge(l8_fc_plt_dt, l8_ts_plt[,c('ndvi_int', 'plotID', 'yearmon')], 
                   by=c('yearmon', 'plotID'), all.x=T)
l8_fc_plt_dt <- tibble(l8_fc_plt_dt)

l8_green <- l8_fc_plt_dt %>% 
  arrange(plotID, by_group=T) %>% 
  mutate(above_95=ndvi_int>upper_95,
         above95_lead = lead(above_95),
         sgnf_outlier = if_else(above_95==T & above95_lead==T, 1,0),
         peak_date = if_else(sgnf_outlier == 1, yearmon, ymd(NA))) %>% 
  group_by(plotID) %>% 
  summarise(sgnf_outlier = sum(sgnf_outlier),
            green_date = min(peak_date, na.rm=T)) %>% 
  mutate(regreening = if_else(sgnf_outlier > 0, 1, 0))

#Merge
plots <- merge(plots, l8_green, by='plotID', all.x=T, all.y=F)
plots$l8_green[is.na(plots$l8_green)] <- 0
fwrite(l8_green, 'output/models/Countries_l8_green.csv')

# Add the residuals per plot to the plots df
l8_armax_plt %>% 
  head(100) %>% 
  residuals() %>% 
  select(.resid)

#### Save the new plots dataset including greening information
st_write(plots, dsn='output/plot_data/Countries_plots_green.GeoJSON', driver='GeoJSON')

plots <- st_read(dsn='output/plot_data/Countries_plots_green.GeoJSON')


# Get county level regreening 
plots_dt <- data.table(plots)

cnt_green <- plots_dt[,.(l8_green=sum(regreening,na.rm=T),
                         number_sites=length(regreening),
                         perc=sum(regreening,na.rm=T)/length(regreening),
                         l8_ha=sum(Hectare),
                         l8_ha_green=sum(Hectare[regreening==1],na.rm=T),
                         l8_ha_perc=sum(Hectare[regreening==1],na.rm=T)/sum(Hectare)),
                      by=list(county)]
cnt_green





type_green <- plots_dt[,.(l8_green=sum(regreening,na.rm=T),
                          number_sites=length(regreening),
                          perc=sum(regreening,na.rm=T)/length(regreening),
                          l8_ha=sum(Hectare),
                          l8_ha_green=sum(Hectare[regreening==1],na.rm=T),
                          l8_ha_perc=sum(Hectare[regreening==1],na.rm=T)/sum(Hectare)),
                      by=list(type)]
type_green

# Exclude plots where no model was fit
cnt_green_fit <- plots_dt[,.(l8_green=sum(l8_green,na.rm=T),
                         number_sites=length(l8_green[fit==1]),
                         perc=sum(l8_green,na.rm=T)/length(l8_green[fit==1]),
                         l8_ha=sum(Hectare[fit==1], na.rm=T),
                         l8_ha_green=sum(Hectare[l8_green==1],na.rm=T),
                         l8_ha_perc=sum(Hectare[fit==1], na.rm=T)/sum(Hectare)),
                      by=list(county)]
cnt_green_fit
