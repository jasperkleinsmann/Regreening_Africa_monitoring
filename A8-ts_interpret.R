library(terra)
library(sf)
library(lubridate)
library(data.table)
library(tsibble)
library(zoo)
library(tidyverse)
library(scales)


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
  mutate(regreening = if_else(sgnf_outlier > 0, 1, 0),
         # change inf values in green_date to NA
         green_date = structure(ifelse(is.infinite(green_date), NA, green_date), class='Date'))

# Change 2000-01-01 values into NA
plots$plant_date[plots$plant_date <= date("2000-01-01") | plots$plant_date >= date("2022-12-01")] <- NA

#Merge
plots <- merge(plots, l8_green, by='plotID', all.x=T, all.y=F)
plots$l8_green[is.na(plots$l8_green)] <- 0
fwrite(l8_green, 'output/models/Countries_l8_green.csv')

# Add the residuals per plot to the plots df

plots <- l8_armax_plt %>% 
l8_armax_plt %>% 
  residuals() %>% 
  tibble() %>% 
  group_by(plotID) %>% 
  summarise(rmse = sqrt(sum(mean(.resid^2)))) %>% 
  right_join(y=plots, by='plotID')

plots <- l8_ref_plt %>% 
  tibble() %>% 
  group_by(plotID) %>% 
  summarise(sd = sd(ndvi_int, na.rm=T),
            mean = mean(ndvi_int, na.rm=T)) %>% 
  right_join(y=plots, by='plotID')

#### Save the new plots dataset including greening information
st_write(plots, dsn='output/plot_data/all_countries/Countries_plots_green.GeoJSON', driver='GeoJSON')
plots <- st_read(dsn='output/plot_data/all_countries/Countries_plots_green.GeoJSON')


# Get county level regreening 
plots_dt <- tibble(plots)

cnt_green <- plots_dt %>% 
  group_by(county) %>% 
  summarise(country = first(country),
            l8_green = sum(regreening,na.rm=T),
            number_sites = length(regreening),
            perc = sum(regreening,na.rm=T)/length(regreening),
            l8_ha = sum(Hectare),
            l8_ha_green = sum(Hectare[regreening==1],na.rm=T),
            l8_ha_perc = sum(Hectare[regreening==1],na.rm=T)/sum(Hectare)) %>% 
  arrange(country)

type_green <- plots_dt %>% 
  group_by(type) %>% 
  summarise(l8_green = sum(regreening,na.rm=T),
            number_sites = length(regreening),
            perc = sum(regreening,na.rm=T)/length(regreening),
            l8_ha = sum(Hectare),
            l8_ha_green = sum(Hectare[regreening==1],na.rm=T),
            l8_ha_perc = sum(Hectare[regreening==1],na.rm=T)/sum(Hectare))
type_green

