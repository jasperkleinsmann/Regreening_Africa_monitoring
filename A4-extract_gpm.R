
library(terra)
library(sf)
library(lubridate)
library(data.table)

# Assign county, regio and country to each plots file
countries <- list('Ethiopia','Ghana','Kenya','Mali','Niger','Rwanda','Senegal')

#### Create dates
start_ts <- as.Date('2013-01-01')
end_ts <- as.Date('2022-11-15')

months_dif <- (year(end_ts) - year(start_ts)) * 12 + (month(end_ts) - month(start_ts))
months_ts <- seq(ymd(start_ts), by = "month", length.out=months_dif)


gpm_ts <- data.table(plotID=numeric(), prcp_month=double(), date=lubridate::Date(), yearmon=lubridate::Date(), country=character())
# Import all data and put in dataframe
for (country in countries){
  print(country)
  # Import 
  gpm <- rast(paste0('output/gee/gpm/', country, '_GPM_stack.tif'))
  plots <- st_read(dsn=paste0('output/plot_data/', country, '/', country, '_plot_centroid.GeoJSON'))
  plots <- vect(plots)

  # Assign dates to gpm_stack layers
  names(gpm) <- months_ts
  
  #### Extract values for each layer
  gpm_plots <- data.table(extract(gpm, plots, cells=F))
  gpm_plots <- gpm_plots[,-c('ID')] # rempve one column
  rownames(gpm_plots) <- plots$plotID
  
  # Transpose dt
  gpm_ts_cntr <- cbind(row = rownames(gpm_plots), stack(gpm_plots))
  
  # Clean dt
  names(gpm_ts_cntr) <- c('plotID', 'prcp_month', 'date')
  gpm_ts_cntr$yearmon <- as.Date(ISOdate(year(gpm_ts_cntr$date), month(gpm_ts_cntr$date), 15))
  
  # Indicate country
  gpm_ts_cntr$country <- country
  # Merge all country data.tables
  gpm_ts <- rbind(gpm_ts, gpm_ts_cntr)
}

gpm_ts$plot_ID <- gpm_ts$plotID
for (i in 1:length(countries)){
  gpm_ts$plotID[gpm_ts$country==countries[[i]]] <- paste0(substr(gpm_ts$country[gpm_ts$country==countries[[i]]][1], 1,1), 
                                                          gpm_ts$plot_ID[gpm_ts$country==countries[[i]]])
}

#### Write as csv
fwrite(gpm_ts, paste0('output/time_series/gpm/Countries_gpm_monthly.csv'))





