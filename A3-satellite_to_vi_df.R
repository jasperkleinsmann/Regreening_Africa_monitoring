library(data.table)
library(tidyverse)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(scales)
library(lubridate)
library(BBmisc)

countries <- list('Ethiopia','Ghana','Kenya','Mali','Niger','Rwanda','Senegal')


##### Import satellite data and merge into single df
refl_l8 <- fread(paste0('output/gee/', countries[[1]], '_l8_ts.csv'))  
refl_l8 <- refl_l8[,c('plotID','yearmon', 'date', 'ndvi', 'prcp_month','county')]
refl_l8$country <- countries[[1]]

for (i in 2:length(countries)){
  print(i)
  cntr_refl <- fread(paste0('output/gee/', countries[[i]], '_l8_ts.csv'))  
  cntr_refl <- cntr_refl[,c('plotID','yearmon', 'date', 'ndvi', 'prcp_month','county')]
  cntr_refl$country <- countries[[i]]
  refl_l8 <- rbind(l8_ts, cntr_refl)
}
# Create new column that is truly plot unique
l8_ts$plot_ID <- l8_ts$plotID
for (i in 1:length(countries)){
  l8_ts$plotID[l8_ts$country==countries[[i]]] <- paste0(substr(l8_ts$country[l8_ts$country==countries[[i]]][1], 1,1), 
                                                        l8_ts$plot_ID[l8_ts$country==countries[[i]]])
}


# Import the reflectance df
refl_l8 <- fread(paste0('output/gee/', country, '_refl_l8_pol.csv'))

#### L8
refl_l8 <- refl_l8[,c('date', 'SR_B4', 'SR_B5', 'SR_B6','QA_PIXEL','plotID')]
# CLEAN AND PROCESS LANDSAT 8 DATA
# Remove rows with NA in red or NIR
refl_l8 <- refl_l8[!is.na(refl_l8$SR_B4) | !is.na(refl_l8$SR_B5) | !is.na(refl_l8$SR_B6),]

# Remove time from datetime
refl_l8$date <- as.Date(refl_l8$date, tryFormats = "%d/%m/%Y")
setorder(refl_l8, cols='date')
refl_l8$yearmon <- as.Date(ISOdate(year(refl_l8$date), month(refl_l8$date), 15))

# Scale DN values to reflectance values
scale_fac <- 0.0000275
offset <- -0.2

refl_l8[,c('SR_B4', 'SR_B5', 'SR_B6')] <- 
  (refl_l8[,c('SR_B4', 'SR_B5', 'SR_B6')] * scale_fac) + offset
# Remove negative reflectance
refl_l8 <- refl_l8[refl_l8$SR_B4 >= 0,]
refl_l8 <- refl_l8[refl_l8$SR_B5 >= 0,]
refl_l8 <- refl_l8[refl_l8$SR_B6 >= 0,]

### Remove clouds
refl_l8$clouds <- 0
# Create function to untangle the bits and return 0 when no cloud
cloud_landsat <- function(x){
  bits <- as.numeric(intToBits(x))[1:16]
  if(bits[7] == 1 && bits[5] == 0 && bits[3] == 0){
    return(0)
  }
  else(return(1))
}
# Apply function to all pixels 
refl_l8$clouds <- lapply(refl_l8$QA_PIXEL, cloud_landsat)
# Filter out pixels with clouds
refl_l8 <- refl_l8[refl_l8$clouds==0,]

# Compute some VIs
refl_l8$ndvi <- (refl_l8$SR_B5 - refl_l8$SR_B4) / (refl_l8$SR_B5 + refl_l8$SR_B4)
refl_l8$osavi <- ((1+0.16)*(refl_l8$SR_B5 - refl_l8$SR_B4)) / (refl_l8$SR_B5 + refl_l8$SR_B4 + 0.16)
refl_l8$ndmi <- (refl_l8$SR_B5 - refl_l8$SR_B6) / (refl_l8$SR_B5 + refl_l8$SR_B5)

# Aggregate values per plot
refl_aggr_l8 <- refl_l8[,.(ndvi=mean(ndvi,na.rm=T), osavi=mean(osavi,na.rm=T), ndmi=mean(ndmi,na.rm=T)), 
                        by=list(plotID, date)]
refl_aggr_l8$yearmon <- as.Date(ISOdate(year(refl_aggr_l8$date), month(refl_aggr_l8$date), 15))

 # Write VI dt
fwrite(refl_aggr_l8, paste0('output/time_series/', country, '_aggr_vi_l8.csv'))

# Remove not needed variables
rm(scale_fac, 
   dupl_l8, dupl_l9, offset, cloud_landsat)
