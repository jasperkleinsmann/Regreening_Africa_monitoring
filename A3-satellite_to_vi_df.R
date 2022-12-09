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

# Function for cloud filtering
cloud_landsat <- function(x){
  bits <- as.numeric(intToBits(x))[1:16]
  if(bits[7] == 1 && bits[5] == 0 && bits[3] == 0){
    return(0)
  }
  else(return(1))
}

##### Import satellite data and merge into single df
# Create df
refl_aggr_l8 <- data.table(date=lubridate::Date(),plotID=integer(), ndvi=double(), yearmon=lubridate::Date(),country=character())

# Do the country specific analysis and loop trhough all countries
for (i in countries){
  # Import data
  refl_l8 <- fread(paste0('output/gee/', i, '_refl_l8_pol.csv'), select =c('date', 'SR_B4', 'SR_B5','QA_PIXEL','plotID'))  
  
  # Remove rows with NA 
  refl_l8 <- refl_l8[!is.na(refl_l8$SR_B4) | !is.na(refl_l8$SR_B5),]
  
  # Remove time from datetime
  refl_l8$date <- as.Date(refl_l8$date, tryFormats = "%d/%m/%Y")
  
  # Scale DN values to reflectance values
  scale_fac <- 0.0000275
  offset <- -0.2
  # Transform DNs
  refl_l8[,c('SR_B4', 'SR_B5')] <- 
    (refl_l8[,c('SR_B4', 'SR_B5')] * scale_fac) + offset
  # Remove negative reflectance
  refl_l8 <- refl_l8[refl_l8$SR_B4 >= 0,]
  refl_l8 <- refl_l8[refl_l8$SR_B5 >= 0,]
  
  # Remove clouds
  refl_l8$clouds <- 0
  # Apply function to all pixels 
  refl_l8$clouds <- lapply(refl_l8$QA_PIXEL, cloud_landsat)
  # Filter out pixels with clouds

  refl_l8 <- refl_l8[refl_l8$clouds==0,]

  # Compute some VIs
  refl_l8$ndvi <- (refl_l8$SR_B5 - refl_l8$SR_B4) / (refl_l8$SR_B5 + refl_l8$SR_B4)
  
  # Aggregate values per plot
  refl_aggr_l8_cntr <- refl_l8[,.(ndvi=mean(ndvi,na.rm=T)), by=list(plotID, date)]
  # Add yearmon
  refl_aggr_l8_cntr$yearmon <- as.Date(ISOdate(year(refl_aggr_l8_cntr$date), month(refl_aggr_l8_cntr$date), 15))
  # Add country
  refl_aggr_l8_cntr$country <- i 
  
  # Rbind
  refl_aggr_l8 <- rbind(refl_aggr_l8, refl_aggr_l8_cntr)

}

# Create new column that is truly plot unique
refl_aggr_l8$plot_ID <- refl_aggr_l8$plotID
for (i in 1:length(countries)){
  refl_aggr_l8$plotID[refl_aggr_l8$country==countries[[i]]] <- paste0(substr(refl_aggr_l8$country[refl_aggr_l8$country==countries[[i]]][1], 1,1), 
                                                                      refl_aggr_l8$plot_ID[refl_aggr_l8$country==countries[[i]]])
}

# Write VI dt
fwrite(refl_aggr_l8, 'output/time_series/Countries_aggr_l8.csv')
refl_aggr_l8 <- fread('output/time_series/Countries_aggr_l8.csv')

# Remove not needed variables
rm(scale_fac, 
   dupl_l8, dupl_l9, offset, cloud_landsat)
