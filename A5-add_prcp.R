
#### Import
country <- 'Senegal'

# Plot data
gpm_month <- data.table(read_csv(paste0('output/time_series/gpm/', country, '_gpm_monthly.csv')))
refl_l8 <- data.table(read_csv(paste0('output/time_series/', country, '_aggr_vi_l8.csv')))

####### Add information gpm
gpm_month <- merge(gpm_month, plots[,c('plotID', 'county')], 
                   by='plotID')
gpm_month <- gpm_month[,-c('geometry')]

# clean and merge refl datasets
l8_ts <- merge(refl_l8, gpm_month[,c('plotID', 'yearmon','prcp_month')],
               by=c('plotID', 'yearmon'), all.y=T)
l8_ts <- l8_ts[,-c('geometry')]
l8_ts <- l8_ts[order(l8_ts$yearmon),]

# Write cleaned L8 and S1 ts
fwrite(l8_ts, paste0('output/time_series/', country, '_l8_ts.csv'))







