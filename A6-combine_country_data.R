
# Assign county, regio and country to each plots file
countries <- list('Ethiopia','Ghana','Kenya','Mali','Niger','Rwanda','Senegal')

# Create plots data paths
country_plots_path <- list()
for (cntr in countries){
  path <- paste0('output/plot_data/', cntr, '/', cntr, '_plots_all.GeoJSON')
  country_plots_path <- append(country_plots_path, path)
}
plots_list <- lapply(country_plots_path, st_read) # import
plots <- rbindlist(plots_list, use.names = T)
plots <- st_sf(plots)
# Change the column names of the plots data
names(plots)[names(plots) == 'district_commune_woreda'] <- 'county'
names(plots)[names(plots) == 'county_region'] <- 'region'

# Import centroids
country_plots_pnt_path <- list()
for (cntr in countries){
  path <- paste0('output/plot_data/', cntr, '/', cntr, '_plot_centroid.GeoJSON')
  country_plots_pnt_path <- append(country_plots_pnt_path, path)
}
centroid_list <- lapply(country_plots_pnt_path, st_read) # import
plot_centroids <- rbindlist(centroid_list, use.names = T)
plot_centroids <- st_sf(plot_centroids)


#### Administrative boundaries
# Create path adminstrative boundaries
admin2_shp <- list('eth_admbnda_adm2_csa_bofedb_2021.shp', 'gha_admbnda_adm2_gss_20210308.shp',
                   'ken_admbnda_adm2_iebc_20180607.shp', 'mli_admbnda_adm2_1m_gov_20211220.shp',
                   'NER_adm02_feb2018.shp', 'rwa_adm2_2006_NISR_WGS1984_20181002.shp',
                   'sen_admbnda_adm2_1m_gov_ocha_20190426.shp')
admin_path <- mapply(paste0, list('data'), '/admin_boundaries/',  countries, '/', admin2_shp)
admin_list <- lapply(admin_path, st_read)

# Make the admin column names consistent 
admin_list[[6]] <- admin_list[[6]][!names(admin_list[[6]]) %in% c("ADM0_FR","ADM1_FR")]
admin_list[[5]]$ADM0 <- 'Niger'
admin_list[[4]]$ADM0_FR <- 'Mali'

admin_cols <- c('ADM2_EN'='ADM2', 'ADM2_FR'='ADM2', 'adm_02'='ADM2',
                'ADM1_EN'='ADM1', 'ADM1_FR'='ADM1', 'adm_01'='ADM1',
                'ADM0_EN'='ADM0', 'ADM0_FR'='ADM0', 'adm_0'='ADM0')

for (i in 1:length(admin_list)){
  col_names <- names(admin_list[[i]])
  for (c in 1:length(col_names)){
    col_name <- admin_cols[names(admin_list[[i]][c])][1]
    if(!is.na(col_name)){
      names(admin_list[[i]])[names(admin_list[[i]]) == col_names[c]] <- col_name
    }
  }
}
# Rbind all the country admins into one admin df
admins <- rbind(admin_list[[1]][,c('ADM0','ADM1','ADM2')],
                admin_list[[2]][,c('ADM0','ADM1','ADM2')],
                admin_list[[3]][,c('ADM0','ADM1','ADM2')],
                admin_list[[4]][,c('ADM0','ADM1','ADM2')],
                admin_list[[5]][,c('ADM0','ADM1','ADM2')],
                admin_list[[6]][,c('ADM0','ADM1','ADM2')],
                admin_list[[7]][,c('ADM0','ADM1','ADM2')])


## Merge the admin info to the plot info
admin <- st_transform(admins, crs=st_crs(plots))
st_write(admin, dsn=paste0('output/plot_data/all_countries/Countries_admins.GeoJSON'))

sf::sf_use_s2(FALSE) 
county_index <- st_within(plot_centroids, admins, sparse=T)
null_counties <- which(sapply(county_index,length)==0)
county_index[null_counties] <- NA
county_index <- unlist(county_index)
plots$county <- admins$ADM2[unlist(county_index)] # county
plots$region <- admin$ADM1[unlist(county_index)] # region
plots$country <- admin$ADM0[unlist(county_index)] # country
plots <- plots[!is.na(plots$county),] # remove plots outside the country boundary

# create new column that is truly unique 
plots$plot_ID <- plots$plotID
for (i in 1:length(countries)){
  plots$plotID[plots$country==countries[[i]]] <- paste0(substr(plots$country[plots$country==countries[[i]]][1], 1,1), 
                                                        plots$plot_ID[plots$country==countries[[i]]])
}

# Write final plots df
st_write(plots, dsn=paste0('output/plot_data/all_countries/Countries_plots_final.GeoJSON'))
plots <- st_read(dsn=paste0('output/plot_data/all_countries/Countries_plots_final.GeoJSON'))

##### Import satellite data and merge into single df
l8_ts <- fread(paste0('output/time_series/', countries[[1]], '_l8_ts.csv'))  
l8_ts <- l8_ts[,c('plotID','yearmon', 'date', 'ndvi', 'prcp_month','county')]
l8_ts$country <- countries[[1]]

for (i in 2:length(countries)){
  print(i)
  cntr_ts <- fread(paste0('output/time_series/', countries[[i]], '_l8_ts.csv'))  
  cntr_ts <- cntr_ts[,c('plotID','yearmon', 'date', 'ndvi', 'prcp_month','county')]
  cntr_ts$country <- countries[[i]]
  l8_ts <- rbind(l8_ts, cntr_ts)
}
# Create new column that is truly plot unique
l8_ts$plot_ID <- l8_ts$plotID
for (i in 1:length(countries)){
  l8_ts$plotID[l8_ts$country==countries[[i]]] <- paste0(substr(l8_ts$country[l8_ts$country==countries[[i]]][1], 1,1), 
                                                        l8_ts$plot_ID[l8_ts$country==countries[[i]]])
}

# Write final plots df
fwrite(l8_ts, 'output/plot_data/all_countries/Countries_l8_ts.csv')

