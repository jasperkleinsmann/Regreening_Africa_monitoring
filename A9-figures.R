library(feasts)
library(tsibble)
library(fable)
library(data.table)
library(tidyverse)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(scales)
library(lubridate)
library(BBmisc)
library(readr)
library(forcats)


l8_ts <- read_csv('output/time_series/Countries_l8_ts.csv')

# Import the plots data in include the plant date in the graph
plots <- st_read(dsn='output/plot_data/all_countries/Countries_plots_green.GeoJSON')
plots_dt <- tibble(plots)
admins <- st_read('output/plot_data/all_countries/Countries_admins.GeoJSON')

# plot precipitation
png(paste0("Figures/presentation_06-09/Vegetation.png"),
width = 1400, height = 800)
ggplot(l8_ts_lag)+
  geom_line(aes(x=yearmonth, y=ndvi), col='darkgreen', lwd=0.9)+
  #geom_line(aes(x=yearmonth, y=normalize(prcp, method = 'range', range = c(0.0,0.5))),
            #col='darkblue', alpha=0.8, lty=2, lwd=0.9)+
  #geom_line(aes(x=yearmonth, y=normalize(prcp_lag1, method = 'range', range = c(0,0.5))),
            #col='darkred', alpha=0.8, lty=2, lwd=0.9)+
  #geom_line(aes(x=yearmonth, y=normalize(prcp_lag2, method = 'range', range = c(0,0.5))),
            #col='orange', alpha=0.8, lty=2, lwd=0.9)+
  ylim(0,0.75)+
  #scale_y_continuous(name = "Vegetation", sec.axis = sec_axis(~.*475, name="Rainfall (mm)"))+
  labs(x='Time', y='Vegetation', title='Average vegetation Regreening Africa sites over time')+
  theme(axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))
dev.off() 


# Forecast all plots
png(paste0("Figures/presentation_06-09/Vegetation_fc_all.png"),
    width = 1400, height = 800)
ggplot()+
  geom_line(data=l8_ref_lag, 
            aes(x = yearmonth, y = ndvi_int - residuals(l8_armax_mod)$.resid), 
            col='darkred',lwd=0.9, alpha=0.8) +
  geom_line(data=l8_ts_lag, aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=1.3)+
  geom_line(data=l8_fc_ts, aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.9, lty=2)+
  geom_ribbon(data=l8_fc_ts, aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red', lwd=0.8)+
  theme(axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) vegetation including 95% CI'))
dev.off() 

# County
cnt <- 'Kayonza'
ggplot()+
  geom_line(data=l8_ref_cnt[l8_ref_cnt$county==cnt,], 
            aes(x = yearmonth, y = ndvi - residuals(l8_armax_cnt[l8_armax_cnt$county==cnt,])$.resid), 
            col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=l8_ts_cnt[l8_ts_cnt$county==cnt,], aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=0.7)+
  geom_line(data=l8_ts_cnt[l8_ts_cnt$county==cnt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(0.2,0.3))), col='blue')+
  geom_line(data=l8_fc_cnt[l8_fc_cnt$county==cnt,], aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.65, lty=2)+
  geom_ribbon(data=l8_fc_cnt[l8_fc_cnt$county==cnt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red')+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) NDVI in',cnt,'including 95% CI'))


# Per plot
plt <- 'E35'
png(paste0("Figures/presentation_06-09/Plot_graph_19731.png"),
    width = 1400, height = 800)
ggplot()+
  #geom_line(data=l8_ref_plt[l8_ref_plt$plotID==plt,], 
            #aes(x = yearmonth, y = ndvi_int - residuals(l8_armax_plt[l8_armax_plt$plotID==plt,])$.resid), 
            #col='darkred',lwd=0.65, alpha=0.6) +
  geom_line(data=l8_ts_plt[l8_ts_plt$plotID==plt,], aes(x=yearmonth, y=ndvi_int), col='darkgreen',lwd=0.7)+
  geom_line(data=l8_fc_plt[l8_fc_plt$plotID==plt,], aes(x = yearmonth, y = .mean), 
            col='darkred',lwd=0.9, lty=2)+
  geom_ribbon(data=l8_fc_plt[l8_fc_plt$plotID==plt,], aes(x=yearmonth, ymin=lower_95, ymax=upper_95),
              alpha=0.1, fill='red')+
  geom_vline(xintercept=plots[plots$plotID==plt,]$plant_date,lty=3,alpha=0.75)+
  geom_line(data=l8_ts_plt[l8_ts_plt$plotID==plt,], 
            aes(x=yearmonth, y=normalize(prcp, method='range',c(0.2,0.3))), col='blue')+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) vegetation including 95% CI'))+
  theme(axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))
dev.off()


l8_fc_plt %>% 
  filter(plotID=="E35") %>% 
  mutate(ci_95 = hilo(ndvi_int,95),
         upper_95=ci_95$upper,
         lower_95=ci_95$lower,
         ci_80 = hilo(ndvi_int, 80),
         upper_80=ci_80$upper,
         lower_80=ci_80$lower,
         ci_50 = hilo(ndvi_int, 50),
         upper_50=ci_50$upper,
         lower_50=ci_50$lower) %>% 
  ggplot()+
  geom_point(data = l8_ts[l8_ts$plotID==plt,], aes(x = yearmonth(yearmon), y = ndvi), alpha=0.2)+
  geom_vline(xintercept = plots[plots$plotID==plt,]$plant_date, lty=3, alpha=1)+
  geom_ribbon(aes(x=yearmonth, ymin=lower_95, ymax=upper_95), alpha=0.1, fill='blue')+
  geom_ribbon(aes(x=yearmonth, ymin=lower_80, ymax=upper_80), alpha=0.2, fill='blue')+
  #geom_ribbon(aes(x=yearmonth, ymin=lower_50, ymax=upper_50), alpha=0.3, fill='blue')+
  geom_line(aes(x = yearmonth, y = .mean), col='darkblue',lwd=0.9, lty=2)+
  geom_line(data=l8_ts_plt[l8_ts_plt$plotID==plt,], aes(x=yearmonth, y=ndvi_int), col='black',lwd=0.7)+
  labs(y='NDVI', x='Time', title=paste('Actual (green) vs predicted (red) vegetation including 95% CI'))+
  theme(axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))

# Hectare green/not green per zone
county_green <- cnt_green[cnt_green$l8_ha>1000,]

ha <- c()
for (i in 1:length(county_green$l8_ha_green)){
  green_yes_no <- c(county_green$l8_ha_green[i], county_green$l8_ha[i]-county_green$l8_ha_green[i])
  ha <- append(ha, green_yes_no)
}
perc_green <- c()
for (i in 1:length(county_green$l8_ha_green)){
  perc_green_yes_no <- c(county_green$l8_ha_perc[i], NA)
  perc_green <- append(perc_green, perc_green_yes_no)
}
total <- rep(county_green$l8_ha, each=2)
county <- c(rep(county_green$county, each=2))
country <- c(rep(county_green$country, each=2))
success <- rep(c('Greening detected', 'No greening detected'),nrow(county_green))
green_county_stack <- data.table(country, county, success, ha, perc_green, total)


#png(paste0("Figures/Internship_report/",country,"_county_green_ha.png"),
#    width = 1400, height = 800)
green_county_stack %>% 
  mutate(county = forcats::fct_inorder(county)) %>% 
  group_by(country) %>% 
  ggplot() +
  geom_col(aes(x=county, y=ha, fill=success), position='stack')+
  geom_text(aes(x=county, y=total, 
                label=scales::percent(round(perc_green,2))), 
            color='black', vjust=-0.3,size=3, fontface='bold')+
  scale_fill_manual(values=c('darkgreen', 'dark blue'))+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        legend.title=element_blank(), 
        axis.title = element_text(size = 24),
        axis.text=element_text(size=18),
        axis.text.x=element_text(angle=90),
        plot.title = element_text(size = 36, face = 'bold', hjust = 0.5))+
  facet_grid(~country, scales='free_x',space='free_x') + theme(panel.spacing = unit(0, "lines"),
                                                                panel.background = element_rect(color = "grey"))+
  labs(x='District', y='Area (in ha)')
 #dev.off()
boxplot(plots_dt$rmse)



type<-c(rep('Tree Planting', 2), rep('FMNR', 2))
success<-rep(c('2) No regreening deceted', '1) Regreening detected'),2)
ha_greening<-c(type_green$l8_ha[2]*(1-type_green$l8_ha_perc[2]), type_green$l8_ha_green[2],
               type_green$l8_ha[1]*(1-type_green$l8_ha_perc[1]),type_green$l8_ha_green[1])
perc_greening<-c(NA, type_green$l8_ha_perc[2],
                 NA, type_green$l8_ha_perc[1])
green_type_bar <- data.table(type, success, ha_greening, perc_greening)

ggplot(green_type_bar) +
  geom_bar(aes(x=type, y=ha_greening, fill=success), position='stack', stat="identity")+
  geom_text(aes(x=type, y=c(ha_greening[1],ha_greening[1]+ha_greening[2], ha_greening[3],ha_greening[3]+ha_greening[4]), 
                label=round(perc_greening,2)), color='black', vjust=-0.2,fontface='bold')+
  scale_fill_manual(values=c('darkgreen', 'dark blue'))+
  theme(legend.title=element_blank(), 
        axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))+
  labs(x='Management', y='Area succesfully regreened (ha)', 
       title='Area succesully regreened per management type (in ha)')


ggplot(plots)


ggplot(plots_dt)+
  geom_point(aes(x=plant_date,y=green_date, col=country), alpha=0.3)+
  geom_smooth(aes(x=plant_date, y=green_date), method='loess')+
  xlim(date('2015-01-01'), date('2022-10-01'))

ggplot(plots_dt)+
  geom_point(aes(x=Hectare, y=regreening))+
  geom_smooth(aes(x=Hectare, y=regreening), method='loess')+
  xlim(0,100)

plots_dt %>% 
  group_by(country) %>% 
  arrange(green_date) %>% 
  summarise(country = first(country),
            green_cum = cumsum(Hectare[regreening==1]),
            total_area = sum(Hectare),
            perc_cum = green_cum/total_area,
            date = green_date[regreening==1]) %>% 
  ggplot()+
  geom_line(aes(x=date, green_cum, col=country),lwd=1)

  
ggplot(plots_dt)+
  geom_boxplot(aes(x=country,y=rmse/sd))
  

plots_dt %>% 
  filter(plant_date>date('2015-01-01')) %>% 
  group_by(year(plant_date)) %>% 
  summarise(n = n())


plots_dt %>% 
  group_by(country) %>% 
  summarise(country = first(country),
            l8_green = sum(regreening,na.rm=T),
            number_sites = length(regreening),
            perc = sum(regreening,na.rm=T)/length(regreening),
            l8_ha = sum(Hectare),
            l8_ha_green = sum(Hectare[regreening==1],na.rm=T),
            l8_ha_perc = sum(Hectare[regreening==1],na.rm=T)/sum(Hectare)) %>% 
  left_join(admins, by=c('country' = 'ADM0')) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = perc))
 

  


