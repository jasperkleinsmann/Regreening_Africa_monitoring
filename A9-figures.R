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


l8_ts <- data.table(read_csv('Rdata/output/time_series/Rwanda_l8_ts.csv'))
s1_ts <- data.table(read_csv('Rdata/output/time_series/Rwanda_s1_ts.csv'))

# Import the plots data in include the plant date in the graph
plots <- data.table(read_csv('Rdata/plot_data/Rwanda_plots_all.csv'))

# Filter out time stamps before the observation
l8_ts <- l8_ts[l8_ts$yearmon > as.Date('2013-01-01') & l8_ts$yearmon < as.Date('2022-09-01')] # start L8 observation
s1_ts <- s1_ts[s1_ts$yearmon > as.Date('2014-01-01') & s1_ts$yearmon < as.Date('2022-09-01')] # start S1 observation

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

t <- l8_fc_plt %>% 
  group_by(plotID) %>% 
  summarize(s=sum(sgnf_outlier, na.rm=T))
View(t)


# Per plot
plt <- 35
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

# Regional statistics
png("Figures/presentation_06-09/Rwanda_cnt_green_ha.png",
    width = 1400, height = 800)
ggplot(cnt_green, aes(x="", y=l8_ha_green, fill=county)) +
  geom_bar(stat="identity", width=0.5, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(y = c(1650, 250, 900, 2250), 
                label = c('Gatsibo (672ha)', 'Nyagatare (398ha)',
                          'Kayonza (901ha)', 'Bugesera (581ha)')), color = "white", size=7.5) +
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 28, face = 'bold', hjust = 0.5))+
  labs(title=paste('Total confirmed regreened area in Rwanda --> 2554ha'))
dev.off()


#png("Figures/presentation_06-09/Rwanda_cnt_green_prct.png",
    #width = 1400, height = 800)
ggplot(cnt_green) +
  geom_bar(aes(x=county, y=l8_ha_perc, fill=county), stat="identity")+
  theme(legend.position="none")+ 
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))+
  labs(x='County', y='Area succesfully regreened (%)', 
       title='Area of land succesully regreened per county (in %)')
#dev.off()


type<-c(rep('FMNR', 2), rep('Tree Planting', 2))
success<-rep(c('Greening', 'No greening'),2)
ha_greening<-c(type_green$l8_ha_green[2], type_green$l8_ha[2]*(1-type_green$l8_ha_perc[2]),
               type_green$l8_ha_green[1], type_green$l8_ha[1]*(1-type_green$l8_ha_perc[1]))
perc_greening<-c(type_green$l8_ha_perc[2], 1-type_green$l8_ha_perc[2],
                 type_green$l8_ha_perc[1], 1-type_green$l8_ha_perc[1])
green_type_bar <- data.table(type, success, ha_greening, perc_greening)

ggplot(green_type_bar) +
  geom_bar(aes(x=type, y=ha_greening, fill=success), position='stack', stat="identity")+
  geom_text(aes(x=type, y=c(ha_greening[1]+ha_greening[2],ha_greening[1],ha_greening[3]+ha_greening[4], ha_greening[3]), 
                label=round(perc_greening,2)), color='white', vjust=2)+
  scale_fill_manual(values=c('darkgreen', 'brown3'))+
  theme(legend.title=element_blank(), 
        axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5))+
  labs(x='Management', y='Area succesfully regreened (ha)', 
       title='Area succesully regreened per management type (in ha)')


ggplot(cnt_green[cnt_green$number_sites>50,]) +
  geom_bar(aes(x=county, y=l8_ha_perc, fill=county), stat="identity")+
  theme(legend.title=element_blank(), 
        axis.title = element_text(size = 24),
        axis.text=element_text(size=20),
        plot.title = element_text(size = 32, face = 'bold', hjust = 0.5),
        axis.text.x=element_text(angle=90, vjust=0.6))+
  labs(x='', y='Area succesfully regreened (ha)', 
       title='Area succesully regreened per management type (in ha)')









