library(ggplot2)
library(maps)
library(mapdata)
library(tidyverse)

source("scripts/08_nutrient-loads-sensslopes.R")

usa <- map_data('usa')
ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3)

state <- map_data("state")
WEUS <- state %>%
  filter(region %in% c("washington", "california", "utah", "nevada",
                                   "oregon", "idaho","north dakota",
                                   "montana", "wyoming", "colorado",
                                   "new mexico", "arizona", "south dakota", "nebraska", "kansas"))
sites <- read.csv("data/study_sites_final.csv",
                  colClasses = "character",
                  stringsAsFactors = FALSE)


trends_n <- left_join(ts_TNtrends_lakes_sp_2, sites, by="lagoslakeid")%>%
  select(lagoslakeid, lake_lon_decdeg,lake_lat_decdeg, categ)

trends_n$lake_lon_decdeg <- as.numeric(as.character(trends_n$lake_lon_decdeg))
trends_n$lake_lat_decdeg <- as.numeric(as.character(trends_n$lake_lat_decdeg))

#remotes::install_github("wilkelab/cowplot")
#install.packages("colorspace", repos = "http://R-Forge.R-project.org")
#remotes::install_github("clauswilke/colorblindr")

library(colorblindr)

TN_map <- ggplot() + 
 # coord_fixed(1.3) + 
  #geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=WEUS, mapping=aes(x=long, y=lat, group=group),color="black", fill=NA) + 
  geom_sf(data = ts_TNtrends_lakes_sp_2, aes(color= Trend), size=5)+
  geom_point(data = trends_n, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, shape= categ), size=3, color="#110066")+
  scale_color_manual(values = c("grey", "royalblue1", "#FFA500")) +
  #scale_color_OkabeIto()+
  ggtitle('TN loads') +
  labs(shape="Category", labels = c("Lake", "Reservoir"))+
  theme(panel.background = element_rect(fill=NA), axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(breaks = seq(-180, 180, by = 10))+ theme(plot.margin = margin(0, 0, 0, 0))


TN_map
trends <- left_join(ts_TPtrends_lakes_sp_2, sites, by="lagoslakeid")%>%
  select(lagoslakeid, lake_lon_decdeg,lake_lat_decdeg, categ)

trends$lake_lon_decdeg <- as.numeric(as.character(trends$lake_lon_decdeg))
trends$lake_lat_decdeg <- as.numeric(as.character(trends$lake_lat_decdeg))


TP_map <- ggplot() + 
 # coord_fixed(1.3) + 
  #geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=WEUS, mapping=aes(x=long, y=lat, group=group),color="black", fill=NA) + 
  geom_sf(data = ts_TPtrends_lakes_sp_2, aes(color= Trend), size=5)+
  geom_point(data = trends, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, shape= categ), size=3, color="#110066")+
  scale_color_manual(values = c("grey", "royalblue1", "#FFA500")) +
  #scale_color_OkabeIto()+
  ggtitle('TP loads') +
  labs(shape="Category", labels = c("Lake", "Reservoir"))+
  theme(panel.background = element_rect(fill=NA), axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_x_continuous(breaks = seq(-180, 180, by = 10))+ theme(plot.margin = margin(0, 0, 0, 0))

TP_map

library(ggpubr)
multi_map <- ggarrange(TN_map, TP_map, 
                       # labels = c("A", "B"),
                       ncol=2, nrow = 1,
                       widths = c(1, 1), common.legend = TRUE, legend = "bottom")

#multi_plot <- annotate_figure(multi_map) #,
                              #top = text_grob("Spatial distribution of TN and TP loads trends", color = "black", face = "bold", size = 11))
multi_map
ggsave("figures/map_fig2_newcolor.png", width=8, height=6,units="in", dpi=300)

cowplot::plot_grid(TN_map, TP_map, ncol = 2, rel_widths = c(1, 1), align = "hv")
cowplot::plot_grid(TN_map, TP_map, ncol = 2, rel_widths = c(1, 1), align = "hv", axis = "tblr", 
                   greedy = TRUE)

##Map of average load values
mean_n <- left_join(TN_loads_ts, sites, by="lagoslakeid")%>%
  select(lagoslakeid, lake_lon_decdeg,lake_lat_decdeg, flux)%>%
  group_by(lagoslakeid) %>%
  mutate(mean_flux = mean(flux))%>%
  select(lagoslakeid, lake_lon_decdeg,lake_lat_decdeg, mean_flux) %>%
  distinct()

mean_n$lake_lon_decdeg <- as.numeric(as.character(mean_n$lake_lon_decdeg))
mean_n$lake_lat_decdeg <- as.numeric(as.character(mean_n$lake_lat_decdeg))

ggplot() + 
  geom_polygon(data=WEUS, mapping=aes(x=long, y=lat, group=group),color="black", fill=NA) + 
  geom_point(data = mean_n, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, color= mean_flux, size=4))+
  scale_color_continuous() +
  ggtitle('mean TN loads') 
  #labs(shape="Category", labels = c("Lake", "Reservoir"))+
  #theme(panel.background = element_rect(fill=NA), axis.title.x=element_blank(),axis.title.y=element_blank())
 # scale_x_continuous(breaks = seq(-180, 180, by = 10))
