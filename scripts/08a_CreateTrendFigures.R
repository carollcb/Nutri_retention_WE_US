# Need to run code in 08_nutrient-loads-sensslopes.R first to get all_loads_ts

withnames <- inner_join(all_loads_ts, upstream_sites_lagos, by="lagoslakeid")

data = withnames %>%
 # filter(lagoslakeid %in% trending_sites$lagoslakeid) |>
  left_join(trending_sites) |>
  mutate(Trend = as.character(Trend))

str(data)

data <- data |>
  mutate(Trend = ifelse(is.na(Trend), 'no trend', Trend)) 

data <- data |>
  left_join(flux_slope_intercept)

data <- data |>
  mutate(Trend = paste0(nutrient, '-', Trend)) |>
  ungroup()

data_count <- data |>
  select(lagoslakeid, nutrient, Trend) |>
  distinct() |>
  count(nutrient, Trend)

data <- data |>
  filter(lagoslakeid %in% trending_sites$lagoslakeid)

# # 3 panels for N and P each
# N_trends <- ggplot(data |> filter(nutrient == 'TN'), 
#                    aes(y = flux, x=water_year, color = lagoslakeid)) +
#   geom_point()+
#   facet_wrap(~Trend, scales = 'free') +
#   geom_abline(aes(intercept = intercept, slope = slope, group=lagoslakeid,
#                          color=lagoslakeid)) + #MK estimate trend
#   labs(y="", x="") +
#   theme_bw()
# 
# P_trends <- ggplot(data |> filter(nutrient == 'TP'), 
#                    aes(y = flux, x=water_year, color = lagoslakeid)) +
#   geom_point()+
#   facet_wrap(~Trend, scales = 'free') +
#   geom_abline(aes(intercept = intercept, slope = slope, group=lagoslakeid,
#                   color=lagoslakeid)) + #MK estimate trend
#   labs(y="", x="Water Year") +
#   theme_bw()
# 
# (N_trends / P_trends) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
# 
# 
# wrap_elements((N_trends / P_trends) + plot_layout(guides = "collect")) +
#   labs(#tag = "Annual nutrient loads (kgy-1)"
#        tag = "Annual nutrient loads "~(kg~yr^-1)) +
#   theme(
#     plot.tag = element_text(size = rel(1), angle = 90),
#     plot.tag.position = "left"
#   )

# trends in both TN and TP 

both_trend <- data |>
  filter(lagoslakeid %in% c('454184', '461718', '467587', '460768')) |>
  mutate(lagoslakeid = factor(lagoslakeid, levels=c('454184', '461718', '467587', '460768')))

both <- ggplot(both_trend |> mutate(flux = ifelse(nutrient == 'TP', flux*10, flux),
                      intercept = ifelse(nutrient == 'TP', intercept*10, intercept),
                      slope = ifelse(nutrient == 'TP', slope*10, slope))) +
  geom_point(aes(y = flux, x=water_year, color = nutrient))+
  facet_wrap(~lake_namelagos, scales = 'free') +
  geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
                  color=nutrient)) + #MK estimate trend
  labs(y= '', x="", title='Load trends for both TN and TP') +
  scale_y_continuous(
    # first axis
    #name = "Annual TN load "~(kg~yr^-1),
    
    # second axis 
    sec.axis = sec_axis(~./10)
  )  +
  theme_bw() +
  scale_color_manual(values=c("darkblue","#994F00")) +
  theme(
        axis.title.y.right = element_text(color = "#994F00"),
        axis.title.y = element_text(color = "darkblue", vjust = -2),
        axis.line.y.right = element_line(color = "#994F00"),
        axis.line.y.left = element_line(color = "darkblue"),
        legend.title = element_blank(), 
        legend.position = 'none')

ggsave('figures/Fig3_both_N_P_trending.png', height=6, width=8, units='in', dpi=1200)


# single nutrient trenders

single_trend <- data |>
  filter(!lagoslakeid %in% c('454184', '461718', '467587', '460768', '454129')) |> # removed 454129 Lake Havasu
  mutate(trend_sig = ifelse(lagoslakeid %in% c('328600', '361058', '479277'),'No trend TP', 
                            ifelse(lagoslakeid %in% c('457120', '360319','454129'), 'No trend TN, TP decreased', 'No trend TN, TP increased'))) 


# no trend TP
a<- ggplot(single_trend |> filter(trend_sig == 'No trend TP') |> mutate(flux = ifelse(nutrient == 'TP', flux*10, flux),
                            intercept = ifelse(nutrient == 'TP', intercept*10, intercept),
                            slope = ifelse(nutrient == 'TP', slope*10, slope)) |>
             mutate(nutrient = ifelse(nutrient == 'TP', 'TP*10', 'TN'))) +
  geom_point(aes(y = flux, x=water_year, color = nutrient))+
  facet_wrap(~lake_namelagos, scales = 'free') +
  geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
                  color=nutrient)) + #MK estimate trend
  labs(y= 'Annual nutrient load '~(kg~yr^-1), x="", title='No trend TP') +
  # scale_y_continuous(
  #   # first axis
  #   name = "Annual TN load "~(kg~yr^-1),
  #   
  #   # second axis 
  #   sec.axis = sec_axis(~./10, name = "Annual TP load "~(kg~yr^-1))
  # )  +
  theme_bw() +
  scale_color_manual(values=c("darkblue","#994F00")) +
  theme(
    #axis.title.y.right = element_text(color = "gold2"),
    #axis.title.y = element_text(color = "darkblue", vjust = -2),
    #axis.line.y.right = element_line(color = "gold2"),
   # axis.line.y.left = element_line(color = "darkblue"),
    legend.title = element_blank(), 
    legend.position = 'none',
    plot.title = element_text(size=9)) + scale_y_continuous(labels = scales::scientific) 

a

# no trend TN, TP decreases
b <- ggplot(single_trend |> filter(trend_sig == 'No trend TN, TP decreased') |> 
              mutate(flux = ifelse(nutrient == 'TP', flux*10, flux),
                     intercept = ifelse(nutrient == 'TP', intercept*10, intercept),
                     slope = ifelse(nutrient == 'TP', slope*10, slope)) |>
              mutate(nutrient = ifelse(nutrient == 'TP', 'TP*10', 'TN'))) +
  geom_point(aes(y = flux, x=water_year, color = nutrient))+
  facet_wrap(~lake_namelagos, scales = 'free') +
  geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
                  color=nutrient)) + #MK estimate trend
  labs(y= '', x="", title='No trend TN, TP decreased') +
  # scale_y_continuous(
  #   # first axis
  #   name = "Annual TN load "~(kg~yr^-1),
  # 
  #   # second axis
  #   sec.axis = sec_axis(~./10, name = "Annual TP load*10 "~(kg~yr^-1))
  # )  +
  theme_bw() +
  scale_color_manual(values=c("darkblue","#994F00")) +
  theme(
    # axis.title.y.right = element_text(color = "gold2"),
    # axis.title.y = element_text(color = "darkblue", vjust = -2),
    # axis.line.y.right = element_line(color = "gold2"),
    # axis.line.y.left = element_line(color = "darkblue"),
    legend.title = element_blank(), 
    #legend.position = 'none',
    plot.title = element_text(size=9))+ scale_y_continuous(labels = scales::scientific) 

b


# no trend TN, TP increased
c <- ggplot(single_trend |> filter(trend_sig == 'No trend TN, TP increased') |> mutate(flux = ifelse(nutrient == 'TP', flux*10, flux),
                                                                                  intercept = ifelse(nutrient == 'TP', intercept*10, intercept),
                                                                                  slope = ifelse(nutrient == 'TP', slope*10, slope)) |>
              mutate(nutrient = ifelse(nutrient == 'TP', 'TP*10', 'TN'))) +
  geom_point(aes(y = flux, x=water_year, color = nutrient))+
  facet_wrap(~lake_namelagos, scales = 'free') +
  geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
                  color=nutrient)) + #MK estimate trend
  labs(y= '', x="", title='No trend TN, TP increased') +
 # scale_y_continuous(
 #   # first axis
 #   name = "Annual TN load "~(kg~yr^-1),
 # 
 #   # second axis
 #   sec.axis = sec_axis(~./10, name = "Annual TP load "~(kg~yr^-1))
 # )  +
  theme_bw() +
  scale_color_manual(values=c("darkblue","#994F00")) +
  theme(
    # axis.title.y.right = element_text(color = "gold2"),
    # axis.title.y = element_text(color = "darkblue", vjust = -2),
    # axis.line.y.right = element_line(color = "gold2"),
    # axis.line.y.left = element_line(color = "darkblue"),
    legend.title = element_blank(), 
    legend.position = 'none',
    plot.title = element_text(size=9)) + scale_y_continuous(labels = scales::scientific) 

c

library(cowplot)
library(ggpubr)
ggarrange(a,c,b, nrow = 2, ncol = 2, labels = c("A", "B", "C"))
ggsave('figures/Fig4_single_trend.png', height=6.5, width=8.5, units='in', dpi=1200)


ggarrange(a,c,b,both, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), common.legend = T)
ggsave('figures/Fig4_single.png', height=8.5, width=12.5, units='in', dpi=1200)

# ggplot(data |> mutate(flux = ifelse(nutrient == 'TP', flux*10, flux),
#                       intercept = ifelse(nutrient == 'TP', intercept*10, intercept),
#                       slope = ifelse(nutrient == 'TP', slope*10, slope))) +
#   geom_point(aes(y = flux, x=water_year, color = nutrient))+
#   facet_wrap(~lagoslakeid, scales = 'free') +
#   geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
#                   color=nutrient)) + #MK estimate trend
#   labs(y= "Annual nutrient loads "~(kg~yr^-1), x="Water Year") +
#   scale_y_continuous(
#     # first axis
#     name = "Average Weekly Nitrate "~(mg~L^-1),
#     
#     # second axis 
#     sec.axis = sec_axis(~./10, name = "Average Weekly Streamflow "~(m^3~s^-1))
#   )  +
#   theme_bw()
# 
# # ggplot(data) +
# #   geom_point(aes(y = flux, x=water_year, color = nutrient))+
# #   facet_wrap(~lagoslakeid, scales = 'free') +
# #   geom_abline(aes(intercept = intercept, slope = slope, group=nutrient,
# #                   color=nutrient)) + #MK estimate trend
# #   labs(y= "Annual nutrient loads "~(kg~yr^-1), x="Water Year") +
# #   theme_bw() +
# #   coord_trans(y="log10")
# 
# 
# 
# 
# 
# 
# 
# ggplot() +
#   geom_point(data = all_loads_ts %>%
#                filter(lagoslakeid %in% trending_sites$lagoslakeid),
#              aes(y = flux, x=water_year, fill=nutrient),
#              col = "black", shape=21)+
#   facet_wrap(nutrient~lagoslakeid, scales="free")  +
#   # facet_grid(lagoslakeid~nutrient, scales = "free_y") + #You might like the facet_wrap version better
#   geom_abline(flux_slope_intercept,
#               mapping=aes(intercept = intercept, slope = slope, group=lagoslakeid,
#                           color=nutrient)) + #MK estimate trend
#   geom_smooth(data = all_loads_ts %>%
#                 filter(lagoslakeid %in% trending_sites$lagoslakeid), method="lm", se=F,
#               mapping=aes(y=flux,x=water_year), color="black", linetype="dashed")+
#   scale_color_manual(values=c("darkblue","gold"))+
#   scale_fill_manual(values=c("darkblue","gold"))+
#   labs(y="Annual nutrient loads (kgy-1)", x="Water Year")
