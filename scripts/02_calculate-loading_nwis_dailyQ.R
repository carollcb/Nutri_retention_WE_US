source("scripts/99_utils.R")

candidate_sites <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                              colClasses = "character",
                                              stringsAsFactors = FALSE
)  %>%
  dplyr::select(station_id, flow_station_id) %>%
  rename(sites = station_id) 


#write.csv(candidate_sites, "upstream_gauges_final.csv")

pb <- progress_bar$new(total = length(candidate_sites$sites),
                       format = "calculating loading for :site_no [:bar] :percent",
                       clear = FALSE,
                       width = 80, show_after = 0)

for(site_no in candidate_sites$sites){
  # site_no <- "05427718"
  # site_no <- candidate_sites$sites[1]
  # pb$tick(tokens = list(site_no = site_no))
  print(paste(which(candidate_sites$sites == site_no), 'out of',
              nrow(candidate_sites), sep = ' '))
  file_out <- paste0("data/results_new_Q/", site_no, "/loadflex.csv")
  if(!file.exists(file_out)){

    file_in <- paste0("data/new_Q_TP_TN/", site_no, ".rds")
    dt      <- readRDS(file_in)
    dt      <- dt %>% # resample to median values per day
      mutate(date = as.Date(strftime(dt$date_time, "%Y-%m-%d"))) %>%
      group_by(date) %>%
      summarize(across(all_of(names(dt)), function(x) median(x, na.rm = TRUE))) %>%
      data.frame()
    data <- dt

    # Fit model for phosphorous
    model_no_tp <- selBestModel(
      "phosphorus_mgl",
      data = data, flow = "flow_cfs",
      dates = "date_time", conc.units = "mg/L",
      station = site_no, time.step = "instantaneous")$model.no
    # print(paste0('best model = ',(model_no)))
    data(Models); Models[model_no_tp,]

    fit_reg2_tp <- suppressWarnings(suppressMessages(loadReg2(
        loadReg(
        as.formula(paste0("phosphorus_mgl ~ model(", model_no_tp, ")")),
        data = data, flow = "flow_cfs", dates = "date",
        time.step = "day")
      )))
    fit_tp <- getFittedModel(fit_reg2_tp)

    summarizeModel(fit_tp)
    fit_tp$cfit$RSQ # cfit = concentration
    fit_tp$lfit$RSQ # lfit = load

    # use complete flow data to get an accurate load sum
    preds_tp <- predictSolute(fit_reg2_tp, newdata = data,
                              flux.or.conc =  'flux', 
                              date = TRUE, se.fit = TRUE)
    # plot(preds$date, preds$flux, type='l', main = paste0('Model ', model_no))
    
    # Fit model for nitrogen
    model_no_tn <- selBestModel(
      "nitrogen_mgl",
      data = data, flow = "flow_cfs",
      dates = "date_time", conc.units = "mg/L",
      station = site_no, time.step = "instantaneous")$model.no
    # print(paste0('best model = ',(model_no)))
    data(Models); Models[model_no_tn,]

    fit_reg2_tn <- suppressWarnings(suppressMessages(loadReg2(
        loadReg(
        as.formula(paste0("phosphorus_mgl ~ model(", model_no_tn, ")")),
        data = data, flow = "flow_cfs", dates = "date",
        time.step = "day")
      )))
    fit_tn <- getFittedModel(fit_reg2_tn)

    summarizeModel(fit_tn)
    fit_tn$cfit$RSQ # cfit = concentration
    fit_tn$lfit$RSQ # lfit = load

    # use complete flow data to get an accurate load sum
    preds_tn <- predictSolute(fit_reg2_tn, newdata = data,
                              flux.or.conc =  'flux', 
                              date = TRUE, se.fit = TRUE)
    # plot(preds$date, preds$flux, type='l', main = paste0('Model ', model_no))

    # get annual load, flow, concentration time series
    summary_dat  <- data %>%
      mutate(water_year = calcWaterYear(date)) %>%
      group_by(water_year) %>%
      summarize(tp_mgl = mean(phosphorus_mgl, na.rm = TRUE),
                tn_mgl = mean(nitrogen_mgl, na.rm = TRUE)) %>%
      dplyr::filter(water_year >= 2000 & water_year <= 2018)

    preds <- rename(preds_tn,
                    flux_TN = flux,
                    se.flux_TN = se.fit) %>%
      left_join(rename(preds_tp,
                       flux_TP = flux,
                       se.flux_TP = se.fit),
                by = 'date')
    preds_annual <- left_join(preds, data, by = "date") %>%
      # mutate(year = strftime(date, "%Y")) %>%
      mutate(water_year = calcWaterYear(date)) %>%
      group_by(water_year) %>%
      summarize(flux_TN_kgy = sum(flux_TN),  # kg/yr
                flux_TP_kgy = sum(flux_TP),  # kg/yr
                flux_TN_kgy_se = sum(se.flux_TN),
                flux_TP_kgy_se = sum(se.flux_TP),
                flow_m3y = mean(flow_cfs) * 893000.0741) %>%  # m3/yr
      dplyr::filter(water_year >= 2000 & water_year <= 2018) %>%
      left_join(summary_dat, by = "water_year")

    preds_annual$RSQ_TN     <- fit_tn$lfit$RSQ # lfit = load
    preds_annual$p.value_TN <- summarizeModel(fit_tn)$p.value
    preds_annual$RMSE_TN    <- summarizeModel(fit_tn)$RMSE
    preds_annual$RSQ_TP     <- fit_tp$lfit$RSQ # lfit = load
    preds_annual$p.value_TP <- summarizeModel(fit_tp)$p.value
    preds_annual$RMSE_TP    <- summarizeModel(fit_tp)$RMSE

    # save preds_annual as data/results/{{site_no}}/loadflex.csv
    dir.create(paste0("data/results_new_Q/", site_no), showWarnings = FALSE, recursive = TRUE)
    write.csv(preds_annual,
              paste0("data/results_new_Q/", site_no, "/loadflex.csv"),
              row.names = FALSE)
  }
  read.csv(file_out, stringsAsFactors = FALSE)
}

#preds_annual <- read.csv("data/results/08383500/loadflex.csv")
# sapply(candidate_sites, function(x) unlink(paste0("data/results/", x, "/loadflex.csv")))

if(interactive()){
plot(preds_annual$water_year, preds_annual$flux_kgy, type = "b", ylim = c(0, 35000))

carpenter_2018 <- data.frame(
  year = 2001:2015,
  pb_load = c(
    "2 2001.006 5217.391",
    "3 2002.053 2318.841",
    "4 2003.017 1594.203",
    "5 2003.939 8985.507",
    "6 2005.028 10434.783",
    "7 2006.034 2753.623",
    "8 2007.081 6666.667",
    "9 2008.045 16086.957",
    "10 2008.966 16521.739",
    "11 2009.972 6884.058",
    "12 2011.061 6231.884",
    "13 2011.983 2898.551",
    "14 2013.073 11231.884",
    "15 2014.078 8623.188",
    "16 2015.042 3333.333")
  )
# x <- carpenter_2018$pb_load[5]
carpenter_2018$pb_load <- sapply(carpenter_2018$pb_load,
                          function(x)
                            as.numeric(strsplit(x, " ")[[1]][3])
                          )
carpenter_2018$year <- preds_annual$water_year

plot(preds_annual$water_year, preds_annual$flux, type = "b", ylim = c(0, 35000))
points(preds_annual$water_year, carpenter_2018$pb_load, col = "red")



preds <- predictSolute(fit_reg2, 'conc', date = TRUE, se.fit = TRUE,
                       lin.or.log = 'lin')
plot(new_dt$date, preds$conc, type='l', main = paste0('Model ', model_no))
points(dt$date, dt$phosphorus_mgl, col='red3')



# ----

# dplyr::filter(dataRetrieval::pCodeToName,
#               parm_cd == "00060")
dplyr::filter(dataRetrieval::pCodeToName,
              characteristicname == "Phosphorus_mg.l.as.P")

dataRetrieval::pCodeToName %>%
  filter(str_detect(characteristicname, "^Phosphorus")) %>%
  View()



View(dt$flow)

par(mfrow = c(2, 1))
plot(dt$flow$dateTime, dt$flow$result_va, type = "l")
plot(dt$phosphorus_grab$startDateTime, dt$phosphorus_grab$result_va, type = "l")


#####

data(app1.calib)
app1.lr <- selBestModel("Phosphorus", data = app1.calib,
                        flow = "FLOW", dates = "DATES", conc.units="mg/L",
                        station=site_no)
# Extract the fitted values
print(app1.lr)

}
