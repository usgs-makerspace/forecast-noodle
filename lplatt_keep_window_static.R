# static small window
# model added to window to show it converging

# https://mesonet.agron.iastate.edu/plotting/auto/plot/160/station:MTGN4::dt:2019-10-28%200000::var:primary::dpi:100.csv

library(dplyr)

time0 <- as.POSIXct("2019-11-01 07:30:00 UTC")
time_min <- time0 - (2*3600*24)
time_max <- time0 + (2*3600*24)

##### Merge model info into one data frame #####
mesonet_files <- list.files(path="1_fetch/out/", 
                            pattern = "mesonet_data_(.*).rds$",
                            full.names = TRUE)
mesonet_all <- NULL
for(file in mesonet_files) {
  one_file <- readRDS(file)
  mesonet_all <- bind_rows(mesonet_all, one_file)
}

##### Calculate how far in advance (in days) the forecast & format into columns #####

models_in_window <- mesonet_all %>% 
  mutate(issued = as.POSIXct(issued, "%Y-%m-%d %H:%M", tz="UTC")) %>% 
  # Somehow, there are 31 "issued" dates that are greater than "valid"
  filter(issued < valid) %>%
  filter(valid >= time_min, valid <= time_max) %>%
  mutate(forecast_range_days = as.numeric(difftime(issued, valid, units = "days")),
         forecast_range_round = ceiling(forecast_range_days/0.5) * 0.5 * -1,
         forecast_name = paste0("Pred_Stage_", sprintf("%.01f", forecast_range_round))) %>% 
  select(valid, forecast_name, primary_value) %>%  
  distinct() %>% 
  # Due to rounding the forecast range, some entries have the exact same identifier 
  #     E.g. valid & forecast_name are the same but primary_value is different
  # For those instances, we are going to use the average forecasted value
  group_by(forecast_name, valid) %>% 
  summarize(primary_value = mean(primary_value)) %>% 
  tidyr::spread(key = forecast_name, value = primary_value) %>% 
  rename(dateTime = valid)

##### Replace missing values with previously predicted value #####

forecast_data <- models_in_window %>% 
  tidyr::gather(key = forecast_name, value = stage, -dateTime) %>% 
  #arrange(forecast_name, dateTime) %>% 
  arrange(dateTime, forecast_name) %>% 
  group_by(dateTime) %>% # So that the values are only getting filled by the previous forecasted
  tidyr::fill(stage, .direction = "down") %>%
  ungroup() %>%
  tidyr::spread(key = forecast_name, value = stage) 
  
# x <- data.frame(t(apply(models_in_window[-1], 1, zoo::na.locf)))
# lapply(x, function(x) t(as.data.frame(x)))
# do.call(full_join, lapply(x, function(x) t(as.data.frame(x))))


##### Get observed data into hourly averages #####

# For now, create hourly observations
obs_hourly <- readRDS('1_fetch/out/nwis_data.rds') %>% 
  filter(dateTime >= time_min, dateTime <= time_max) %>%
  mutate(Day_Hour = format(dateTime, "%Y-%m-%d %H")) %>% 
  group_by(Day_Hour) %>% 
  summarize(GH_Hourly = mean(GH_Inst)) %>% 
  ungroup() %>% 
  # rename to GH_Inst for now just so I don't have to change the code
  mutate(dateTime = as.POSIXct(Day_Hour, "%Y-%m-%d %H", tz = "UTC")) %>% 
  select(dateTime, Stage_Observed = GH_Hourly)

##### Join observed data into forecast data #####

data_to_plot <- left_join(forecast_data, obs_hourly, by = "dateTime")

saveRDS(data_to_plot, "6_visualize/static_window_plot_ready_data.R")
