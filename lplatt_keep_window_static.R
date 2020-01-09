# static small window
# model added to window to show it converging

library(dplyr)

plot_type <- switch(Sys.info()[['sysname']],
                    Windows= "cairo",
                    Linux  = "Xlib",
                    Darwin = "quartz")

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

##### Plot things #####

# Create a frame for all except dateTime & Stage_Observed
# Also sort them because 0 and 0.5 days out are always first
cols_to_plot <- sort(head(tail(colnames(data_to_plot), -1), -1), decreasing = TRUE)

for(i in cols_to_plot) {
  
  png(sprintf("6_visualize/tmp_test2/frame_%s.png", i), height = 400, width = 800, type = plot_type)
  
  # Setup base plot with observed values in background
  x_range <- range(data_to_plot[["dateTime"]])
  y_range <- range(as.matrix(data_to_plot[cols_to_plot]), na.rm = TRUE)
  plot(x_range, y_range, type = 'n', ylab = "Stage", xlab = "Time")
  lines(data_to_plot[["dateTime"]], data_to_plot[["Stage_Observed"]], lwd = 2, col = "lightgrey", lty = "dotted")
  
  # Add forecast line
  #lines(data_to_plot[["dateTime"]], data_to_plot[[i]], lwd = 3, col = "blue")
  
  # Try a forecast polygon
  i_polygon_data <- data_to_plot[c("dateTime", i, "Stage_Observed")] %>%
    tidyr::gather(key = type, value = stage, -dateTime) %>% 
    arrange(dateTime, desc(stage)) %>% 
    # Add column for "first" or "second" value and then spread so that we can plot appropriately
    mutate(val_order = rep(c("first", "second"), nrow(data_to_plot))) %>% 
    select(-type) %>% 
    tidyr::spread(key = val_order, stage) %>% 
    # For now, remove any row with an NA bc it couldn't figure out if it was higher or lower
    # Maybe be smarter in the future
    filter(!is.na(first) & !is.na(second))
  
  x_vals <- c(i_polygon_data$dateTime, rev(i_polygon_data$dateTime))
  y_vals <- c(i_polygon_data$first, rev(i_polygon_data$second))
  polygon(x = x_vals, y = y_vals, col = "#00868B40", border = "#00868B59")
  
  title(sprintf("Forecast %s days in advance", tail(unlist(strsplit(i, "Pred_Stage_")), 1)))
  
  dev.off()
}

##### Create video
out_file <- "video_test_static_window.mp4"
png_frames <- rev(list.files('6_visualize/tmp_test2', full.names = TRUE)) # Reverse because it is reading in wrong order
file_name_df <- tibble(origName = png_frames,
                       countFormatted = dataRetrieval::zeroPad(1:length(png_frames), padTo = 3),
                       newName = file.path("6_visualize/tmp_test2", paste0("frame_", countFormatted, ".png")))
file.rename(from = file_name_df$origName, to = file_name_df$newName)
shell_command <- sprintf(
  "ffmpeg -y -framerate %s -i 6_visualize/tmp_test2/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
  2, 10, out_file)
system(shell_command)
file.rename(from = file_name_df$newName, to = file_name_df$origName)
