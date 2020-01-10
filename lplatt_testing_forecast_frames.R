library(dplyr)


plot_type <- switch(Sys.info()[['sysname']],
                    Windows= "cairo",
                    Linux  = "Xlib",
                    Darwin = "quartz")

mesonet_files <- list.files(path="1_fetch/out/", 
                            pattern = "mesonet_data_(.*).rds$",
                            full.names = TRUE)
mesonet_all <- NULL
for(file in mesonet_files) {
  one_file <- readRDS(file)
  mesonet_all <- bind_rows(mesonet_all, one_file)
}
mesonet_distinct <- distinct(mesonet_all) %>% 
  mutate(issued = lubridate::round_date(as.POSIXct(issued, tz = "UTC"), unit = '15 minutes'))

# For now, create hourly observations
obs <- readRDS('1_fetch/out/nwis_data.rds') %>% 
  mutate(Day_Hour = format(dateTime, "%Y-%m-%d %H")) %>% 
  group_by(Day_Hour) %>% 
  summarize(GH_Hourly = mean(GH_Inst)) %>% 
  # rename to GH_Inst for now just so I don't have to change the code
  mutate(dateTime = as.POSIXct(Day_Hour, "%Y-%m-%d %H", tz = "UTC"),
         GH_Inst = GH_Hourly)

joined <- inner_join(obs, mesonet_distinct, by = c("dateTime" = "valid")) %>% 
  mutate(forecast_range = dateTime - issued,
         forecast_range_round = 2*(forecast_range/2))

#seems issued datetimes are inconsistent, so forecasted points are odd intervals out :(
#don't line up on nice 6/12/18/24 hour etc intervals
#check out distribution here
nsum <- joined %>% group_by(forecast_range_round) %>% summarize(n=n())


# Filter models to the one right before the first observed date
all_unique_models <- unique(mesonet_distinct$issued) 
first_in_date_range <- head(which(all_unique_models >= min(obs$dateTime)), 1)
first_model_to_use <- all_unique_models[first_in_date_range-1]

# function to plot all past model lines
plot_old_model_lines <- function(t, forecast_data) {
  forecast_data_t <- forecast_data %>% 
    filter(valid <= t)
  unique_models_t <- unique(forecast_data_t$issued) 
  for(m in unique_models_t) {
    m_data <- filter(forecast_data_t, issued == m)
    lines(m_data$valid, m_data$primary_value, lty="dotted", col = "gray60")
  }
}


forecast_data_plot_ready <- mesonet_distinct %>% 
  filter(issued >= first_model_to_use)# %>% 
  # mutate(forecast_range = valid - issued,
  #        forecast_range_round = 2*(forecast_range/2),
  #        forecast_category = cut(as.numeric(forecast_range), breaks = c(0, 24, 72, Inf), 
  #                                labels = c("Short Range Forecast", "Medium Range Forecast", "Long Range Forecast"))) %>% 
  # group_by(valid, forecast_category) %>% 
  # slice(which.max(primary_value), which.min(primary_value)) %>% # returns the first maximum in each group
  # ungroup()

range <- range(obs$dateTime)

model_timesteps <- unique(forecast_data_plot_ready$issued)

for(i in 1:length(model_timesteps)) {
  
  t_model <- model_timesteps[i]
  t_model_next <- model_timesteps[i + 1]
  
  # Get correct model data
  model_df_i <- forecast_data_plot_ready %>% 
    # mutate(model_released = abs(as.numeric(issued - t))) %>% # should probably do closest before or after, not both
    filter(issued == t_model) %>% 
    #top_n(-1, model_released) %>% 
    #left_join(obs, by = c("valid" = "dateTime")) %>% 
    select(issued, valid, primary_value) %>% #, forecast_range_round, forecast_category) %>%
    distinct()
  
  # Range of observations that will be plot occur before the next model is issued
  obs_df <- filter(obs, dateTime >= t_model & dateTime <= t_model_next)
  
  # Loop through each observation to plot the data
  for(t_obs in (1:length(unique(obs_df$dateTime)))[c(T,F)]) { # Down sampled by keeping every other observation
    
    timestep <- unique(obs_df$dateTime)[t_obs]
    obs_to_plot <- filter(obs, dateTime <= timestep)
    
    # Model info for this timestep
    model_df_timestep <- model_df_i %>% 
      # Trying to add this here instead of before and do range based on current date of frame
      mutate(forecast_range = valid - issued, #~~~~~~~~~~~~~~~!!!!!!!!!!!!!!!!!! might need to actually keep valid-issued
             forecast_range_round = 2*(forecast_range/2)) %>% 
      filter(forecast_range_round >= 0) %>% 
      mutate(forecast_range_round = as.numeric(forecast_range_round),
             forecast_category = cut(forecast_range_round, breaks = c(-Inf, 24, 72, Inf), 
                                     labels = c("Short Range Forecast", "Medium Range Forecast", "Long Range Forecast"))) %>% 
      #group_by(valid, forecast_category) %>% 
      #slice(which.max(primary_value), which.min(primary_value)) %>% # returns the first maximum in each group
      #ungroup() %>% 
      left_join(obs, by = c("valid" = "dateTime"))
      #####
    
    # Set up frame to save
    png_fn <- sprintf("6_visualize/tmp_test/frame_%s.png", format(timestep, "%Y%m%d_%H"))
    message(sprintf("Creating %s", png_fn))
    png(png_fn, height = 400, width = 800, type = plot_type)
    #if(t == structure(1558785600, class = c("POSIXct", "POSIXt"), tzone = "UTC")) browser()
    # Set up base plot
    plot(range(obs$dateTime), range(obs$GH_Inst), type = 'n', ylab = "Stage", xlab = "Date")
    
    # Add old lines to plot
    #plot_old_model_lines(timestep, mesonet_distinct)
    
    # Now add short term forecast
    short_term_df <- model_df_timestep %>% 
      filter(forecast_category == "Short Range Forecast") %>% 
      select(dateTime = valid, primary_value, GH_Inst)
    short_term_df_plot_ready <- short_term_df %>% 
      tidyr::gather(key = type, value = stage, -dateTime) %>% 
      arrange(dateTime, desc(stage)) %>% 
      # Add column for "first" or "second" value and then spread so that we can plot appropriately
      mutate(val_order = rep(c("first", "second"), nrow(short_term_df))) %>% 
      select(-type) %>% 
      #distinct(stage, val_order, .keep_all = TRUE) %>% # I think the "distinct" in the code above is what I need
      tidyr::spread(key = val_order, stage) %>% 
      # For now, remove any row with an NA bc it couldn't figure out if it was higher or lower
      # Maybe be smarter in the future
      filter(!is.na(first) & !is.na(second))
    
    polygon(x = c(short_term_df_plot_ready$dateTime, rev(short_term_df_plot_ready$dateTime)), 
            y = c(short_term_df_plot_ready$first, rev(short_term_df_plot_ready$second)),
            col = "#00868BBF", border = "#00868BFF")
    
    # Now add medium term forecast
    medium_term_df <- model_df_timestep %>% 
      filter(forecast_category == "Medium Range Forecast") %>% 
      select(dateTime = valid, primary_value, GH_Inst)
    medium_term_df_plot_ready <- medium_term_df %>% 
      tidyr::gather(key = type, value = stage, -dateTime) %>% 
      arrange(dateTime, desc(stage)) %>% 
      # Add column for "first" or "second" value and then spread so that we can plot appropriately
      mutate(val_order = rep(c("first", "second"), nrow(medium_term_df))) %>% 
      select(-type) %>% 
      tidyr::spread(key = val_order, stage) %>% 
      # For now, remove any row with an NA bc it couldn't figure out if it was higher or lower
      # Maybe be smarter in the future
      filter(!is.na(first) & !is.na(second))
    
    polygon(x = c(medium_term_df_plot_ready$dateTime, rev(medium_term_df_plot_ready$dateTime)), 
            y = c(medium_term_df_plot_ready$first, rev(medium_term_df_plot_ready$second)),
            col = "#00868B80", border = "#00868BA6")
    
    # Now add long term forecast
    long_term_df <- model_df_timestep %>% 
      filter(forecast_category == "Long Range Forecast") %>% 
      select(dateTime = valid, primary_value, GH_Inst)
    long_term_df_plot_ready <- long_term_df %>% 
      tidyr::gather(key = type, value = stage, -dateTime) %>% 
      arrange(dateTime, desc(stage)) %>% 
      # Add column for "first" or "second" value and then spread so that we can plot appropriately
      mutate(val_order = rep(c("first", "second"), nrow(long_term_df))) %>% 
      select(-type) %>% 
      tidyr::spread(key = val_order, stage) %>% 
      # For now, remove any row with an NA bc it couldn't figure out if it was higher or lower
      # Maybe be smarter in the future
      filter(!is.na(first) & !is.na(second))
    
    polygon(x = c(long_term_df_plot_ready$dateTime, rev(long_term_df_plot_ready$dateTime)), 
            y = c(long_term_df_plot_ready$first, rev(long_term_df_plot_ready$second)),
            col = "#00868B40", border = "#00868B59")
    
    # Add obs lines
    #lines(obs$dateTime, obs$GH_Inst, lty = "dashed") # All observed values going into the future
    lines(obs_to_plot$dateTime, obs_to_plot$GH_Inst, lwd = 3, col = "red") # Current observation
    
    dev.off()
    
    
  #browser()
  }
  
}

##### Create video NOT CURRENTLY WORKING
out_file <- "video_test_montague_12hr.mp4"

# Get list of frames that should be in this current build
png_frames <- list.files('6_visualize/tmp_test', full.names = TRUE)

file_name_df <- tibble(origName = png_frames,
                       countFormatted = dataRetrieval::zeroPad(1:length(png_frames), padTo = 3),
                       newName = file.path("6_visualize/tmp_test", paste0("frame_", countFormatted, ".png")))
file.rename(from = file_name_df$origName, to = file_name_df$newName)

# added ffmpeg better code for reducing video size
# see https://unix.stackexchange.com/questions/28803/how-can-i-reduce-a-videos-size-with-ffmpeg
# and https://slhck.info/video/2017/02/24/crf-guide.html

shell_command <- sprintf(
  "ffmpeg -y -framerate %s -i 6_visualize/tmp_test/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
  8, 10, out_file)
system(shell_command)

file.rename(from = file_name_df$newName, to = file_name_df$origName)
