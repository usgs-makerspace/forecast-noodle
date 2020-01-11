library(dplyr)

data_to_plot <- readRDS("6_visualize/static_window_plot_ready_data.R")

forecasts_to_plot <- sort(head(tail(colnames(data_to_plot), -1), -1), decreasing = TRUE)
timesteps <- data_to_plot$dateTime

# Only illustrating what 2.5 days out would look like for now

# Setup base plot with observed values in background
x_range <- range(data_to_plot[["dateTime"]])
y_range <- range(as.matrix(data_to_plot[forecasts_to_plot]), na.rm = TRUE)


intro_frames <- function() {
  plot_type <- switch(Sys.info()[['sysname']],
                      Windows= "cairo",
                      Linux  = "Xlib",
                      Darwin = "quartz")
  
  # Frame 1 with text
  png("6_visualize/tmp_test3/frame_AA_intro_1.png", height = 400, width = 800, type = plot_type)
  par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
  showtext::showtext_begin() # begin using google fonts
  plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
  text(x=0.5, y=0.7, "Are river forecasts", cex=5)
  text(x=0.5, y=0.35, "accurate?", cex=7, col = "cornflowerblue")
  showtext::showtext_end()
  dev.off()
  
  # Frame 2 with text
  png("6_visualize/tmp_test3/frame_AA_intro_2.png", height = 400, width = 800, type = plot_type)
  par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
  showtext::showtext_begin() # begin using google fonts
  plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
  text(x=0.5, y=0.8, "Forecasts are generally more", cex=3)
  text(x=0.5, y=0.5, "accurate the closer you are to the", cex=3)
  text(x=0.5, y=0.2, "date you are trying to predict.", cex=3)
  showtext::showtext_end()
  dev.off()
  
  # Frame 3 with text & graph of how to interpret
  png("6_visualize/tmp_test3/frame_AB_intro_3.png", height = 400, width = 800, type = plot_type)
  par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
  showtext::showtext_begin() # begin using google fonts
  plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
  text(x=0.5, y=0.9, "Let's see this concept in action.", cex=3.5)
  # Add lines for illustration
  x_vals <- seq(0.3, 0.7, by=0.02)
  y_vals <- seq(0.3, 0.7, by=0.02)
  x_vals_forecast <- x_vals[6:16]
  y_vals_forecast <- c(0.4, seq(0.47, 0.7, length.out=7), 0.68, 0.65, 0.6)
  x_vals_poly <- c(x_vals[6:16], rev(x_vals_forecast))
  y_vals_poly <- c(y_vals[6:16], rev(y_vals_forecast))
  points(x_vals, y_vals, lwd = 2, col = "#ff150c", lty = "dotted", type="l")
  points(x_vals_forecast[c(T,F)], y_vals_forecast[c(T,F)], col = "#00868B", pch=20, cex=0.9)
  polygon(x_vals_poly, y_vals_poly, col = "#00868B40", border = "#00505359")
  text(x=0.7,y=0.7, "reality", cex=1, pos=4)
  text(x=0.45,y=0.6, "our best guess", cex=1, pos=2)
  text(x=0.67,y=0.5, "difference between guess and reality", cex=1, pos=1)
  arrows(x0=0.54,x1=0.54,y0=0.54,y1=0.69, length = 0.05, code=3, col="#00868B")
  text(x=0.5, y=0.1, "Watch as predictions start to match reality through time ...", cex=1.8)
  showtext::showtext_end()
  dev.off()
}
initiate_plot <- function(fn = NULL) {
  
  plot_type <- switch(Sys.info()[['sysname']],
                      Windows= "cairo",
                      Linux  = "Xlib",
                      Darwin = "quartz")
  
  if(!is.null(fn)) png(fn, height = 400, width = 800, type = plot_type)
  
  par(family = 'abel') # sysfonts::font_add_google('Abel','abel')
  showtext::showtext_begin() # begin using google fonts
  
  # Add baseplot
  par(mar=c(4,7,3,2), lheight = 1.3)
  plot(x_range, y_range, type = 'n', ylab = "", xlab = "", 
       las=1, col.ticks="white")
  mtext("River\nHeight", side=2, line=7, col="black", las=1, padj=0, adj=0, cex=1.5)
  
}
complete_plot <- function(fn = NULL) {
  box(col="white", lwd=3) # added to see if the weird axes overlapping stops
  
  # close off google fonts
  showtext::showtext_end()
  
  if(!is.null(fn)) dev.off()
}

##### Create intro frames #####
intro_frames()

##### Loop to create frames for observed line plotting #####

# Loop through each timestep to show progression of observed
for(t in 1:length(timesteps)) {
  fn <- sprintf("6_visualize/tmp_test3/frame_B_observed_%02d.png", t)
  initiate_plot(fn)
  lines(data_to_plot[["dateTime"]][1:t], data_to_plot[["Stage_Observed"]][1:t], 
        lwd = 4, col = "#ff150c", lty = "dotted")
  title("Observed river height")
  complete_plot(fn)
}

##### Loop to create frames for forecasted line plotting #####
polygon_data <- list()
for(fc_count in 1:length(forecasts_to_plot)) {
  
  fc <- forecasts_to_plot[fc_count]
  fc_val <- tail(unlist(strsplit(fc, "Pred_Stage_")), 1) # "days" only, no extra text
  
  # Get data for current forecast
  fc_data <- data_to_plot[c("dateTime", fc, "Stage_Observed")]
  fc_polygon_data <- fc_data %>%
    tidyr::gather(key = type, value = stage, -dateTime) %>% 
    arrange(dateTime, desc(stage)) %>% 
    # Add column for "first" or "second" value and then spread so that we can plot appropriately
    mutate(val_order = rep(c("first", "second"), nrow(data_to_plot))) %>% 
    select(-type) %>% 
    tidyr::spread(key = val_order, stage) %>% 
    # For now, remove any row with an NA bc it couldn't figure out if it was higher or lower
    # Maybe be smarter in the future
    filter(!is.na(first) & !is.na(second))
  
  for(ts in 0:length(timesteps)) {
    
    if(ts != 0) {
      fc_polygon_data_ts <- fc_polygon_data %>% 
        filter(dateTime <= timesteps[ts])
      x_vals <- c(fc_polygon_data_ts$dateTime, rev(fc_polygon_data_ts$dateTime))
      y_vals <- c(fc_polygon_data_ts$first, rev(fc_polygon_data_ts$second))
    } 
    
    # Add discrete forecast points for this timestep & any previous
    fn <- sprintf("6_visualize/tmp_test3/frame_C_forecast_%s_%02d.png", fc_val, ts)
    
    initiate_plot(fn)
    if(fc > 1) {
      lapply(polygon_data, function(df) {
        polygon(x = df$x_vals, y = df$y_vals, col = "#bdbdbd40", border = "#bdbdbd40")
      })
    }
    points(fc_data$dateTime, fc_data[[2]], col = "#00868B", cex=1.3, pch=20)
    if(ts != 0) polygon(x = x_vals, y = y_vals, col = "#00868B40", border = "#00505359", lwd=2)
    lines(data_to_plot[["dateTime"]], data_to_plot[["Stage_Observed"]], 
          lwd = 4, col = "#ff150c", lty = "dotted")
    title(sprintf("River forecast %s days in advance", fc_val), cex.main=2)
    complete_plot(fn)
    
  }
  # added after timestep loop so it should have the last timestep data
  polygon_data[[fc_count]] <- data.frame(x_vals, y_vals)
  
}

##### Create initial video #####
create_draft_ts_video <- function(out_file, frame_grp = NULL, sec_paused_on_frame = 5, framerate_out = 15) {
  png_frames_all <- list.files('6_visualize/tmp_test3', full.names = TRUE)
  
  if(frame_grp == "C") {
    png_frames_forecast <- png_frames_all[grepl("_C_", png_frames_all)]
    
    png_frames_forecast_df <- data.frame(fn = png_frames_forecast,
                                         stringsAsFactors = FALSE) %>% 
      rowwise() %>% 
      mutate(
        fn_vals_only = gsub("6_visualize/tmp_test3/frame_C_forecast_","", fn),
        forecast = head(unlist(strsplit(fn_vals_only, "_")), 1),
        timestep_extension = tail(unlist(strsplit(fn_vals_only, "_")), 1),
        timestep = head(unlist(strsplit(timestep_extension, "[.]")), 1)) %>% 
      ungroup() %>% 
      # Needed frames to go from 2.5 to 0.0 forecast but from 0-29 timesteps
      arrange(desc(forecast), timestep)
    
    png_frames <- png_frames_forecast_df$fn # Reverse because it is reading in wrong order
  } else {
    png_frames <- png_frames_all[grepl(sprintf("_%s_", frame_grp), png_frames_all)]
  }
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = dataRetrieval::zeroPad(1:length(png_frames), padTo = 3),
                         newName = file.path("6_visualize/tmp_test3", paste0("frame_", countFormatted, ".png")))
  file.rename(from = file_name_df$origName, to = file_name_df$newName)
  shell_command <- sprintf(
    "ffmpeg -y -framerate %s -i 6_visualize/tmp_test3/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
    1/sec_paused_on_frame, framerate_out, out_file)
  system(shell_command)
  file.rename(from = file_name_df$newName, to = file_name_df$origName)
}

intro_video <- "video_intro_draft.mp4"
diagram_video <- "video_intro_diagram_draft.mp4"
observed_video <- "video_observed_draft.mp4"
forecast_video <- "video_forecast_draft.mp4"
create_draft_ts_video(sprintf("6_visualize/%s", intro_video), frame_grp = "AA", sec_paused_on_frame = 5)
create_draft_ts_video(sprintf("6_visualize/%s", diagram_video), frame_grp = "AB", sec_paused_on_frame = 7)
create_draft_ts_video(sprintf("6_visualize/%s", observed_video), frame_grp = "B", sec_paused_on_frame = 0.15)
create_draft_ts_video(sprintf("6_visualize/%s", forecast_video), frame_grp = "C", sec_paused_on_frame = 0.3)

# Concatenate videos with varying framerates
writeLines(sprintf("file %s", c(intro_video, diagram_video, observed_video, forecast_video)), "6_visualize/videosToMerge.txt")
# Need to delete the file if you rerun. Couldn't get `-y` to work with this one ...
system(sprintf('ffmpeg -f concat -i 6_visualize/videosToMerge.txt -c copy %s', "6_visualize/video_forecast_noodle.mp4"))

