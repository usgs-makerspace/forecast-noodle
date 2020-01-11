##### Plot things #####

library(dplyr)

plot_type <- switch(Sys.info()[['sysname']],
                    Windows= "cairo",
                    Linux  = "Xlib",
                    Darwin = "quartz")

data_to_plot <- readRDS("6_visualize/static_window_plot_ready_data.R")

# Create an intro frame

# Frame 1 with text
png("6_visualize/tmp_test2/frame_A1_intro.png", height = 400, width = 800, type = plot_type)
par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
showtext::showtext_begin() # begin using google fonts
plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
text(x=0.5, y=0.7, "Are river forecasts", cex=5)
text(x=0.5, y=0.35, "accurate?", cex=7, col = "cornflowerblue")
showtext::showtext_end()
dev.off()

# Frame 2 with text
png("6_visualize/tmp_test2/frame_A2_intro.png", height = 400, width = 800, type = plot_type)
par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
showtext::showtext_begin() # begin using google fonts
plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
text(x=0.5, y=0.8, "Forecasts are generally more", cex=3)
text(x=0.5, y=0.5, "accurate the closer you are to the", cex=3)
text(x=0.5, y=0.2, "date you are trying to predict.", cex=3)
showtext::showtext_end()
dev.off()

# Frame 3 with text & graph of how to interpret
png("6_visualize/tmp_test2/frame_A3_intro.png", height = 400, width = 800, type = plot_type)
par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
showtext::showtext_begin() # begin using google fonts
plot(c(0,1), c(0,1), type = 'n', axes=FALSE, xlab="", ylab="")
text(x=0.5, y=0.9, "Let's see this concept in action.", cex=3.5)
# Add lines for illustration
x_vals <- seq(0.3, 0.7, by=0.02)
y_vals <- seq(0.3, 0.7, by=0.02)
x_vals_poly <- c(x_vals[6:16], rev(x_vals[6:16]))
y_vals_poly <- c(y_vals[6:16], rev(c(0.4, seq(0.47, 0.7, length.out=7), 0.68, 0.65, 0.6)))
points(x_vals, y_vals, lwd = 2, col = "grey", lty = "dotted", type="l")
polygon(x_vals_poly, y_vals_poly, col = "#00868B40", border = "#00868B59")
text(x=0.7,y=0.7, "reality", cex=1, pos=4)
text(x=0.45,y=0.6, "our best guess", cex=1, pos=2)
text(x=0.67,y=0.5, "difference between guess and reality", cex=1, pos=1)
arrows(x0=0.54,x1=0.54,y0=0.54,y1=0.69, length = 0.05, code=3, col="#00868B")
text(x=0.5, y=0.1, "Watch as predictions start to match reality through time ...", cex=1.8)
showtext::showtext_end()
dev.off()

# Create a frame for all except dateTime & Stage_Observed
# Also sort them because 0 and 0.5 days out are always first
cols_to_plot <- sort(head(tail(colnames(data_to_plot), -1), -1), decreasing = TRUE)

for(i in cols_to_plot) {
  
  png(sprintf("6_visualize/tmp_test2/frame_%s.png", i), height = 400, width = 800, type = plot_type)
  
  par(family = 'abel') # install font with sysfonts::font_add_google('Abel','abel')
  showtext::showtext_begin() # begin using google fonts
  
  # Setup base plot with observed values in background
  x_range <- range(data_to_plot[["dateTime"]])
  y_range <- range(as.matrix(data_to_plot[cols_to_plot]), na.rm = TRUE)
  plot(x_range, y_range, type = 'n', ylab = "River Height", xlab = "")
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
  
  title(sprintf("River Forecast %s days in advance", tail(unlist(strsplit(i, "Pred_Stage_")), 1)), cex=2)
  
  # close off google fonts
  showtext::showtext_end()
  
  dev.off()
}

##### Create initial video
out_file <- "6_visualize/video_draft.mp4"
png_frames_all <- list.files('6_visualize/tmp_test2', full.names = TRUE)
png_frames <- c(png_frames_all[1:3], rev(png_frames_all[-1:-3])) # Reverse because it is reading in wrong order
file_name_df <- tibble(origName = png_frames,
                       countFormatted = dataRetrieval::zeroPad(1:length(png_frames), padTo = 3),
                       newName = file.path("6_visualize/tmp_test2", paste0("frame_", countFormatted, ".png")))
file.rename(from = file_name_df$origName, to = file_name_df$newName)
shell_command <- sprintf(
  "ffmpeg -y -framerate %s -i 6_visualize/tmp_test2/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
  1/1.3, 15, out_file)
system(shell_command)
file.rename(from = file_name_df$newName, to = file_name_df$origName)

# Now slow down intro by cutting, slowing down, then putting back together
# Needs to be in specific format

writeLines(sprintf("file %s", c("video_intro_slow.mp4", "video_diagram_slow.mp4", "video_main.mp4")), "6_visualize/videosToMerge.txt")
system(sprintf("ffmpeg -ss 0 -i %s -c copy -t 1 %s", out_file, "6_visualize/video_intro.mp4"))
system(sprintf("ffmpeg -ss 1 -i %s -c copy -t 3 %s", out_file, "6_visualize/video_diagram.mp4"))
system(sprintf("ffmpeg -ss 4 -i %s -c copy -t 15 %s", out_file, "6_visualize/video_main.mp4"))
system(sprintf('ffmpeg -i %s -filter:v "setpts=2.0*PTS" %s', "6_visualize/video_intro.mp4", "6_visualize/video_intro_slow.mp4"))
system(sprintf('ffmpeg -i %s -filter:v "setpts=6.0*PTS" %s', "6_visualize/video_diagram.mp4", "6_visualize/video_diagram_slow.mp4"))
system(sprintf('ffmpeg -f concat -i 6_visualize/videosToMerge.txt -c copy %s', "6_visualize/video_final.mp4"))

