#Creating overlay functions
library(ggplot2)
library(signal)


#makes a table of datapoints using V6 and time (plot_CE function)
create_CE_table <- function(file, hz = 50, filterby = 3, name = NA) {
  plot_CE(file, hz = hz, filterby = filterby, name = name, return = 'df')
}

#combines a bunch of tables (not used in directory function)
combine_CE_traces <- function(...){
  master_df <- bind_rows(...)
  return(master_df)
}


#plots an overlay based on a master dataframe of all traces
plot_CE_overlay <- function(master_df, xlab = 'Time (min)', ylab = 'Counts',
                            xmin = NA, xmax = NA, ymin = NA, ymax = NA) {
  overlay <- ggplot(data = master_df)+
    geom_line(aes(x=time, y=filtered_V6, color = trace)) +
    theme_bw() +
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  return(overlay)
}

#loops through a directory and creates a master dataframe, then plots using plot_CE_overlay
overlay_CE_directory <- function(folder, hz = 50, xlab = 'Time (min)', ylab = 'Counts', filterby = 3,
                                 xmin = NA, xmax = NA, ymin = NA, ymax = NA, return = "plot"){
  wd <- getwd()

  file_directory <- file.path(wd, folder)


  file_list <- list.files(path=file_directory)

  master_table <- create_CE_table(file = file.path(folder, file_list[1]), hz = hz, filterby = filterby)
  
  for (i in 2:length(file_list)){
    temp_table <- create_CE_table(file = file.path(folder, file_list[i]), hz = hz, filterby = filterby)
    master_table <- bind_rows(master_table, temp_table)
  }
  

  overlay <- plot_CE_overlay(master_table, xlab = xlab, ylab = ylab,
                             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  if (return == "plot"){
    #show plot
    return(overlay)
  } else {
    return(master_table)
  }
}

#zooms in on a plot
zoom_plot <- function(plot, xmin = NA, xmax =NA, ymin = NA, ymax = NA){
  zoomed_plot <- plot +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  return(zoomed_plot)
}


#The base plot function
plot_CE <- function(file, hz = 50, xlab = 'Time (min)', ylab = 'Counts', filterby = 3, return = "plot", name = NA,
                    xmin = NA, xmax = NA, ymin = NA, ymax = NA){
  
  
  #import from text file
  df_raw <- read.delim(file, header=FALSE)
  
  #create time list
  time <- ((0:(nrow(df_raw)-1))/hz/60)
  df_raw$time <- time
  
  
  #filter be median, default 3
  filtered_V6 <- medfilt1(df_raw$V6, filterby)
  df_raw$filtered_V6 <- filtered_V6
  
  if (is.na(name)){
    #obtain file name
    trace_name_path <- tools::file_path_sans_ext(file)
    trace_name <- basename(trace_name_path)
  } else {
    trace_name <- name
  }
  
  #add file name to df
  df_raw$trace <- trace_name
  
  #create plot
  plot <- ggplot(data = df_raw) +
    geom_line(aes(x=time, y=filtered_V6)) +
    theme_bw() +
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank()) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  if (return == "plot"){
    #show plot
    return(plot)
  } else {
    return(df_raw)
  }
}


#save as tiff
save_as_tif <- function(object, file = 'image.tiff', w = 7, h = 5, r = 300, u = 'in'){
  tiff(file, units = u, width = 7, height = h, res = r)
  
  show(object)
  
  dev.off()
}
