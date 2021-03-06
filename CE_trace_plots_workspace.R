
plot_CE <- function(file, hz = 60, xlab = 'Time (min)', ylab = 'Counts', filterby = 3, return = "plot"){
  library(ggplot2)
  library(signal)


  #import from text file
  df_raw <- read.delim(file, header=FALSE)

  #create time list
  time <- ((0:(nrow(df_raw)-1))/hz/60)
  df_raw$time <- time


  #filter be median, default 3
  filtered_V6 <- medfilt1(df_raw$V6, filterby)
  df_raw$filtered_V6 <- filtered_V6
  
  #obtain file name
  trace_name_path <- tools::file_path_sans_ext(file)
  trace_name <- basename(trace_name_path)
 
  #add file name to df
  df_raw$trace <- trace_name

  #create plot
  plot <- ggplot(data = df_raw) +
    geom_line(aes(x=time, y=filtered_V6)) +
    theme_bw() +
    labs(x=xlab, y=ylab)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme(panel.grid = element_blank())
  
  if (return == "plot"){
    #show plot
    return(plot)
  } else {
    return(df_raw)
  }
}

