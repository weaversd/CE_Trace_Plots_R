#Creating overlay functions


create_CE_table <- function(file, hz = 50, filterby = 3, name = NA) {
  plot_CE(file, hz = hz, filterby = filterby, name = name, return = 'df')
}


combine_CE_traces <- function(...){
  master_df <- bind_rows(...)
  return(master_df)
}



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


overlay_CE_directory <- function(folder, hz = 50, xlab = 'Time (min)', ylab = 'Counts', filterby = 3,
                                 xmin = NA, xmax = NA, ymin = NA, ymax = NA){
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
  return(overlay)
}
