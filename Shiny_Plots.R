cat(file = stderr(), "Shiny_Plots.R", "\n")


#Bar plot-------------------------------------------------
bar_plot <- function(table_name, plot_title, plot_dir, params) {
  cat(file = stderr(), "Function bar_plot", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  design <- RSQLite::dbReadTable(conn, "design")
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- df |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  
  namex <- design$Label
  datay <- colSums(df, na.rm = TRUE)
  df2 <- data.frame(namex)
  df2$Total_Intensity <- datay
  colnames(df2) <- c("Sample", "Total_Intensity")
  df2$Sample <- factor(df2$Sample, levels = df2$Sample)
  file_name <- stringr::str_c(plot_dir, plot_title, "_barplot.png")
  ymax <- max(datay)
  ggplot2::ggplot(data = df2, ggplot2::aes(x = Sample, y = Total_Intensity)) +
    ggplot2::geom_bar(stat = "identity", fill = design$colorlist) + ggplot2::theme_classic() + 
    ggplot2::ggtitle(plot_title) + 
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    #scale_y_discrete(labels = NULL) +
    ggplot2::coord_cartesian(ylim = NULL, expand = TRUE) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
          axis.text.x = ggplot2::element_text(size = 5, angle = 90,  color = "black"),
          axis.text.y = ggplot2::element_text(size = 5,  color = "black"),
    ) 
  ggplot2::ggsave(file_name, width = 5, height = 4)
  return("done")
}

#Box plot-------------------------------------------------
box_plot <- function(table_name, plot_title, plot_dir, params) {
  cat(file = stderr(), "Function box_plot", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  design <- RSQLite::dbReadTable(conn, "design")
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- df |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  
  
  png(filename = stringr::str_c(plot_dir, plot_title, "_boxplot.png"), width = 888, height = 571)
  data_box <- log2(df)
  data_box[data_box == -Inf ] <- NA
  graphics::boxplot(data_box, 
          col = design$colorlist, 
          notch = TRUE, 
          boxwex = 0.8,
          main = c(plot_title),
          axes = TRUE,
          horizontal = TRUE,
          las = 1,
          graphics::par(mar = c(8,10,4,2)))
  dev.off()
}


#Histogram for total intensity-------------------------------------------------
histogram_plot <- function(table_name, plottitle, params)
{
  start <- Sys.time()
  cat(file = stderr(), "Function histogram_plot...", "\n")
  library(grid)
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  design <- RSQLite::dbReadTable(conn, "design")
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  title <- as.character(params$file_prefix)
  df_log <- as.matrix(log2(df))
  intensity_cutoff <- as.numeric(params$intensity_cutoff)
  
  data_dist <- as.vector(t(df_log))
  data_dist <- data_dist[!is.na(data_dist)]
  data_dist <- data_dist[data_dist > 0]
  data_dist <- data.frame(data_dist)
  data_dist$bin <- dplyr::ntile(data_dist, 100)  

  bottom1_max <- max(data_dist[data_dist$bin == 1,]$data_dist)
  bottom2_max <- max(data_dist[data_dist$bin == 2,]$data_dist)
  bottom5_max <- max(data_dist[data_dist$bin == 5,]$data_dist)
  
  x_mean <- mean(df_log, na.rm = TRUE)
  x_stdev <- sd(df_log, na.rm = TRUE)
  
  if (params$intensity_cutoff_sd < 0) {
    new_cutoff <- x_mean - (as.numeric(params$intensity_cutoff_sd) * x_stdev)
  }else{
    new_cutoff <- x_mean + (as.numeric(params$intensity_cutoff_sd) * x_stdev)
  }
  
  params$intensity_mean <- x_mean
  params$intensity_sd <- x_stdev
  
  if (new_cutoff < intensity_cutoff) {intensity_cutoff <- new_cutoff}
  if (intensity_cutoff < log2(100000)) {intensity_cutoff <- log2(100000)}
  
  file_name <- stringr::str_c(params$output_dir, title, "_histogram.png")
  df_gather <- tidyr::gather(df)
  df_gather <- subset(df_gather, df_gather$value > 0)
  df_gather$value <- log2(df_gather$value)
  
  params$intensity_cutoff <- trunc(2^intensity_cutoff)
  
  my_legend1 <- grid::grid.text("Default Minimum Intensity: 5e+06", x = .80, y = .95, gp = grid::gpar(col = "green4", fontsize = 10))
  my_legend2 <- grid::grid.text(stringr::str_c("Mean: ", (trunc((2^x_mean),0))), x = .80, y = .90, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend3 <- grid::grid.text(stringr::str_c("Mean + Stdev: ",  (trunc((2^(x_mean + x_stdev) ),0))  ), x = .80, y = .85, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend4 <- grid::grid.text(stringr::str_c("Mean - Stdev: ",  (trunc((2^(x_mean - x_stdev) ),0))  ), x = .80, y = .80, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend5 <- grid::grid.text(stringr::str_c("Bottom5: ", trunc((2^bottom5_max),0)), x = .80, y = .75, gp = grid::gpar(col = "coral", fontsize = 10))
  my_legend6 <- grid::grid.text(stringr::str_c("Bottom2: ", trunc((2^bottom2_max),0)), x = .80, y = .70, gp = grid::gpar(col = "coral", fontsize = 10))
  my_legend7 <- grid::grid.text(stringr::str_c("Bottom1: ", trunc((2^bottom1_max),0)), x = .80, y = .65, gp = grid::gpar(col = "coral", fontsize = 10))
  
  plot_dir <- params$qc_path

    ggplot2::ggplot(df_gather, ggplot2::aes(value)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position = "top") + 
      ggplot2::geom_histogram(bins = 100, fill = "blue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = log2(5000000)), color = 'green4') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x_mean)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = (x_mean + x_stdev))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = (x_mean - x_stdev))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = bottom5_max), color = 'coral') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = bottom2_max), color = 'coral') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = bottom1_max), color = 'coral') +
      ggplot2::ggtitle(plottitle) +
      ggplot2::annotation_custom(my_legend1) + 
      ggplot2::annotation_custom(my_legend2) +
      ggplot2::annotation_custom(my_legend3) +
      ggplot2::annotation_custom(my_legend4) +
      ggplot2::annotation_custom(my_legend5) +
      ggplot2::annotation_custom(my_legend6) +
      ggplot2::annotation_custom(my_legend7)
    ggplot2::ggsave(stringr::str_c(plot_dir, plottitle, ".png"), width = 8, height = 6)


  
  cat(file = stderr(), stringr::str_c("histogram plot function complete in --> ", Sys.time() - start), "\n")
}



