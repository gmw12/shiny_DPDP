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