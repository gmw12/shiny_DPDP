cat(file = stderr(), "Shiny_Plots.R", "\n")

#---------------------------------------------------------------------
create_plot <- function(session, input, output, params, plot_number) {
  cat(file = stderr(), "Function create_plot1...", "\n")
  
  showModal(modalDialog("Create_plot1...", footer = NULL))  
  source("Shiny_File.R")
  source("Shiny_Interactive.R")
  
  #confirm data exists in database
  data_name <- stringr::str_c("protein_", input$stats_norm_type)
  if (data_name %in% list_tables()) {
    cat(file = stderr(), stringr::str_c(data_name, " is in database"), "\n") 
    
    #load data
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
    df <- RSQLite::dbReadTable(conn, data_name)
    design <- RSQLite::dbReadTable(conn, "design")
    stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
    RSQLite::dbDisconnect(conn)
    
    #reduce to samples
    df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
      
    comp_string <- input[[str_c("stats_plot_comp", plot_number)]]
    
    cat(file = stderr(), "Function create_plot1...1" , "\n")
    for (i in 1:length(comp_string)) {
      if (comp_string[i] != params$comp_spqc) {
        comp_number <- which(stats_comp$Name == comp_string[i])
        if (i == 1) {
          comp_cols <- c(stats_comp$N_loc[comp_number], stats_comp$D_loc[comp_number] )
        }else{
          comp_cols <- c(comp_cols, stats_comp$N_loc[comp_number], stats_comp$D_loc[comp_number] )
        }
      }else{
        if (i == 1) {
          comp_cols <- c(stats_comp$SPQC_loc[1])
        }else{
          comp_cols <- c(comp_cols, stats_comp$SPQC_loc[1])
        }
      }
    }
    
    cat(file = stderr(), "Function create_plot1...2" , "\n")

    #convert to string to list of cols    
    comp_cols <- strsplit(comp_cols, ",") |> unlist() |> as.numeric()
    
    
    cat(file = stderr(), "Function create_plot1...3" , "\n")
    comp_cols <- sort(unique(unlist(comp_cols)), decreasing = FALSE)
    df <- df[,comp_cols]
    
    #from protein normalization there will be standard deviation of 0 for the normalized protein - this will crash some of the pca/cluster/heatmap calcs
    df <- df[apply(df, 1, var) != 0, ]
    
    namex <- design$Label[comp_cols]
    color_list <- design$colorlist[comp_cols]
    if (params$primary_group == "Primary") {
      groupx <- design$PrimaryGroup[comp_cols]
    }else{
      groupx <- design$Group[comp_cols]
    }
    
    cat(file = stderr(), "Function create_plot1...4" , "\n")
    
    if (input$plot_type1 == "Bar") {
      interactive_barplot(session, input, output, df, namex, color_list, "stats_barplot", input$stats_plot_comp, plot_number)   
    }
    
    if (input$plot_type1 == "Box") {
      interactive_boxplot(session, input, output, df, namex, color_list, input$stats_plot_comp, plot_number)  
    }

    if (input$plot_type1 == "PCA_2D") {
        interactive_pca2d(session, input, output, df, namex, color_list, groupx, input$stats_plot_comp, plot_number)  
    }   
    
    if (input$plot_type1 == "PCA_3D") {
      interactive_pca3d(session, input, output, df, namex, color_list, groupx, input$stats_plot_comp, plot_number)  
    }    
    
    if (input$plot_type1 == "Cluster") {
      interactive_cluster(session, input, output, df, namex, input$stats_plot_comp, plot_number)  
    }    
    
    if (input$plot_type1 == "Heatmap") {
      interactive_heatmap(session, input, output, df, namex, groupx, input$stats_plot_comp, params, plot_number)  
    }    
    
    
    removeModal()
    cat(file = stderr(), "Function create_plot1...end" , "\n")
  }
}







#Bar plot-------------------------------------------------
bar_plot <- function(table_name, plot_title, plot_dir, params) {
  cat(file = stderr(), "Function bar_plot", "\n")
  
  source("Shiny_Plots.R")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- df |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  
  bar_plot2(table_name, plot_title, plot_dir, params, df)
  
  cat(file = stderr(), "Function bar_plot...end", "\n")
  return("done")
}


#Bar plot2-------------------------------------------------
bar_plot2 <- function(table_name, plot_title, plot_dir, params, df) {
  cat(file = stderr(), "Function bar_plot2", "\n")
  
  conn2 <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  design <- RSQLite::dbReadTable(conn2, "design")
  RSQLite::dbDisconnect(conn2)
  
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
  ggplot2::ggsave(file_name, width = 5, height = 5)
  cat(file = stderr(), "Function bar_plot2...end", "\n")
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
  colnames(df) <- design$Label
  
  df <- df |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
  
  png(filename = stringr::str_c(plot_dir, plot_title, "_boxplot.png"), width = 400, height = 250)
  data_box <- log2(df)
  data_box[data_box == -Inf ] <- NA
  graphics::boxplot(data_box, 
          col = design$colorlist, 
          notch = TRUE, 
          boxwex = 0.8,
          #ylab = design$Label,
          main = c(plot_title),
          axes = TRUE,
          horizontal = TRUE,
          las = 1,
          graphics::par(mar = c(2,8,4,1))) #bottom, left, top, right
  dev.off()
  cat(file = stderr(), "Function box_plot...end", "\n")
}


#Histogram for total intensity-------------------------------------------------
histogram_plot <- function(table_name, plottitle, params)
{
  start <- Sys.time()
  cat(file = stderr(), "Function histogram_plot...", "\n")
  library(grid)
  grDevices::pdf(NULL)

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  title <- as.character(params$file_prefix)
  df_log <- as.matrix(log2(df))
  intensity_cutoff <- log2(as.numeric(params$intensity_cutoff))
  
  data_dist <- as.vector(t(df_log))
  data_dist <- data_dist[!is.na(data_dist)]
  data_dist <- data_dist[data_dist > 0]
  data_dist <- data.frame(data_dist)
  data_dist$bin <- dplyr::ntile(data_dist, 100)  

  bottomX_max <- max(data_dist[data_dist$bin == params$bottom_x,]$data_dist)
  
  x_mean <- mean(df_log, na.rm = TRUE)
  x_stdev <- sd(df_log, na.rm = TRUE)
  
  params$intensity_mean <- x_mean
  params$intensity_sd <- x_stdev
  
  if (params$custom_intensity_cutoff) {
    cat(file = stderr(), "using custom intensity cutoff...", "\n")
    intensity_cutoff <- x_mean + (as.numeric(params$intensity_cutoff_sd) * x_stdev)
    params$intensity_cutoff <- trunc(2^intensity_cutoff)
    cat(file = stderr(), stringr::str_c("new intensity_cuttoff = ", intensity_cutoff), "\n")
  }

  df_gather <- tidyr::gather(df)
  df_gather <- subset(df_gather, df_gather$value > 0)
  df_gather$value <- log2(df_gather$value)

  #create impute stats
  test_alignment <- function(x) {
    missing <- sum(is.na(x))/length(x) * 100
    misaligned_count <- 0
    if (missing > params$misaligned_cutoff && missing < 100) {
      if (mean(x, na.rm = TRUE) >= params$intensity_cutoff) {
        misaligned_count <- sum(x > 0, na.rm = TRUE)
      }
    }
    return(misaligned_count)
  }
  
  total_na <- list()
  total_misaligned <- list()
  for (i in 1:nrow(df_groups)) {
    temp_df <- df[df_groups$start[i]:df_groups$end[i]] 
    count_na <- sum(is.na(temp_df))
    temp_df$test <- apply(temp_df, 1, test_alignment )
    count_miss <- sum(temp_df$test)
    total_misaligned <- c(total_misaligned, count_miss) 
    total_na <- c(total_na, count_na)
  } 
  
  params$total_na <- toString(total_na)
  params$total_misaligned <- toString(total_misaligned)
 
  cat(file = stderr(), stringr::str_c("na - > ", params$total_na, "   ma -> ", params$total_misaligned), "\n")
  
  #save params to database
  RSQLite::dbWriteTable(conn, "parameters", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  my_legend1 <- grid::grid.text(stringr::str_c("Misaligned Intensity Cutoff:", round(intensity_cutoff, digits = 1), " / ", round(params$intensity_cutoff, digits = 1)), x = .80, y = .95, gp = grid::gpar(col = "green4", fontsize = 10))
  my_legend2 <- grid::grid.text(stringr::str_c("Mean: ", round(x_mean, digits = 1), " / ", (trunc((2^x_mean),0))), x = .80, y = .90, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend8 <- grid::grid.text(stringr::str_c("Stdev: ", round(x_stdev, digits = 1), " / ", (trunc((2^x_stdev),0))), x = .80, y = .85, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend3 <- grid::grid.text(stringr::str_c("Mean + Stdev: ", round((x_mean + x_stdev),digits = 1), " / ",  (trunc((2^(x_mean + x_stdev) ),0))  ), x = .80, y = .80, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend4 <- grid::grid.text(stringr::str_c("Mean - Stdev: ", round((x_mean - x_stdev),digits = 1), " / ",  (trunc((2^(x_mean - x_stdev) ),0))  ), x = .80, y = .75, gp = grid::gpar(col = "black", fontsize = 10))
  my_legend5 <- grid::grid.text(stringr::str_c("Bottom", params$bottom_x, ": ", round(bottomX_max, digits = 1), " / ", round((2^bottomX_max),digits = 1)), x = .80, y = .70, gp = grid::gpar(col = "coral", fontsize = 10))
  
  plot_dir <- params$qc_path

    ggplot2::ggplot(df_gather, ggplot2::aes(value)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position = "top") + 
      ggplot2::geom_histogram(bins = 100, fill = "blue") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = intensity_cutoff), color = 'green4') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x_mean)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = (x_mean + x_stdev))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = (x_mean - x_stdev))) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = bottomX_max), color = 'coral') +
      ggplot2::ggtitle(plottitle) +
      ggplot2::xlab("log intensity") +
      ggplot2::ylab("Count") +
      ggplot2::annotation_custom(my_legend1) + 
      ggplot2::annotation_custom(my_legend2) +
      ggplot2::annotation_custom(my_legend8) +
      ggplot2::annotation_custom(my_legend3) +
      ggplot2::annotation_custom(my_legend4) +
      ggplot2::annotation_custom(my_legend5) 
    ggplot2::ggsave(stringr::str_c(plot_dir, plottitle, ".png"), width = 8, height = 8)


  
  cat(file = stderr(), stringr::str_c("histogram plot function complete in --> ", Sys.time() - start), "\n")
}



#Bar plot-------------------------------------------------
missing_bar_plot <- function(params) {
  cat(file = stderr(), "Function missing_bar_plot", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "missing_values")
  RSQLite::dbDisconnect(conn)
  
  ggplot2::ggplot(df) +
  ggplot2::geom_bar(ggplot2::aes(x = key, y = num.missing), stat = 'identity') +
  ggplot2::labs(x = 'variable', y = "number of missing values", title = 'Number of missing values') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(stringr::str_c(params$qc_path, "missing_bar_plot.png"), width = 8, height = 8)
  
  cat(file = stderr(), "Function missing_bar_plot...end", "\n")
  return("done")
}

#Bar plot-------------------------------------------------
missing_percent_plot <- function(params) {
  cat(file = stderr(), "Function missing_percent_plot", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "missing_values_plots")
  RSQLite::dbDisconnect(conn)
  
  
  levels <- (df  |> dplyr::filter(isna == T) |> dplyr::arrange(dplyr::desc(pct)))$key
  
  ggplot2::ggplot(df) +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(key, dplyr::desc(pct)), 
               y = pct, fill = as.logical(isna)), 
           stat = 'identity', alpha = 0.8) +
  ggplot2::scale_x_discrete(limits = levels) +
  ggplot2::scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")
  ggplot2::ggsave(stringr::str_c(params$qc_path, "missing_percent_plot.png"), width = 6, height = 8)
  
  cat(file = stderr(), "Function missing_percent_plot...end", "\n")
  return("done")
}



#-----------------------------------------------------------------------------------------
cv_grouped_plot_bg <- function(params) {
  cat(file = stderr(), stringr::str_c("function cv_grouped_plot_bg...."), "\n")
  
  file_name <- stringr::str_c(params$qc_path, "CV_barplot.png")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "summary_cv")
  RSQLite::dbDisconnect(conn)
  
  df <- setNames(data.frame(t(df[,-1])), df[,1])
  df$Norm <- row.names(df)
  row.names(df) <- NULL
  data_plot <- df |> tidyr::pivot_longer(-Norm, names_to = "Sample", values_to = "CV")
  
  # Grouped
  ggplot2::ggplot(data_plot, ggplot2::aes(fill = Sample, y = CV, x = Norm)) + 
    ggplot2::geom_bar(position = "dodge", stat = "identity")
  ggplot2::ggsave(file_name, width = 8, height = 4)
  
  cat(file = stderr(), stringr::str_c("function cv_grouped_plot_bg....end"), "\n")
}





































# #Bar plot-------------------------------------------------
# bar_plot <- function(table_name, plot_title, plot_dir, params) {
#   cat(file = stderr(), "Function bar_plot", "\n")
#   
#   conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
#   design <- RSQLite::dbReadTable(conn, "design")
#   df <- RSQLite::dbReadTable(conn, table_name)
#   RSQLite::dbDisconnect(conn)
#   
#   df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
#   df <- df |>  dplyr::mutate(across(!where(is.numeric), as.numeric))
#   
#   namex <- design$Label
#   datay <- colSums(df, na.rm = TRUE)
#   df2 <- data.frame(namex)
#   df2$Total_Intensity <- datay
#   colnames(df2) <- c("Sample", "Total_Intensity")
#   df2$Sample <- factor(df2$Sample, levels = df2$Sample)
#   file_name <- stringr::str_c(plot_dir, plot_title, "_barplot.png")
#   ymax <- max(datay)
#   ggplot2::ggplot(data = df2, ggplot2::aes(x = Sample, y = Total_Intensity)) +
#     ggplot2::geom_bar(stat = "identity", fill = design$colorlist) + ggplot2::theme_classic() + 
#     ggplot2::ggtitle(plot_title) + 
#     ggplot2::xlab(NULL) +
#     ggplot2::ylab(NULL) +
#     #scale_y_discrete(labels = NULL) +
#     ggplot2::coord_cartesian(ylim = NULL, expand = TRUE) +
#     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
#                    axis.text.x = ggplot2::element_text(size = 5, angle = 90,  color = "black"),
#                    axis.text.y = ggplot2::element_text(size = 5,  color = "black"),
#     ) 
#   ggplot2::ggsave(file_name, width = 5, height = 5)
#   cat(file = stderr(), "Function bar_plot...end", "\n")
#   return("done")
# }