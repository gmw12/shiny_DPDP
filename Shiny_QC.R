cat(file = stderr(), "Shiny_QC.R", "\n")


#----------------------------------------------------------------------------------------- 
#------------------------------------------------------------------------------------------------
qc_stats <- function(session, input, output, params){
  cat(file = stderr(), "Function - qc_stats...", "\n")
  showModal(modalDialog("QC Stats...", footer = NULL))
  
  #calc cv stats
  bg_qc <- callr::r_bg(func = qc_stats_bg, args = list(params), stderr = str_c(params$error_path, "//error_qc.txt"), supervise = TRUE)
  bg_qc$wait()
  print_stderr("error_qc.txt")

  #save grouped plot of average cv's
  bg_cv_grouped_plot <- callr::r_bg(func = cv_grouped_plot_bg, args = list(params), stderr = stringr::str_c(params$error_path, "//cv_plot.txt"), supervise = TRUE)
  bg_cv_grouped_plot$wait()
  print_stderr("cv_plot.txt")
  
  #Norm Comparison
  bg_norm_comp_plot <- callr::r_bg(func = norm_comp_plot_bg, args = list(params), stderr = stringr::str_c(params$error_path, "//norm_comp_plot.txt"), supervise = TRUE)
  bg_norm_comp_plot$wait()
  print_stderr("norm_comp_plot.txt")
  
  #ADH barplot
  
  
  #force update of widget
  qc_norm_types <- as.list(strsplit(params$norm_type, ",")[[1]])
  updateSelectInput(session, "qc_norm_type", choices = qc_norm_types)
  updateSelectInput(session, "spike_norm_type", choices = qc_norm_types)
  updateSelectInput(session, "stats_norm_type", choices = qc_norm_types)
  update_stat_choices(session, input, output)
  
  removeModal()
  cat(file = stderr(), "Function - qc_stats...end", "\n")
}

#------------------------------------------------------------------------------------------------

#collect QC statistics
qc_stats_bg <- function(params){
  cat(file = stderr(), stringr::str_c("function qc_stats_bg..."), "\n")
  
  source("Shiny_Stats_Functions.R")

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
  
  #create starter df for summary CV's
  summary_cv <- data.frame(df_groups$Group)
  colnames(summary_cv)[1] <- "Group"

  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  for (norm in norm_type) {
    cat(file = stderr(), stringr::str_c("qc_stats...", norm), "\n")
    
    norm <- stringr::str_replace_all(norm, " ", "")
    
    if (params$data_output == "Protein") {
      table_name <- stringr::str_c('protein_impute_', norm)
    } else if (params$data_output == "Peptide") {
      table_name <- stringr::str_c('peptide_impute_', norm)
    }
    
    df <- RSQLite::dbReadTable(conn, table_name)
    
    #create starter df for final data CV's
    stat_df <- df[1:1]
    
    #reduce to data only
    df_data <- df[,(ncol(df) - params$sample_number + 1):ncol(df)]
    
    #generate %CV's for each group
    for (i in 1:nrow(df_groups)) 
    {
      stat_df[ , stringr::str_c(df_groups$Group[i], "_CV")] <- percentCV_gw(df_data[df_groups$start[i]:df_groups$end[i]])
    } 

    avg_cv <- colMeans(stat_df[,2:ncol(stat_df)], na.rm = TRUE)
    summary_cv <- cbind(summary_cv, avg_cv)
    colnames(summary_cv)[ncol(summary_cv)] <- norm
    
    #combine df and cv data
    df <- cbind(df, stat_df[,2:ncol(stat_df)])
    
    #save CV data to db
    RSQLite::dbWriteTable(conn, stringr::str_c(table_name, "_cv"), stat_df, overwrite = TRUE)
    RSQLite::dbWriteTable(conn, stringr::str_c(table_name, "_final"), df, overwrite = TRUE)
  }

  RSQLite::dbWriteTable(conn, "summary_cv", summary_cv, overwrite = TRUE)
  
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), stringr::str_c("function qc_stats_bg..."), "\n")
}



#----------------------------------------------------------------------------------------- 
#------------------------------------------------------------------------------------------------

#create bar plots for all norm types
norm_comp_plot_bg <- function(params){
  cat(file = stderr(), stringr::str_c("function norm_comp_plot_bg..."), "\n")
  source("Shiny_Plots.R")
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  for (norm in norm_type) {
    norm <- stringr::str_replace_all(norm, " ", "")
    
    if (params$data_output == "Protein") {
      table_name <- stringr::str_c('protein_impute_', norm)
    } else if (params$data_output == "Peptide") {
      table_name <- stringr::str_c('peptide_impute_', norm)
    }
    
    bar_plot(table_name, table_name, params$qc_path, params) 
  }
  
  cat(file = stderr(), stringr::str_c("function norm_comp_plot_bg...end"), "\n")
}

#------------------------------------------------------------------------------------------------

#create bar plots for specific protein
qc_protein_plots <- function(session, input, output, params){
  cat(file = stderr(), stringr::str_c("function qc_protein_plots..."), "\n")
  
  qc_norm_type <- stringr::str_replace_all(input$qc_norm_type, " ", "") 
  qc_accession <- input$qc_plot_accession
  
  cat(file = stderr(), stringr::str_c(qc_norm_type, "   ", qc_accession), "\n")
  
  if (params$data_output == "Protein") {
    table_name <- stringr::str_c('protein_impute_', qc_norm_type)
  } else if (params$data_output == "Peptide") {
    table_name <- stringr::str_c('peptide_impute_', qc_norm_type)
  }

  plot_title <- str_c(qc_accession, '_', qc_norm_type)
  
  bg_qcprotein <- callr::r_bg(func = qc_protein_plots_bg, args = list(table_name, plot_title, qc_accession, params$qc_path, params), stderr = str_c(params$error_path, "//error_qcprotein.txt"), supervise = TRUE)
  bg_qcprotein$wait()
  print_stderr("error_qcprotein.txt")
  
  plot_path <- stringr::str_c(params$qc_path, plot_title, "_barplot.png")
  
  output$qc_protein_barplot <- renderImage({
    list(src = plot_path, contentType = 'image/png', width = 480, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  
  cat(file = stderr(), stringr::str_c("function qc_protein_plots...end"), "\n")
}
#------------------------------------------------------------------------------------------------

#create bar plots for specific protein
qc_protein_plots_bg <- function(table_name, plot_title, qc_accession, plot_dir, params){
  cat(file = stderr(), stringr::str_c("function qc_protein_plots_bg..."), "\n")
  
  source("Shiny_Plots.R")
  source("Shiny_File.R")
    
  #save(table_name, file="z1"); save(plot_title, file="z2"); save(plot_dir, file="z3"); save(qc_accession, file="z4")
  #  load(file="z1"); load(file="z2"); load(file="z3"); load(file="z4")
  
  df <- read_table_try(table_name, params)

  df <- df[df$Accession %in% qc_accession,]
  
  bar_plot2(table_name, plot_title, plot_title, params$qc_path, params, df)

  cat(file = stderr(), stringr::str_c("function qc_protein_plots_bg...end"), "\n")
}

#-----------------------------------------------------------------------------------------
#create bar plots for spike protein
qc_spike_plots <- function(session, input, output, params){
  cat(file = stderr(), stringr::str_c("function qc_spike_plots..."), "\n")
  
  qc_norm_type <- stringr::str_replace_all(input$spike_norm_type, " ", "") 
  qc_accession <- input$spike_plot_accession
  
  if (params$data_output == "Protein") {
    table_name <- stringr::str_c('protein_impute_', qc_norm_type)
  } else if (params$data_output == "Peptide") {
    table_name <- stringr::str_c('peptide_impute_', qc_norm_type)
  }

  plot_title <- str_c('QC_Spike_', qc_norm_type)
  
  cat(file = stderr(), stringr::str_c(qc_norm_type, "   ", qc_accession), "\n")
  
  bg_qcspike <- callr::r_bg(func = qc_spike_plots_bg, args = list(table_name, plot_title, qc_accession, params), stderr = str_c(params$error_path, "//error_qcspike.txt"), supervise = TRUE)
  bg_qcspike$wait()
  print_stderr("error_qcspike.txt")

  plot_path <-  stringr::str_c(params$qc_path, plot_title, "_barplot.png")
  
  output$spike_protein_barplot <- renderImage({
    list(src = plot_path, contentType = 'image/png', width = 800, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  spike_DT <- bg_qcspike$get_result()
  output$qc_spike_table <-  renderRHandsontable(spike_DT)
  
  cat(file = stderr(), stringr::str_c("function qc_spike_plots...end"), "\n")
}


#-----------------------------------------------------------------------------------------
#create bar plots for spike protein
qc_spike_plots_bg <- function(table_name, plot_title, qc_accession, params){
  cat(file = stderr(), stringr::str_c("function qc_spike_plots_bg..."), "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  qc_spike <- df_design |> dplyr::select(contains(c("Label", "Level")))
  colnames(qc_spike)[2] <- "SpikeLevel"
  
  # qc_accession = 'P02662,P02663'
  qc_accession <- gsub(" ", "", qc_accession, fixed = TRUE)
  qc_accession <- unlist(strsplit(as.character(qc_accession), split = ","))
  
  spike_protein <- subset(df, Accession %in% qc_accession)  
  spike_protein <- spike_protein[(ncol(df) - params$sample_number + 1):ncol(df)]
  qc_spike$Intensity <- colSums(spike_protein)
  
  qc_spike_final <- aggregate(qc_spike[,2:3], by = list(Category = qc_spike$SpikeLevel), FUN = mean)
  qc_spike_final$Category <- NULL
  qc_spike_final$SpikeLevel <- as.character(qc_spike_final$SpikeLevel)
  qc_spike_final$PercentofLow <- ""
  
  for (i in 2:nrow(qc_spike_final)) {
    qc_spike_final$PercentofLow[i] <- round((qc_spike_final$Intensity[i]/qc_spike_final$Intensity[1]), 2)
  }
  
  file_name <- stringr::str_c(params$qc_path, plot_title, "_barplot.png")
  ggplot2::ggplot(qc_spike, ggplot2::aes(fill = Label, y = Intensity, x = SpikeLevel)) + 
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = plot_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2))
  ggplot2::ggsave(file_name, width = 8, height = 4)
  
  
  data_spike <- rhandsontable::rhandsontable(qc_spike_final, readOnly = TRUE, rowHeaders = NULL, digits = 0) |> 
    rhandsontable::hot_col(col = 'SpikeLevel', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'PercentofLow', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Intensity', format = "0")
  
  cat(file = stderr(), stringr::str_c("function qc_spike_plots_bg...end"), "\n")
  
  return(data_spike)
}

