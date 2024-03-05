cat(file = stderr(), "Shiny_QC.R", "\n")


#----------------------------------------------------------------------------------------- 
#------------------------------------------------------------------------------------------------
qc_stats <- function(params){
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
      table_name <- stringr::str_c('protein_', norm)
    } else if (params$data_output == "Peptide") {
      table_name <- stringr::str_c('peptide_', norm)
    }
    
    df <- RSQLite::dbReadTable(conn, table_name)
    
    #create starter df for final data CV's
    stat_df <- df[1:1]
    
    #reduce to data only
    df <- df[,(ncol(df) - params$sample_number + 1):ncol(df)]
    
    #generate %CV's for each group
    for (i in 1:nrow(df_groups)) 
    {
      stat_df[ , stringr::str_c(df_groups$Group[i], "_CV")] <- percentCV_gw(df[df_groups$start[i]:df_groups$end[i]])
    } 

    avg_cv <- colMeans(stat_df[,2:ncol(stat_df)], na.rm = TRUE)
    summary_cv <- cbind(summary_cv, avg_cv)
    colnames(summary_cv)[ncol(summary_cv)] <- norm
    
    #save CV data to db
    RSQLite::dbWriteTable(conn, stringr::str_c(table_name, "_cv"), stat_df, overwrite = TRUE)
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
    table_name <- stringr::str_c('protein_', norm)

    bar_plot(table_name, table_name, params$qc_path, params) 
  }
  
  cat(file = stderr(), stringr::str_c("function norm_comp_plot_bg...end"), "\n")
}








