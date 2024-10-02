cat(file = stderr(), "Shiny_Noise.R", "\n")


#-------------------------------------------------------------------------------------------#----------------------------------------------------------------------------------------
noise_inflection <- function(session, input, output, params){
  cat(file = stderr(), "Function - noise_inflection...", "\n")
  showModal(modalDialog("Applying noise parameters...", footer = NULL))
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "remove noise precursor...", "\n")
    bg_inflection <- callr::r_bg(func = noise_inflection_bg, args = list("precursor_start", params), stderr = str_c(params$error_path, "//error_inflection.txt"), supervise = TRUE)
    bg_inflection$wait()
    print_stderr("error_inflection.txt")
  } 
  
  #reload params, new data added
  params <<- param_load_from_database()
  
  cat(file = stderr(), "Function - noise_inflection...end", "\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
noise_inflection_bg <- function(table_name, params){
  cat(file = stderr(), "Function - noise_inflection_bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  
  vec <- unlist(df, use.names = FALSE)
  vec <- vec[vec > 1]
  vec <- sort(vec[!is.na(vec)], decreasing = TRUE)
  vec <- log(vec, 2)
  
  df <- data.frame(vec)
  df$ID <- seq.int(nrow(df))
  
  df2 <- df[seq(1, nrow(df), 100),]
  df2$ID <- seq.int(nrow(df2))
  rownames(df2) <- NULL
  
  df_row <- nrow(df2)
  for (i in (1:100)) {
    m <- (df2$vec[df_row] - df2$vec[df_row-20]) / (df2$ID[df_row] - df2$ID[df_row - 20]) *1000
    df_row <- df_row - 10
    noise <- df2$vec[df_row]
    if (m > -4) {
      cat(file = stderr(), stringr::str_c("m --> ", m, "   noise --> ", noise), "\n")
      break
      }
  }
  
  params$noise_inflection <- round(2^noise, digits = 2)

  # count data points below inflection 
  params$noise_count <- length(which(df$vec < noise)) 
  params$noise_total <- nrow(df)
  cat(file = stderr(), stringr::str_c("noise data points to be removed = ", params$noise_count, " out of ", nrow(df)), "\n")
  
  
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  ggplot2::ggplot(df2, ggplot2::aes(x = ID, y = vec)) +
    ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(xintercept = df_row) +
    ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                    'Count', y = "Intensity") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  ggplot2::ggsave(stringr::str_c(params$qc_path, "Inflection_Point.png"), width = 8, height = 6)
  
  
  cat(file = stderr(), stringr::str_c("noise_inflection_bg... end"), "\n")
  return()
}

#----------------------------------------------------------------------------------------
backup_noise_inflection_bg <- function(table_name, params){
  cat(file = stderr(), "Function - noise_inflection_bg...", "\n")
  
  library(inflection)
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)

  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]

  vec <- unlist(df, use.names = FALSE)
  vec <- vec[vec > 1]
  vec <- sort(vec[!is.na(vec)], decreasing = TRUE)
  vec <- log(vec, 2)
  
  df <- data.frame(vec)
  df$ID <- seq.int(nrow(df))
  
  cc <- check_curve(df$ID, df$vec)
  cat(file = stderr(), stringr::str_c("Full inflection, check_curve = ", cc$ctype), "\n")
  cat(file = stderr(), stringr::str_c("Full inflection, index = ", cc$index), "\n")
  
  ipede = ede(df$ID, df$vec, cc$index)
  cat(file = stderr(), stringr::str_c("full ede inflection = ", ipede[3]), "\n")
  cat(file = stderr(), stringr::str_c("full ede inflection value = ", 2^df$vec[df$ID == floor(ipede[3])]), "\n")
  
  #   params$noise_inflection <- round(2^df$vec[df$ID == floor(ipede[3])], digits = 2)
  
  df2 <- df[ipede[3]:(nrow(df)),]
  #   df2 = df2[(nrow(df2)/):(nrow(df2)),]
  
  cc2 <- check_curve(df2$ID, df2$vec)
  cat(file = stderr(), stringr::str_c("inflection, check_curve = ", cc2$ctype), "\n")
  cat(file = stderr(), stringr::str_c("inflection, index = ", cc2$index), "\n")
  
  ipede2 = ede(df2$ID, df2$vec, cc2$index)
  cat(file = stderr(), stringr::str_c("ipede2 = ", ipede2[3]), "\n")
  
  # test inflection point, if above threshold need to calc again
  test <- 0
  if(!is.na(ipede2[3])) {
    test <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)
    cat(file = stderr(), stringr::str_c("noise test = ", test), "\n")
  }
  
  if (is.na(ipede2[3]) | test > 50) {
    for (i in (1:10)){
      df2 = df2[(nrow(df2)/2):(nrow(df2)),]
      cc2 <- check_curve(df2$ID, df2$vec)
      cat(file = stderr(), stringr::str_c("rerun ", i, "  inflection, check_curve = ", cc2$ctype), "\n")
      cat(file = stderr(), stringr::str_c("inflection, index = ", cc2$index), "\n")
      
      ipede2 = ede(df2$ID, df2$vec, cc2$index)
      cat(file = stderr(), stringr::str_c("ipede2 = ", ipede2[3]), "\n")
      if (!is.na(ipede2[3])) { 
        test <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)
        if (test < 50 ) {break} 
        } 
    }
  }
    
  cat(file = stderr(), stringr::str_c("ede inflection = ", ipede2[3]), "\n")
  cat(file = stderr(), stringr::str_c("ede inflection value = ", 2^df2$vec[df2$ID == floor(ipede2[3])]), "\n")
  
  params$noise_inflection <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)
  
  # count data points below inflection 
  params$noise_count = nrow(df) - floor(ipede2[3])
  params$noise_total = nrow(df)
  cat(file = stderr(), stringr::str_c("noise data points to be removed = ", params$noise_count, " out of ", nrow(df)), "\n")
  
  plotdf <- df[seq(1, nrow(df), 10),]
  
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  ggplot2::ggplot(plotdf, ggplot2::aes(x = ID, y = vec)) +
    ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(xintercept = ipede2[3]) +
    ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                    'Count', y = "Intensity") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  ggplot2::ggsave(stringr::str_c(params$qc_path, "Inflection_Point.png"), width = 8, height = 6)
  
  
  cat(file = stderr(), stringr::str_c("noise_inflection_bg... end"), "\n")
  return()
}



#-------------------------------------------------------------------------------------------#----------------------------------------------------------------------------------------
noise_remove <- function(session, input, output, params){
  cat(file = stderr(), "Function - noise_remove...", "\n")
  showModal(modalDialog("Removing noise...", footer = NULL))
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "remove noise precursor...", "\n")
    bg_noise <- callr::r_bg(func = noise_remove_bg, args = list("precursor_start", "precursor_noise", params), stderr = str_c(params$error_path, "//error_noise.txt"), supervise = TRUE)
    bg_noise$wait()
    print_stderr("error_noise.txt")
  } 
  
  #reload params, new data added
  params <<- param_load_from_database()
  
  #create histogram
  filter_histogram_plot(sesion, input, output, params, "precursor_noise", "Precursor_NoiseFiltered_Histogram")
  
  cat(file = stderr(), "Function - noise_remove...end", "\n")
  removeModal()
}

#----------------------------------------------------------------------------------------
noise_remove_bg <- function(table_name, new_table_name, params){
  cat(file = stderr(), "Function - noise_remove_bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  
  cat(file = stderr(), stringr::str_c("noise type -> ", params$noise_type), "\n")
  
  if (params$noise_type == "dynamic") {
    noise_baseline <- params$noise_inflection
  } else if (params$noise_type == "fixed")  {
    noise_baseline <- params$noise_baseline_value
  } else{
    noise_baseline <- 0
  }
  
  df[(ncol(df) - params$sample_number + 1):ncol(df)][df[(ncol(df) - params$sample_number + 1):ncol(df)] < noise_baseline] <- NA
  
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)

  cat(file = stderr(), stringr::str_c("noise_remove_bg... end"), "\n")
  return()
}
