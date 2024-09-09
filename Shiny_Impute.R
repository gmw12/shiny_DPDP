cat(file = stderr(), "Shiny_Impute.R", "\n")

#--------------------------------------------------------------------------------
impute_apply <- function(session, input, output) {
  cat(file = stderr(), "Function - impute_apply...", "\n")
  showModal(modalDialog("Apply Imputation...", footer = NULL))
  start <- Sys.time()
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  bg_impute_apply <- callr::r_bg(func = impute_apply_bg, args = list(norm_type, params), stderr = str_c(params$error_path, "//error_impute_bg.txt"), supervise = TRUE)
  bg_impute_apply$wait()
  print_stderr("error_impute_bg.txt")


  #create imputed df's for protein and precursor
  source("Shiny_MVA_Functions.R")
  bg_impute_df <- callr::r_bg(func = create_imputed_df, args = list(params), stderr = str_c(params$error_path, "//error_impute_df.txt"), supervise = TRUE)
  bg_impute_df$wait()
  print_stderr("error_impute_df.txt")
  
  
  removeModal()
  cat(file = stderr(), stringr::str_c("Function - impute_apply...end (completed in ", Sys.time() - start, ")") , "\n\n")
}

#----------------------------------------------------------------------------------------------------

impute_apply_bg <- function(norm_type, params) {
  cat(file = stderr(), "Function - impute_apply_bg...", "\n")

  source("Shiny_Impute.R")
  source("Shiny_File.R")

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_random <- RSQLite::dbReadTable(conn, "random")
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
  RSQLite::dbDisconnect(conn)
  
  for (norm in norm_type) {
    cat(file = stderr(), stringr::str_c("impute_apply...", norm), "\n\n")
    norm <- stringr::str_replace_all(norm, " ", "")
    
    table_name <- stringr::str_c("precursor_norm_", norm)
    cat(file = stderr(), stringr::str_c("imputing...", table_name), "\n")
    
    conn1 <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
    df <- RSQLite::dbReadTable(conn1, table_name)
    RSQLite::dbDisconnect(conn1)
    
    bg_name <- stringr::str_c('bg_impute_', norm)
    bg_file <- stringr::str_c(params$error_path, "//error_", bg_name, ".txt")
    assign(bg_name, callr::r_bg(func = impute_apply_bg2, args = list(norm, params, df_random, df_groups, df), stderr = bg_file, supervise = TRUE))
  }

  for (norm in norm_type) {
    norm <- stringr::str_replace_all(norm, " ", "")
    bg_name <- stringr::str_c('bg_impute_', norm)
    bg_impute <- get(bg_name)
    bg_impute$wait()
    print_stderr2(stringr::str_c("error_", bg_name, ".txt"), params)
  }
  
  cat(file = stderr(), "Function - impute_apply_bg...end", "\n")
}


#----------------------------------------------------------------------------------------------------

impute_apply_bg2 <- function(norm, params, df_random, df_groups, df) {
  cat(file = stderr(), "Function - impute_apply_bg2...", "\n")
  
  source('Shiny_Impute_Functions.R')
  source('Shiny_Norm_Functions.R')
  
  if (params$impute_type == "duke") {
    df_impute <- impute_duke(df, df_random, df_groups, params)
    df_impute <- impute_bottomx(df_impute, df_random, params)
    }
  if (params$impute_type == "bottomx") {
    df_impute <- impute_bottomx(df, df_random, params)
    }
  
  #if sltmm then apply tmm
  if (norm == "sltmm") {
    cat(file = stderr(), "sltmm found, apply tmm norm now ...", "\n")
    info_columns <- ncol(df_impute) - params$sample_number
    conn2 <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
    norm_data <- RSQLite::dbReadTable(conn2, "precursor_normdata")
    RSQLite::dbDisconnect(conn2)
    df_impute <- tmm_normalize(norm_data, df_impute, info_columns)
  }
  
  new_table_name <- stringr::str_c('precursor_impute_', norm)
  conn3 <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn3, new_table_name, df_impute, overwrite = TRUE)
  RSQLite::dbDisconnect(conn3)
  
  cat(file = stderr(), "Function - impute_apply_bg2...end", "\n")
}


#----------------------------------------------------------------------------------------------------

old_apply_impute <- function(session, input, output){
  cat(file = stderr(), "apply_impute function...1", "\n")
  
  # save set of random numbers to be used for impute, if reimpute numbers the same
  set.seed(123)
  #get number of missing values
  total_missing <- table(is.na(dpmsr_set$data$normalized$impute))
  cat(file = stderr(), str_c(total_missing[2], " missing values"), "\n")
  dpmsr_set$y$rand_impute <<- runif(total_missing[2]*2, min = -1, max = 1)
  cat(file = stderr(), "created random number dataframe", "\n")
  dpmsr_set$y$rand_count <<- 1
  
  cat(file = stderr(), "apply_impute function...2", "\n")
  ncores <- detectCores()
  cat(file = stderr(), str_c("ncores = ", ncores), "\n")
  if (is.na(ncores) | ncores < 1) {ncores <- 1}
  norm_list <- dpmsr_set$y$norm_list
  dpmsr_set$data$impute <<- mclapply(norm_list, impute_parallel, mc.cores = ncores)
  #need to complete TMM norms after imputation ()
  
  cat(file = stderr(), "apply_impute function...3", "\n")
  #check that the parallel processing went through, if not do it again one at a time
  check_impute_parallel(norm_list)
  
  #check if directlfq norm was run and impute the protein level data
  check_impute_protein_directlfq()
  
  norm_list2 <- dpmsr_set$y$norm_list2
  
  info_columns <- ncol(dpmsr_set$data$impute$impute) - dpmsr_set$y$sample_number
  
  cat(file = stderr(), "apply_impute function...4", "\n")
  if (2 %in% dpmsr_set$y$norm_list2)
  {dpmsr_set$data$impute$tmm <<- tmm_normalize(dpmsr_set$data$impute$impute, "TMM_Norm", info_columns)
  }
  cat(file = stderr(), "apply_impute function...5", "\n")
  if (3 %in% dpmsr_set$y$norm_list2)
  {dpmsr_set$data$impute$sltmm <<- tmm_normalize(dpmsr_set$data$impute$sl, "SLTMM_Norm", info_columns)
  }
  cat(file = stderr(), "apply_impute function...6", "\n")
  if (11 %in% dpmsr_set$y$norm_list2)
  {dpmsr_set$data$impute$protein <<- protein_normalize(dpmsr_set$data$impute$impute, "Protein_Norm", info_columns)
  }
  cat(file = stderr(), "apply_impute function...end", "\n")
}


#--------------------------------------------------------------------------------
impute_parallel <- function(norm_type) {
  if (norm_type == 99) {data_raw_impute <- impute_only(dpmsr_set$data$normalized$impute, "impute")}
  else if (norm_type == 98) {data_raw_impute_bottom <- impute_bottom(dpmsr_set$data$normalized$impute, "impute_test")}
  else if (norm_type == 1) {data_sl_impute <- impute_only(dpmsr_set$data$normalized$sl, "sl")}
  else if (norm_type == 4) {data_quantile_impute <- impute_only(dpmsr_set$data$normalized$quantile, "quantile")}
  else if (norm_type == 5) {data_lr_impute <- impute_only(dpmsr_set$data$normalized$lr, "lr")}
  else if (norm_type == 6) {data_loess_impute <- impute_only(dpmsr_set$data$normalized$loess, "loess")}
  else if (norm_type == 7) {data_vsn_impute <- impute_only(dpmsr_set$data$normalized$vsn, "vsn")}
  else if (norm_type == 8) {data_ti_impute <- impute_only(dpmsr_set$data$normalized$ti, "ti")}
  else if (norm_type == 9) {data_mi_impute <- impute_only(dpmsr_set$data$normalized$mi, "mi")}
  else if (norm_type == 10) {data_ai_impute <- impute_only(dpmsr_set$data$normalized$ai, "ai")}
  else if (norm_type == 13) {data_directlfq_impute <- impute_only(dpmsr_set$data$normalized$directlfq, "directlfq")}
}

#--------------------------------------------------------------------------------------
check_impute_parallel <- function(norm_list){
  for (norm_name in names(norm_list)) {
    if (is.null(dpmsr_set$data$impute[[norm_name]]    )) {
      cat(file = stderr(), str_c("check parallel, apply_impute function...",norm_name), "\n")
      dpmsr_set$data$impute[[norm_name]] <<- impute_only(dpmsr_set$data$normalized[[norm_name]], norm_name  )
    }else{
      cat(file = stderr(), str_c("check parallel, found --> ",norm_name), "\n")
    }
  }
}

#--------------------------------------------------------------------------------------
check_impute_protein_directlfq <- function(){
  
  if (is.null(dpmsr_set$data$directlfq$directlfq_protein)) {
    cat(file = stderr(), str_c("DirectLFQ not run, no protein level impute necesary"), "\n")
  }else{
    df <- impute_only(dpmsr_set$data$directlfq$directlfq_protein, "directlfq_protein")
    dpmsr_set$data$directlfq$directlfq_protein_impute <<- df
  }
}

#--------------------------------------------------------------------------------------
# data_raw_impute <- impute_only(dpmsr_set$data$normalized$impute, "impute")
# norm_name  <- "impute"
# data_out <- dpmsr_set$data$normalized$impute
#data_in <- dpmsr_set$data$normalized$sl
#norm_name <-






