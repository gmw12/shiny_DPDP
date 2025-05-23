cat(file = stderr(), "Shiny_Impute.R", "\n")

#--------------------------------------------------------------------------------
impute_apply <- function(session, input, output, db_path) {
  cat(file = stderr(), "Function - impute_apply...", "\n")
  showModal(modalDialog("Apply Imputation...", footer = NULL))
  start <- Sys.time()
  
  params <- get_params(db_path)
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  bg_impute_apply <- callr::r_bg(func = impute_apply_bg, args = list(norm_type, params, db_path), stderr = str_c(params$error_path, "//error_impute_bg.txt"), supervise = TRUE)
  bg_impute_apply$wait()
  print_stderr("error_impute_bg.txt", db_path)


  #create imputed df's for protein and precursor
  source("Shiny_MVA_Functions.R")
  bg_impute_df <- callr::r_bg(func = create_imputed_df, args = list(params, db_path), stderr = str_c(params$error_path, "//error_impute_df.txt"), supervise = TRUE)
  bg_impute_df$wait()
  print_stderr("error_impute_df.txt", db_path)
  
  
  removeModal()
  cat(file = stderr(), stringr::str_c("Function - impute_apply...end (completed in ", Sys.time() - start, ")") , "\n")
}

#----------------------------------------------------------------------------------------------------

impute_apply_bg <- function(norm_type, params, db_path) {
  cat(file = stderr(), "Function - impute_apply_bg...", "\n")

  #save(params, file = "z1"); save(norm_type, file = "z2"); save(db_path, file = "z3")
  #.   load(file = "z1"); load(file = "z2"); load(file = "z3")
  
  source("Shiny_Impute.R")
  source("Shiny_File.R")

  df_random <- read_table_try("random", db_path)
  df_groups <- read_table_try("sample_groups", db_path)
  
  for (norm in norm_type) {
    cat(file = stderr(), stringr::str_c("impute_apply...", norm), "\n")
    norm <- stringr::str_replace_all(norm, " ", "")
    
    table_name <- stringr::str_c("precursor_norm_", norm)
    cat(file = stderr(), stringr::str_c("imputing...", table_name), "\n")
    
    df <- read_table_try(table_name, db_path)

    bg_name <- stringr::str_c('bg_impute_', norm)
    bg_file <- stringr::str_c(params$error_path, "//error_", bg_name, ".txt")
    arglist <- list(norm, params, db_path, df_random, df_groups, df)
    assign(bg_name, callr::r_bg(func = impute_apply_bg2, args = arglist, stderr = bg_file, supervise = TRUE))
  }

  for (norm in norm_type) {
    cat(file = stderr(), stringr::str_c("getting r_bg info for... ", norm), "\n")
    norm <- stringr::str_replace_all(norm, " ", "")
    bg_name <- stringr::str_c('bg_impute_', norm)
    bg_impute <- get(bg_name)
    bg_impute$wait()
    
    bg_impute_list <- bg_impute$get_result()
    new_table_name <- bg_impute_list[[1]]
    df_impute <- bg_impute_list[[2]]
    write_table_try(new_table_name, df_impute, db_path)
    
    print_stderr(stringr::str_c("error_", bg_name, ".txt"), db_path)
    #print_stderr2(stringr::str_c("error_", bg_name, ".txt"), db_path)
  }
  
  cat(file = stderr(), "Function - impute_apply_bg...end", "\n")
}


#----------------------------------------------------------------------------------------------------

impute_apply_bg2 <- function(norm, params, db_path, df_random, df_groups, df) {
  cat(file = stderr(), "Function - impute_apply_bg2...", "\n")
  
  source('Shiny_Impute_Functions.R')
  source('Shiny_Norm_Functions.R')
  source('Shiny_Filter.R')
  
  if (params$impute_type == "duke") {
    df_impute <- impute_duke(df, df_random, df_groups, params, db_path)
    df_impute <- impute_bottomx(df_impute, df_random, params)
    }
  if (params$impute_type == "bottomx") {
    df_impute <- impute_bottomx(df, df_random, params)
    }
  
  #if sltmm then apply tmm
  if (norm == "sltmm") {
    cat(file = stderr(), "sltmm found, apply tmm norm now ...", "\n")
    info_columns <- ncol(df_impute) - params$sample_number
    #need to re-create norm_data
    norm_data <- norm_filter_exclude_include(df_impute, params)
    #norm_data <-  read_table_try("precursor_normdata", db_path)
    df_impute <- tmm_normalize(norm_data, df_impute, info_columns)
  }
  
  cat(file = stderr(), "Function - impute_apply_bg2...1", "\n")
  new_table_name <- stringr::str_c('precursor_impute_', norm)
  #added to bg above to avoid database locking error...  write_table_try(new_table_name, df_impute, db_path)
  
  cat(file = stderr(), "Function - impute_apply_bg2...end", "\n")
  return(list(new_table_name, df_impute))
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






