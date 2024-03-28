cat(file = stderr(), "Shiny_MVA_Functions.R", "\n")

#-------------------------------------------------------------------------------
stat_create_comp_df <- function(df, factorsN, factorsD, params, df_design) {
  cat(file = stderr(), "Function - stat_create_comp_df...", "\n")
  
  #reduce dataframe to data only
  df_info <- df[1:(ncol(df) - params$sample_number)]
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df_N <- df
  df_D <- df
  df_SPQC <- df
  
  # find sample position numbers
  samples_N <- which(df_design$Group %in% as.list(factorsN))
  samples_D <- which(df_design$Group %in% as.list(factorsD))
  cat(file = stderr(), stringr::str_c("samples_N = ", samples_N), "\n")
  cat(file = stderr(), stringr::str_c("samples_D = ", samples_D), "\n")
  
  # find SPQC numbers
  samples_SPQC <- which(df_design$Group %in% as.list(params$comp_spqc))
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", samples_SPQC), "\n")
  
  # reduce dataframe to samples of interest
  df_N <- df_N[c(samples_N)]
  df_D <- df_D[c(samples_D)]
  cat(file = stderr(), stringr::str_c("df_N = ", ncol(df_N)), "\n")
  cat(file = stderr(), stringr::str_c("df_D = ", ncol(df_D)), "\n")
  
  # reduce dataframe to SPQC
  df_SPQC <- df_SPQC[c(samples_SPQC)]
  cat(file = stderr(), stringr::str_c("df_SPQC = ", ncol(df_SPQC)), "\n")
  
  #combine info and samples to dataframe for stats later
  df_out <- cbind(df_info, df_N, df_D, df_SPQC)
  
  return(df_out)
}


#----------------------------------------------------------------------------------