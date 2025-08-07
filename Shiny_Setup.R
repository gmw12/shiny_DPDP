cat(file = stderr(), "Shiny_Setup.R", "\n")

# #---------------------------------------------------------------------
# create_parameter_table <- function(session, input, output){
#   cat(file = stderr(), "\n",  "Function create_parameter_table", "\n")
# 
#   conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
#   RSQLite::dbWriteTable(conn, "params", df, overwrite = TRUE)
#   RSQLite::dbDisconnect(conn)
# 
#   write_table_try("params", df, db_path)
#   
#   gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
#   cat(file = stderr(), "Function create_parameter_table...end", "\n")
#   
#   return(df)
# }


#---------------------------------------------------------------------
# load_parameters <- function(session, input, output){
#   
#   cat(file = stderr(), "Function load_parameter_table...", "\n")
#   
#   updateCheckboxInput(session, "primary_group", value = params$primary_group)
#   
#   gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
#   cat(file = stderr(), "Function load_parameter_table...end", "\n")
#   
# }


#---------------------------------------------------------------------
load_design_file <- function(session, input, output){
  cat(file = stderr(), "Function load_design_file...", "\n")
  showModal(modalDialog("Loading design file...", footer = NULL))
  
  params <- create_default_params(volumes, python_path)
  
  params$file_prefix <- input$file_prefix
  
  design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
  params$design_path <- str_extract(design_sbf$datapath, "^/.*/")
  params$design_file <- design_sbf$datapath
  
  # set root data dir
  volumes <- c(dd = params$design_path, volumes)
  cat(file = stderr(), stringr::str_c("Adding default data directory --> "), "\n")
  cat(file = stderr(), stringr::str_c(volumes), "\n")
  
  #Global set of data and database paths
  params$data_path <- str_c(params$design_path, input$file_prefix, "/")
  db_path <- str_c(database_dir, "//", input$file_prefix, ".db")
  params$error_path <- str_c(params$data_path, "Error")
  
  params$database_dir <- database_dir
  params$database_path <- db_path
  
  assign("db_path", db_path, envir = .GlobalEnv)
  assign("volumes", volumes, envir = .GlobalEnv)
  
  #create working directory for 
  create_dir(params$data_path)
  create_dir(params$database_dir)
  create_dir(params$error_path)

  write_params(params, db_path)
  
  bg_design <- callr::r_bg(excel_to_db, args = list(design_sbf$datapath, "design", db_path), stderr = str_c(params$error_path, "//error_design.txt"), supervise = TRUE)
  bg_design$wait()
  print_stderr("error_design.txt", db_path)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  
  cat(file = stderr(), "Function load_design_file...end", "\n\n")
  
  hide_enable(session, input, output, db_path)
  
  removeModal()
}

#---------------------------------------------------------------------

load_archive_file <- function(session, input, output){
  cat(file = stderr(), "Function load_archive_file...", "\n")
  #showModal(modalDialog("Loading archive file...", footer = NULL))
  
  if(site_user == "dpmsr") {
    cat(file = stderr(), "loading archive_file for dpmsr...", "\n")
    archive_sfb <- parseFilePaths(volumes, input$sfb_archive_file)
    archive_path <- str_extract(archive_sfb$datapath, "^/.*/")
    cat(file = stderr(), stringr::str_c("archive_path --->", archive_path), "\n")
    archive_zip <- archive_sfb$datapath
    archive_name <- basename(archive_sfb$datapath)
  }else{
    cat(file = stderr(), "loading archive_file for customer...", "\n")
    archive_zip <-input$sfb_archive_customer_file$datapath
    cat(file = stderr(), stringr::str_c("archive_zip --->", archive_zip), "\n")
    archive_path <- str_extract(archive_zip, "^/.*/")
    cat(file = stderr(), stringr::str_c("archive_path --->", archive_path), "\n")
    database_dir <- stringr::str_c(database_dir, "/", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  
  create_dir(database_dir)
  
  # if archive_zip has file extention *.zip
  if (getExtension(archive_zip) == "zip"){
    cat(file = stderr(), stringr::str_c("unzip file to database_dir:  ", database_dir), "\n")
    utils::unzip(zipfile = archive_zip, exdir = database_dir)
    #check for files in database_dir
    temp_files <- list.files(database_dir)
    if (length(temp_files) == 0) {
      temp_files <- ""
    }
    cat(file = stderr(), stringr::str_c("files in database_dir:  ", temp_files), "\n")
    # section will load params file from db if not present
    zip_files <- list.files(path = database_dir, recursive = TRUE) 
    for (file_name in zip_files){
      if (tools::file_ext(file_name) == "db"){
        break
      }
    }
    
    db_path <- stringr::str_c(database_dir, "/", file_name)
    assign("db_path", db_path, envir = .GlobalEnv)
    params <- get_params(db_path)
    
    if (file.exists(stringr::str_c(database_dir, "/params"))){
      load(file=stringr::str_c(database_dir, "/params"))
    }
    
    params$database_path <- db_path
    params$database_dir <- database_dir #test added 8/7/25
    params$archive_source <- "zip"
    params$archive_path <- archive_path
    write_params(params, db_path)
    
  }else if (getExtension(archive_zip) == "dpmsr_set") {
      cat(file = stderr(), stringr::str_c("legacy dpmsr_set file loaded"), "\n")
      params$archive_source <- "dpmsr_set"
      params$design_path <- archive_path
      params$archive_path <- archive_path
      convert_dpmsr_set(archive_zip, params)
      params <- get_params()
  }else{
    cat(file = stderr(), stringr::str_c("archive file not recognized"), "\n")
  }
  
  #check params dir's and update if needed
  archive_update_dirs(session, input, output, db_path, archive_path)
  
  if(Sys.getenv("USER") == "erik") {
    system("chmod -R 777 /home/erik/Shiny_DPDP/database")
  }
  
  hide_enable(session, input, output, db_path)
  
  #removeModal()
  cat(file = stderr(), "Function load_archive_file...end", "\n")
  return(archive_zip)
}

#---------------------------------------------------------------------------------------------------------------------------
#. convert_dpmsr_set(archive_zip, params) # load(file = "/Users/gregwaitt/Data/10482_062124.dpmsr_set")

convert_dpmsr_set <- function(file_name, db_path){
  cat(file = stderr(), "function convert_dpmsr_set....", "\n")
  showModal(modalDialog("Converting dpmsr_set file to database..", footer = NULL))  
  
  #need a temp error path, will be updated later
  temp_error_path <- stringr::str_c(params$archive_path, "error")
  create_dir(temp_error_path)
  
  arg_list <- list(file_name, params)
  bg_convert_dpmsr_set <- callr::r_bg(func = convert_dpmsr_set_bg, args = arg_list, stderr = stringr::str_c(temp_error_path, "//bg_convert_dpmsr_set.txt"), supervise = TRUE)
  bg_convert_dpmsr_set$wait()
  print_temp_stderr("bg_convert_dpmsr_set.txt", temp_error_path)

  params <- bg_convert_dpmsr_set$get_result()
  
  assign("database_dir", params$database_dir, envir = .GlobalEnv)
  assign("database_path", params$database_path, envir = .GlobalEnv)

  set_sample_groups(db_path)
  params <- get_params()
  assign("params", params, envir = .GlobalEnv)
  
  cat(file = stderr(), "function convert_dpmsr_set....end", "\n\n")
  removeModal()
}

#---------------------------------------------------------------------------------------------------------------------------
convert_dpmsr_set_bg <- function(file_name, db_path){
  cat(file = stderr(), "function convert_dpmsr_set_bg....", "\n")
  source('Shiny_File.R')
  
  #save(file_name, file = "z1")
  #.  load(file = "z1"); load(file = archive_zip);  load(file = "/Users/gregwaitt/Data/10482_062124.dpmsr_set"); 
  
  cat(file = stderr(), stringr::str_c("Converting, ", file_name), "\n")
  load(file = file_name)
  
  params$file_prefix <- dpmsr_set$x$file_prefix
  params$raw_data_format <- tolower(dpmsr_set$x$raw_data_input)
  params$raw_data_output <- dpmsr_set$x$raw_data_output
  params$data_source <- dpmsr_set$x$data_source
  params$primary_group <- dpmsr_set$x$primary_group
  params$data_output <- dpmsr_set$x$final_data_output
  params$norm_ptm <- dpmsr_set$x$peptide_ptm_norm
  
  design <- dpmsr_set$design[,1:7]
  colnames(design)[colnames(design) == "PD_Order"] <- "Raw_Order"
  
  #Global set of data and database paths
  params$data_path <- stringr::str_c(params$archive_path, params$file_prefix, "/")
  params$database_dir <- database_dir
  params$database_path <- stringr::str_c(database_dir, "//", params$file_prefix, ".db")
  params$error_path <- create_dir(stringr::str_c(params$data_path, "Error"))
  
  #create working directory for 
  create_dir(params$data_path)
  create_dir(params$database_dir)
  create_dir(params$error_path)
  
  #dpmsr_set$data
  set_names <- c("data_raw_precursor", "data_precursor_start", "norm_data")
  db_names <- c("precursor_raw", "precursor_start", "precursor_normdata")
  for (i in (1:length(set_names))){
    if (set_names[i] %in% names(dpmsr_set$data)) {
      cat(file = stderr(), stringr::str_c("transferring dpmsr_set$data$", set_names[i], " to ", db_names[i]), "\n")
      write_table_try(db_names[i], df <- dpmsr_set$data[[set_names[i]]], db_path)
    }
  }
  
  
  #dpmsr_set$data$normalized
  set_names <- c("impute", "sl", "sl")
  db_names <- c("precursor_norm_impute", "precursor_norm_sl", "precursor_norm_sltmm")
  for (i in (1:length(set_names))){
    if (set_names[i] %in% names(dpmsr_set$data$normalized)) {
      cat(file = stderr(), stringr::str_c("transferring dpmsr_set$data$normalized$", set_names[i], " to ", db_names[i]), "\n")
      write_table_try(db_names[i], df <- dpmsr_set$data$normalized[[set_names[i]]], db_path)
    }
  }
  
  #dpmsr_set$data$impute
  set_names <- c("impute", "sl", "sltmm")
  params$norm_type <- paste(set_names, collapse = ",")
  db_names <- c("precursor_impute_impute", "precursor_impute_sl", "precursor_impute_sltmm")
  for (i in (1:length(set_names))){
    if (set_names[i] %in% names(dpmsr_set$data$impute)) {
      cat(file = stderr(), stringr::str_c("transferring dpmsr_set$data$impute$", set_names[i], " to ", db_names[i]), "\n")
      write_table_try(db_names[i], df <- dpmsr_set$data$impute[[set_names[i]]], db_path)
    }
  }
  
  #dpmsr_set$data$final
  set_names <- c("impute", "sl", "sltmm")
  db_names <- c("protein_impute_final", "protein_sl_final", "protein_sltmm_final")
  for (i in (1:length(set_names))){
    if (set_names[i] %in% names(dpmsr_set$data$final)) {
      cat(file = stderr(), stringr::str_c("transferring dpmsr_set$data$final$", set_names[i], " to ", db_names[i]), "\n")
      write_table_try(db_names[i], df <- dpmsr_set$data$final[[set_names[i]]], db_path)
    }
  }
  
  #no shortcuts here, params needs to be updated globally
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, "design", design, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "function convert_dpmsr_set_bg....end", "\n\n")
  return(params)
}


#--------------------------------------------------------------
archive_update_dirs <- function(session, input, output, db_path, archive_path){
  cat(file = stderr(), "Function update_dirs...", "\n")
  
  params <- get_params(db_path)
  
  if(fs::is_dir(params$design_path) & params$archive_source != "dpmsr_set") {
    cat(file = stderr(), "Current params dir structure is valid...", "\n")
  }else {
    cat(file = stderr(), stringr::str_c("Updating params dir structure... archive_path = ", archive_path), "\n")
    test <- unlist(str_split(params$design_path, "/"))
    test <- test[nzchar(test)]
    test <- test[length(test)]
    
    cat(file = stderr(), stringr::str_c("design_path = ", params$design_path), "\n")
    #params$design_path <- stringr::str_c(archive_path, test, "/")
    params$design_path <- archive_path
    cat(file = stderr(), stringr::str_c("new design_path = ", archive_path), "\n")
    params$data_path <- stringr::str_c(archive_path, params$file_prefix, "/")
    cat(file = stderr(), stringr::str_c("new data_path = ", params$data_path), "\n")
    params$backup_path <- stringr::str_c(params$data_path, "Backup/")
    params$extra_path <- stringr::str_c(params$data_path, "Extra/")
    params$error_path <- stringr::str_c(params$data_path, "Error/")
    params$qc_path <- stringr::str_c(params$data_path, "QC/")
    params$string_path <- stringr::str_c(params$data_path, "String/")
    params$phos_path <- stringr::str_c(params$data_path, "Phos/") 
    params$app_path <- stringr::str_c(params$data_path, "Backup//App/") 
    write_params(params, db_path)
    
    path_list <- c(params$design_path, params$data_path, params$backup_path, params$extra_path, params$error_path, params$qc_path, params$string_path, params$phos_path, params$app_path)
    
    for (path in path_list) {
      cat(file = stderr(), stringr::str_c("creating path = ", path), "\n")
      if (site_user == "dmpsr") {
        create_dir(path)
      }else{
        create_dir_only(path)
      }
    }
    
    #only update if DPMSR, customer does not have accesses 
    if (site_user == "dpmsr" & params$archive_source != "dpmsr_set") {
      #update plots
      if (params$raw_data_format != "protein") {
        parameter_create_plots(sesion, input, output, db_path)
        filter_histogram_plot(sesion, input, output, db_path, "precursor_start", "Precursor_Start_Histogram")
        render_noise_graphs(session, input, output, db_path)
        filter_histogram_plot(sesion, input, output, db_path, "precursor_noise", "Precursor_NoiseFiltered_Histogram")
        filter_create_plots(sesion, input, output, db_path)
      }
      
      ui_render_parameters(session, input, output, db_path)
      render_norm_graphs(session, input, output, db_path)
      render_norm_apply_graphs(session, input, output, db_path)
      impute_meta_data(db_path)
      impute_create_plots(session, input, output, db_path)
      render_impute_graphs(session, input, output, db_path)
      qc_stats(session, input, output, db_path)
      render_qc_graphs(session, input, output, db_path)
      
    }
  }
  
  cat(file = stderr(), "Function update_dirs...end", "\n")
}


#------------------------

check_comp_name_length <- function(){
  cat(file = stderr(), "Function check_comp_name_length...", "\n")
  names <- unique(dpmsr_set$design$Group)
  names_len <- sort(nchar(names), decreasing = TRUE)
  if (length(names) > 1) {
    longest_comp <- names_len[1] + names_len[2]
  }else{
    longest_comp <- names_len[1] + names_len[1]
  }
  if (longest_comp > 27) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}


#----------------------------------------------------------------------------------------
set_sample_groups <- function(db_path){
  cat(file = stderr(), "Function set_sample_groups...", "\n")
  showModal(modalDialog("Setting Sample Groups...", footer = NULL))
  
  bg_samplegroups <- callr::r_bg(set_sample_groups_bg, args = list(db_path), stderr = str_c(get_param('error_path', db_path), "//error_setsamplegroups.txt"), supervise = TRUE)
  bg_samplegroups$wait()
  print_stderr("error_setsamplegroups.txt", db_path)
  
  cat(file = stderr(), "set_sample_groups...end", "\n")
  removeModal()
}

#----------------------------------------------------------------------------------------
set_sample_groups_bg <- function(db_path){
  cat(file = stderr(), "Function set_sample_groups_bg ...", "\n")
  source("Shiny_File.R")

  params <- get_params(db_path)
  #----------------------------------------------------------------------------------------
  #check if sample list is sorted (groups are together)
  
  check_design_sort <- function(group_type, design){
    cat(file = stderr(), "Function check_design_sort...", "\n")
    #save(design, file = "z1"); save(design, file = "z2")
    # load(file = "z1"); load(file = "z2")
    design$check <- 0
    
    if (design[[group_type]][1] == design[[group_type]][2]) {
      design$check[1] <- 1
    }
    for (i in (2:nrow(design))) {
      if (design[[group_type]][i] == design[[group_type]][i - 1] || design[[group_type]][i] == design[[group_type]][i + 1]) {
        design$check[i] <- 1
      } 
    }
    
    if (sum(design$check) >= nrow(design)) {
      cat(file = stderr(), "The sample design file is properly sorted", "\n")
    }else{
      cat(file = stderr(), "The sample design file is NOT properly sorted", "\n")
      # sort dataframe, if SPQC exists put it at bottom
      design_spqc <- design[design[[group_type]] == 'SPQC',]
      design <- design[design[[group_type]] != 'SPQC',]
      
      design <- dplyr::arrange(design, design[[group_type]], Replicate)
      design <- rbind(design, design_spqc)
      cat(file = stderr(), "The sample design was sorted alphabetically with SPQC at bottom", "\n")
    }
    
    design$check <- NULL
    cat(file = stderr(), "Function check_design_sort...end", "\n")
    return(design)
  }

  design <- read_table_try("design", db_path)

  #check if using primary group for filter and impute
  cat(file = stderr(), "set_sample_groups ...1", "\n")
  design$PrimaryGroup <- sapply(design$Group, function(string) stringr::str_split(string, "_")[[1]][1])
  if (params$primary_group == "Primary") {
    group_type <- "PrimaryGroup"
  }else{
    group_type <- "Group"
  }
  
  #check to see if designfile/samplelist is sorted, if not then sort it
  design <- check_design_sort(group_type, design)
  
  # number of groups, comparisons are not hardcoded, R will extract information from excel SampleData file
  sample_number <- nrow(design)
  
  #count number of samples in each group - add column
  design$Count <- sapply(design[[group_type]], function(string) sum(string == design[[group_type]]))
  
  # create unqiue group dataframe with sample number
  sample_groups <- design[7:ncol(design)]
  sample_groups <- sample_groups |> dplyr::distinct(sample_groups[[group_type]], .keep_all = TRUE)
  #replace Group names with itself or the PrimaryGroup Name
  sample_groups$Group <- sample_groups[[group_type]]
  
  #count number of groups
  group_number <- nrow(sample_groups)
  
  cat(file = stderr(), "set_sample_groups ...2", "\n")
  #assign start and end columns for each group in pd output
  sample_groups$start <- 1
  sample_groups$end <- sample_groups$Count[1]
  for (i in 2:(group_number)) {
    sample_groups$start[i] <- sample_groups$start[i - 1] + sample_groups$Count[i - 1]
    sample_groups$end[i] <- sample_groups$start[i] + sample_groups$Count[i] - 1
  }
  
  #assign colors to groups
  cat(file = stderr(), "set_sample_groups ...3", "\n")
  color_choices <- randomcoloR::distinctColorPalette(group_number)
  group_color <- color_choices[1:group_number]
  sample_groups$colorlist <- color_choices[1:group_number]
  design$colorlist <- with(sample_groups, colorlist[match(design[[group_type]], sample_groups[[group_type]])])
  sample_groups$title <- sample_groups[[group_type]]

  #organize column headers for final output
  design$Header1 <- stringr::str_c(design$Group, " ", design$ID)
  design$Header2 <- stringr::str_c(design$Group, " ", design$ID, " Normalized")
  design$Header3 <- stringr::str_c(design$Group, " ", design$ID, " Imputed")
  
  cat(file = stderr(), "set_sample_groups ...4", "\n")
  params$sample_number <- sample_number
  params$group_number <- group_number
  params$unique_groups <- toString(unique(design$Group))
  
  cat(file = stderr(), "set_sample_groups ...5", "\n")
  
  write_table_try("design", design, db_path)
  write_table_try("sample_groups", sample_groups, db_path)
  
  write_params(params, db_path)
  
  #params0<-params; save(params0, file = "params0"); write_table_try("params0", params, db_path))
  #load(file = "params0")
  
  cat(file = stderr(), "set_sample_groups_bg ...end", "\n")
}


