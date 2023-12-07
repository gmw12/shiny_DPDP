cat(file = stderr(), "Shiny_Setup.R", "\n")




#---------------------------------------------------------------------
create_parameter_table <- function(session, input, output){
  cat(file = stderr(), "Function create_parameter_table", "\n")

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "parameters", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)

  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function create_parameter_table...end", "\n")
  
  return(df)
}


#---------------------------------------------------------------------
save_parameters <- function(session, input, output){
  
  cat(file = stderr(), "Function save_parameters...", "\n")

  for (name in names(params)) {
    if (!is.null(input[[name]])) {
      params[[name]] <<- input[[name]]
    }
  }

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "parameters", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function save_parameter_table...end", "\n")
  
}


#---------------------------------------------------------------------
load_parameters <- function(session, input, output){
  
  cat(file = stderr(), "Function load_parameter_table...", "\n")
  
  updateCheckboxInput(session, "primary_group", value = params$primary_group)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function load_parameter_table...end", "\n")
  
}



#---------------------------------------------------------------------
load_design_file <- function(session, input, output){
  cat(file = stderr(), "Function load_design_file", "\n")
  
  design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
  design_path <- str_extract(design_sbf$datapath, "^/.*/")
  
  #Global set of data and database paths
  params$data_path <<- str_c(design_path, input$file_prefix, "/")
  database_dir <- str_c(getwd(), "/database/")
  params$database_path <<- str_c(database_dir, input$file_prefix, ".db")
  
  #create working directory for 
  create_dir(params$data_path)
  create_dir(database_dir)
  
  cat(file = stderr(), str_c("loading design file from ", design_path), "\n")
  
  test <- callr::r_bg(excel_to_db, args = list(design_sbf$datapath, "design", params$database_path), stderr = "error.txt", supervise = TRUE)
  test$wait()
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  
  cat(file = stderr(), "Function load_design_file...end", "\n")
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
set_sample_groups <- function(session, input, output){
  background <- callr::r_bg(set_sample_groups_bg, args = list(session, input, output, params), stderr = "error.txt", supervise = TRUE)
  background$wait()
}
#----------------------------------------------------------------------------------------
set_sample_groups_bg <- function(session, input, output, params, check_design_sort){
  cat(file = stderr(), "Function set_sample_groups...", "\n")
  
  
  #----------------------------------------------------------------------------------------
  #check if sample list is sorted (groups are together)
  
  check_design_sort <- function(group_type, design){
    cat(file = stderr(), "Function check_design_sort...", "\n")
    
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
      
      design <- arrange(design, design[[group_type]], Replicate)
      design <- rbind(design, design_spqc)
      cat(file = stderr(), "The sample design was sorted alphabetically with SPQC at bottom", "\n")
    }
    
    design$check <- NULL
    return(design)
  }


  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  design <- RSQLite::dbReadTable(conn, "design")
  RSQLite::dbDisconnect(conn)
  
  #check if using primary group for filter and impute
  cat(file = stderr(), "set_sample_groups ...1", "\n")
  if (params$primary_group == "Primary") {
    group_type <- "PrimaryGroup"
    design$PrimaryGroup <- sapply(design$Group, function(string) stringr::str_split(string, "_")[[1]][1])
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
  design$Header1 <- stringr::str_c(design$ID, " ", design$Group)
  design$Header2 <- stringr::str_c(design$ID, " ", design$Group, " Normalized")
  design$Header3 <- stringr::str_c(design$ID, " ", design$Group, " Imputed")
  
  cat(file = stderr(), "set_sample_groups ...4", "\n")
  params$sample_number <- sample_number
  params$group_number <- group_number
  params$unique_groups <- toString(unique(design$Group))
  
  cat(file = stderr(), "set_sample_groups ...5", "\n")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "sample_groups", sample_groups, overwrite = TRUE )
  RSQLite::dbWriteTable(conn, "design", design, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "parameters", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
}

