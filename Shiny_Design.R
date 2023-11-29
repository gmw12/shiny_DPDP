cat(file = stderr(), "Shiny_Design.R", "\n")

#---------------------------------------------------------------------
load_design_file <- function(session, input, output){
  cat(file = stderr(), "Function load_design_file", "\n")
  
  design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
  design_path <- str_extract(design_sbf$datapath, "^/.*/")
  
  #Global set of data and database paths
  data_path <<- str_c(design_path, input$file_prefix, "/")
  database_dir <- str_c(getwd(), "/database/")
  database_path <<- str_c(database_dir, input$file_prefix, ".db")
  
  #create working directory for 
  create_dir(data_path)
  create_dir(database_dir)
  
  cat(file = stderr(), str_c("loading design file from ", design_path), "\n")
  
  test <- callr::r_bg(excel_to_db, args = list(design_sbf$datapath, "design", database_path), stderr = "error.txt", supervise = TRUE)
  test$wait()
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  
  cat(file = stderr(), "Function load_design_file...end", "\n")
}


#------------------------

check_comp_name_length <- function(){
  cat(file = stderr(), "check_comp_name_length...", "\n")
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
  cat(file = stderr(), "Function set_sample_groups...", "\n")
  
  sample_info <- dpmsr_set$design

  #check if using primary group for filter and impute
  if (input$primary_group) {
    group_type <- "PrimaryGroup"
    sample_info$PrimaryGroup <- sapply(sample_info$Group, function(string) str_split(string, "_")[[1]][1])
  }else{
    group_type <- "Group"
  }
  
  #check to see if designfile/samplelist is sorted, if not then sort it
  sample_info <- check_design_sort(group_type, sample_info)
  
  # number of groups, comparisons are not hardcoded, R will extract information from excel SampleData file
  sample_number <- nrow(sample_info)
  
  #count number of samples in each group - add column
  sample_info$Count <- sapply(sample_info[[group_type]], function(string) sum(string == sample_info[[group_type]]))
  
  # create unqiue group dataframe with sample number
  sample_groups <- sample_info[7:ncol(sample_info)]
  sample_groups <- sample_groups %>% distinct(sample_groups[[group_type]], .keep_all = TRUE)
  #replace Group names with itself or the PrimaryGroup Name
  sample_groups$Group <- sample_groups[[group_type]]
  
  #count number of groups
  group_number <- nrow(sample_groups)
  
  #assign start and end columns for each group in pd output
  sample_groups$start <- 1
  sample_groups$end <- sample_groups$Count[1]
  for (i in 2:(group_number)) {
    sample_groups$start[i] <- sample_groups$start[i - 1] + sample_groups$Count[i - 1]
    sample_groups$end[i] <- sample_groups$start[i] + sample_groups$Count[i] - 1
  }
  
  #assign colors to groups
  color_choices <- distinctColorPalette(group_number)
  group_color <- color_choices[1:group_number]
  sample_groups$colorlist <- color_choices[1:group_number]
  sample_info$colorlist <- with(sample_groups, colorlist[match(sample_info[[group_type]], sample_groups[[group_type]])])
  sample_groups$title <- sample_groups[[group_type]]
  
  #organize column headers for final output
  sample_info$Header1 <- str_c(sample_info$ID, " ", sample_info$Group)
  sample_info$Header2 <- str_c(sample_info$ID, " ", sample_info$Group, " Normalized")
  sample_info$Header3 <- str_c(sample_info$ID, " ", sample_info$Group, " Imputed")
  
  #create factor vector
  group_factor <- rep(1, sample_groups$Count[1])
  for (i in 2:nrow(sample_groups)) {
    group_factor <- c(group_factor, rep(i, sample_groups$Count[i]))
  }
  group_factor <- factor(group_factor)
  
  #save to dpmsr_set
  dpmsr_set$y$sample_number <<- sample_number
  dpmsr_set$y$sample_groups <<- sample_groups
  dpmsr_set$y$group_number <<- group_number
  dpmsr_set$y$group_factor <<- group_factor
  dpmsr_set$design <<- sample_info
  
  #create dataframe to hold cv summaries for normalization strategies
  dpmsr_set$data$summary_cv <<- data.frame(sample_groups[[group_type]])
  
  #save unique group names delete this too
  dpmsr_set$y$uniquegroups <<- unique(sample_info$Group)
  
}

