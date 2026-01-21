cat("Loading Shiny_Report_Functions.R - Version 2.1 (Formatting Fix - Smart Detection)\n")

report_options <- function(session, input, output, db_path) {
  cat(file = stderr(), "Function report_options...", "\n")
  
  params <- get_params(db_path)
  
  #read excel file to dataframe
  library(readxl)
  base_path <- "/mnt/DocShare/Proteomics_Report_Templates/lists"
  
  df_staff <- read_excel(stringr::str_c(base_path, "/staff.xlsx"), col_names = TRUE)
  df_staff$full_name <- stringr::str_c(df_staff$first_name, " ", df_staff$last_name)
  df_report_types <- read_excel(stringr::str_c(base_path, "/report_types.xlsx"), col_names = TRUE)

  df_databases <- read_excel(stringr::str_c(base_path, "/databases.xlsx"), col_names = TRUE)
  
  report_list <- c("report_sample_prep", "report_data_analysis", "report_data_collection", 
                    "report_study_design", "report_writing")
  
  for (i in 1:length(report_list)) {
    updatePickerInput(session, report_list[i], choices = df_staff$full_name)
  }
  
  updatePickerInput(session, "report_sample_prep", selected = "Tricia Ho")
  updatePickerInput(session, "report_data_collection", selected = "Greg Waitt")
  updatePickerInput(session, "report_data_analysis", selected = "Greg Waitt")
  updatePickerInput(session, "report_study_design", selected = "Erik Soderblom")
  updatePickerInput(session, "report_writing", selected = "Jeffrey Kuhn")
  
  updatePickerInput(session, "report_databases", choices = df_databases$database)
  
  updateSelectInput(session, "report_type", choices = df_report_types$report_type)
  
  #get the word before the first "_" in params$file_prefix
  project_number <- strsplit(get_param("file_prefix", db_path), "_")[[1]][1]
  report_filename <- stringr::str_c(project_number, "_Report_", format(Sys.Date(), "%m%d%y"), ".docx")
  
  updateTextInput(session, "report_filename", value = report_filename)
  
  #check if params$raw_data_paths exists
  if (is.null(params$raw_data_paths)) {
    shinyalert(session = session,
               title = "Error",
               text = "Please navigate to the TIC tab and select at least one file.",
               type = "error",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE)
  }
  
  #report templates
  template_path <- "/mnt/DocShare/Proteomics_Report_Templates/base_templates"
  docx_files <- list.files(template_path, pattern = "\\.docx$", full.names = FALSE)
  docx_files <- docx_files[!grepl("^~\\$", docx_files)]
  
  # Update your UI element
  updateSelectInput(session, "report_template", choices = docx_files)
  

  cat(file = stderr(), "Function report_options...end", "\n")
}

#-------------------------------------------------------------------------------------------


read_replacement_text <- function(file_path) {
  text <- readr::read_file(file_path, locale = readr::locale(encoding = "latin1"))
  trimws(text)
}

#-------------------------------------------------------------------------------------------
replace_text_placeholders_batch_robust <- function(docx_in, docx_out, replacements) {
  cat(file = stderr(), "Function replace_text_placeholders_batch_robust...", "\n")
  library(xml2)
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(docx_in, exdir = temp_dir)
  
  word_dir <- file.path(temp_dir, "word")
  
  xml_files <- list.files(word_dir, pattern = "\\.(xml)$", full.names = TRUE)
  relevant_files <- grep("document\\.xml|header[0-9]*\\.xml|footer[0-9]*\\.xml", 
                         xml_files, value = TRUE)
  
  for (xml_file in relevant_files) {
    if (!file.exists(xml_file)) next
    
    # Suppress namespace warnings from xml2 when reading Word document XML
    doc_xml <- suppressWarnings(read_xml(xml_file))
    ns <- xml_ns(doc_xml)
    
    paragraphs <- xml_find_all(doc_xml, ".//w:p", ns)
    
    for (para in paragraphs) {
      text_nodes <- xml_find_all(para, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      
      # Get full paragraph text
      para_text <- paste(sapply(text_nodes, xml_text), collapse = "")
      
      # Try each placeholder
      for (placeholder in names(replacements)) {
        if (grepl(placeholder, para_text, fixed = TRUE)) {
          para_text <- gsub(placeholder, replacements[[placeholder]], para_text, fixed = TRUE)
        }
      }
      
      # If text changed, preserve formatting by modifying the existing first run
      original_text <- paste(sapply(text_nodes, xml_text), collapse = "")
      if (para_text != original_text) {
        # Get all unique parent runs
        parent_runs <- unique(lapply(text_nodes, xml_parent))
        
        # Check if all runs have the same formatting by comparing their rPr
        all_same_format <- TRUE
        if (length(parent_runs) > 1) {
          first_rPr <- xml_find_first(parent_runs[[1]], ".//w:rPr", ns)
          for (i in 2:length(parent_runs)) {
            current_rPr <- xml_find_first(parent_runs[[i]], ".//w:rPr", ns)
            if (!identical(as.character(first_rPr), as.character(current_rPr))) {
              all_same_format <- FALSE
              break
            }
          }
        }
        
        # If formatting is inconsistent across the placeholder, use no formatting
        if (all_same_format) {
          # Use the first run's formatting
          first_run <- parent_runs[[1]]
          existing_text_nodes <- xml_find_all(first_run, ".//w:t", ns)
          
          if (length(existing_text_nodes) > 0) {
            # Clear all text nodes except the first
            if (length(existing_text_nodes) > 1) {
              for (i in 2:length(existing_text_nodes)) {
                xml_remove(existing_text_nodes[[i]])
              }
            }
            # Update the first text node
            xml_text(existing_text_nodes[[1]]) <- para_text
          }
          
          # Remove all other parent runs
          if (length(parent_runs) > 1) {
            for (i in 2:length(parent_runs)) {
              xml_remove(parent_runs[[i]])
            }
          }
        } else {
          # Mixed formatting - create a new run with no formatting
          new_run <- read_xml(
            '<w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
               <w:t xml:space="preserve"></w:t>
             </w:r>'
          )
          text_node <- xml_find_first(new_run, ".//w:t")
          xml_text(text_node) <- para_text
          
          # Insert new run before the first parent run
          xml_add_sibling(parent_runs[[1]], new_run, .where = "before")
          
          # Remove all old parent runs
          for (parent_run in parent_runs) {
            xml_remove(parent_run)
          }
        }
      }
    }
    
    write_xml(doc_xml, xml_file)
  }
  
  # Rezip
  old_wd <- getwd()
  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, all.files = TRUE, include.dirs = FALSE)
  zip::zip(basename(docx_out), files = files, compression_level = 9)
  file.copy(basename(docx_out), docx_out, overwrite = TRUE)
  setwd(old_wd)
  
  unlink(temp_dir, recursive = TRUE)
  invisible(docx_out)
  cat(file = stderr(), "Function replace_text_placeholders_batch_robust...end", "\n")
}


#-------------------------------------------------------------------------------------------

replace_text_placeholder_robust <- function(docx_in, docx_out, placeholder, replacement_text) {
  cat(file = stderr(), "Function replace_text_placeholders_robust...", "\n")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(docx_in, exdir = temp_dir)
  
  word_dir <- file.path(temp_dir, "word")
  
  xml_files <- list.files(word_dir, pattern = "\\.(xml)$", full.names = TRUE)
  relevant_files <- grep("document\\.xml|header[0-9]*\\.xml|footer[0-9]*\\.xml", 
                         xml_files, value = TRUE)
  
  replaced <- FALSE
  
  for (xml_file in relevant_files) {
    if (!file.exists(xml_file)) next
    
    # Suppress namespace warnings from xml2 when reading Word document XML
    doc_xml <- suppressWarnings(read_xml(xml_file))
    ns <- xml_ns(doc_xml)
    
    paragraphs <- xml_find_all(doc_xml, ".//w:p", ns)
    
    for (para in paragraphs) {
      text_nodes <- xml_find_all(para, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      
      para_text <- paste(sapply(text_nodes, xml_text), collapse = "")
      
      if (grepl(placeholder, para_text, fixed = TRUE)) {
        para_text <- gsub(placeholder, replacement_text, para_text, fixed = TRUE)
        
        # Get all unique parent runs
        parent_runs <- unique(lapply(text_nodes, xml_parent))
        
        # Check if all runs have the same formatting by comparing their rPr
        all_same_format <- TRUE
        if (length(parent_runs) > 1) {
          first_rPr <- xml_find_first(parent_runs[[1]], ".//w:rPr", ns)
          for (i in 2:length(parent_runs)) {
            current_rPr <- xml_find_first(parent_runs[[i]], ".//w:rPr", ns)
            if (!identical(as.character(first_rPr), as.character(current_rPr))) {
              all_same_format <- FALSE
              break
            }
          }
        }
        
        # If formatting is inconsistent across the placeholder, use no formatting
        if (all_same_format) {
          # Use the first run's formatting
          first_run <- parent_runs[[1]]
          existing_text_nodes <- xml_find_all(first_run, ".//w:t", ns)
          
          if (length(existing_text_nodes) > 0) {
            # Clear all text nodes except the first
            if (length(existing_text_nodes) > 1) {
              for (i in 2:length(existing_text_nodes)) {
                xml_remove(existing_text_nodes[[i]])
              }
            }
            # Update the first text node
            xml_text(existing_text_nodes[[1]]) <- para_text
          }
          
          # Remove all other parent runs
          if (length(parent_runs) > 1) {
            for (i in 2:length(parent_runs)) {
              xml_remove(parent_runs[[i]])
            }
          }
        } else {
          # Mixed formatting - create a new run with no formatting
          new_run <- read_xml(
            '<w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
               <w:t xml:space="preserve"></w:t>
             </w:r>'
          )
          text_node <- xml_find_first(new_run, ".//w:t")
          xml_text(text_node) <- para_text
          
          # Insert new run before the first parent run
          xml_add_sibling(parent_runs[[1]], new_run, .where = "before")
          
          # Remove all old parent runs
          for (parent_run in parent_runs) {
            xml_remove(parent_run)
          }
        }
        
        replaced <- TRUE
      }
    }
    
    write_xml(doc_xml, xml_file)
  }
  
  if (!replaced) {
    warning(paste0("Placeholder '", placeholder, "' not found in document, headers, or footers"))
  }
  
  # Rezip
  old_wd <- getwd()
  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, all.files = TRUE, include.dirs = FALSE)
  zip::zip(basename(docx_out), files = files, compression_level = 9)
  file.copy(basename(docx_out), docx_out, overwrite = TRUE)
  setwd(old_wd)
  
  unlink(temp_dir, recursive = TRUE)
  invisible(docx_out)
  
  cat(file = stderr(), "Function replace_text_placeholders_robust...end", "\n")
}

#-------------------------------------------------------------------------------------------

extract_method_info <- function(db_path) {
  cat(file = stderr(), "Function extract_method_info...", "\n")
  library(rawrr)
  
  rawfile <- get_param("raw_data_paths", db_path)
  rawfile <- rawfile[1]
  
  H <- rawrr::readFileHeader(rawfile = rawfile)
  I <- rawrr::readIndex(rawfile = rawfile)
  S1 <- rawrr::readSpectrum(rawfile = rawfile, scan = 1)
  S2 <- rawrr::readSpectrum(rawfile = rawfile, scan = 2)
  S1 <- S1[[1]]
  S2 <- S2[[1]]
  
  #create empty dataframe df_info
  df_info <- data.frame(
    Label = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  header_list <- c("Serial number", "Sample comment", "User text 0", "Time range", "Instrument method")
  
  s1_list <- c("FT Resolution:", "Max. Ion Time (ms):", "massRange", "AGC Target:")
  
  s2_list <- c("massRange", "Max. Ion Time (ms):", "MS2 Isolation Width:", 
               "AGC Target:", "HCD Energy:")
  
  
  for (item in header_list) {
    df_info <- rbind(df_info, data.frame(Label = item, Value = as.character(H[[item]]), stringsAsFactors = FALSE))
  }
  
  for (item in s1_list) {
    df_info <- rbind(df_info, data.frame(Label = paste0("Scan 1 - ", item), Value = as.character(S1[[item]]), stringsAsFactors = FALSE))
  }
  
  for (item in s2_list) {
    df_info <- rbind(df_info, data.frame(Label = paste0("Scan 2 - ", item), Value = as.character(S2[[item]]), stringsAsFactors = FALSE))
  }
  
  dia_range1 <- floor(min(I$precursorMass) - (as.numeric(S2$"MS2 Isolation Width:")/2))
  dia_range2 <- floor(max(I$precursorMass) + (as.numeric(S2$"MS2 Isolation Width:")/2))
  
  dia_window <- round((I$precursorMass[3] - I$precursorMass[2]), digits=1)
  
  df_info <- rbind(df_info, data.frame(Label = "DIA range1", Value = dia_range1, stringsAsFactors = FALSE))
  df_info <- rbind(df_info, data.frame(Label = "DIA range2", Value = dia_range2, stringsAsFactors = FALSE))
  df_info <- rbind(df_info, data.frame(Label = "DIA window", Value = dia_window, stringsAsFactors = FALSE))
  
  cat(file = stderr(), "Function extract_method_info...end", "\n")
  return(df_info)
  
}


#-------------------------------------------------------------------------------------------

get_google_data <- function(project_number) {
  cat(file = stderr(), "Function get_google_data...", "\n")
  library(googlesheets4)
  library(dplyr)
  
  # Authenticate with Google Sheets using service account
  gs4_auth(
    path = Sys.getenv("GSA_KEY_PATH"),
    cache = FALSE
  )
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1e5vAffsYfqYL-g38XcNSK_aq1OqohDGZ9QF_582enbc/edit#gid=0"
  
  # Try Current Projects first
  dat <- read_sheet(sheet_url, sheet = "Current Projects")
  
  # Filter for the project number
  filtered_dat <- dat %>%
    filter(`Project Group #` == project_number)
  
  # If no results found, try Completed tab
  if (nrow(filtered_dat) == 0) {
    message("Project not found in Current Projects. Checking Completed tab...")
    dat <- read_sheet(sheet_url, sheet = "Completed")
    
    filtered_dat <- dat %>%
      filter(`Project Group #` == project_number)
    
    # If still not found, return an error or NULL
    if (nrow(filtered_dat) == 0) {
      stop("Project number ", project_number, " not found in either Current Projects or Completed tabs.")
    }
  }
  
  # Extract values from filtered data
  order_id <- filtered_dat %>%
    pull(`Order #`)
  
  sample_amount <- filtered_dat %>%
    pull(`Sample_Amount`)
  
  resuspend_volume <- filtered_dat %>%
    pull(`Recon_Vol`)
  
  trypsin_amount <- filtered_dat %>%
    pull(`Trypsin_Amount`)
  
  cat(file = stderr(), "Function get_google_data...end", "\n")
  
  return(list(
    order_id = order_id,
    sample_amount = sample_amount,
    resuspend_volume = resuspend_volume,
    trypsin_amount = trypsin_amount
  ))
}



#---------------------------------------------------------------------
# Function to create the formatted text
format_sample_counts <- function(df) {
  # Get unique counts
  unique_counts <- unique(df$Count)
  
  # Create a list to store groups by count
  count_groups <- list()
  
  for (count in unique_counts) {
    # Get groups with this count
    groups <- df$Group[df$Count == count]
    count_groups[[as.character(count)]] <- groups
  }
  
  # Build the text
  text_parts <- c()
  
  for (count in names(count_groups)) {
    groups <- count_groups[[count]]
    
    # Format the group names
    if (length(groups) == 1) {
      group_text <- groups[1]
    } else if (length(groups) == 2) {
      group_text <- paste(groups[1], "and", groups[2])
    } else {
      # For 3+ groups: use commas and "and" before last item
      group_text <- paste(
        paste(groups[-length(groups)], collapse = ", "),
        "and",
        groups[length(groups)]
      )
    }
    
    text_parts <- c(text_parts, paste0(count, " of each ", group_text))
  }
  
  # Join with semicolons
  final_text <- paste(text_parts, collapse = "; ")
  
  return(final_text)
}

#--------------------------------------------------------------------------------------------------
create_staff_contribution <- function(session, input, output) {
  cat(file = stderr(), "Function create_staff_contribution...\n")
  
  # Collect input data
  all_data <- list(
    study_design = input$report_study_design,
    sample_preparation = input$report_sample_prep,
    data_collection = input$report_data_collection,
    data_analysis = input$report_data_analysis,
    report_writing = input$report_writing
  )
  
  # Handle NULL or empty inputs
  all_data <- lapply(all_data, function(x) if(is.null(x)) character(0) else x)
  
  # Get unique people
  people <- unique(unlist(all_data))
  
  # Early return if no contributors
  if (length(people) == 0) {
    cat(file = stderr(), "Function create_staff_contribution...end (no contributors)\n")
    return("")
  }
  
  # Extract last names for sorting
  get_last_name <- function(full_name) {
    parts <- strsplit(full_name, " ")[[1]]
    return(parts[length(parts)])
  }
  
  # Sort people by last name
  people <- people[order(sapply(people, get_last_name))]
  
  # Map people to categories
  result <- sapply(people, function(person) {
    categories <- names(all_data)[sapply(all_data, function(x) person %in% x)]
    paste0(person, " (", paste(categories, collapse = ", "), ")")
  }, USE.NAMES = FALSE)
  
  # Format output
  staff_contribution <- gsub("_", " ", result)
  staff_contribution <- paste(staff_contribution, collapse = ", ")
  
  cat(file = stderr(), "Function create_staff_contribution...end\n")
  return(staff_contribution)
}
#-------------------------------------------------------------------------------------------

create_staff_contribution_old <- function(session, input, output) {
  cat(file = stderr(), "Function create_staff_contribution...", "\n")
 
   # Your lists
  study_design <- input$report_study_design
  sample_preparation <- input$report_sample_prep
  data_collection <- input$report_data_collection
  data_analysis <- input$report_data_analysis
  report_writing <- input$report_writing
  
  # Combine into a list
  all_data <- list(study_design = study_design, sample_preparation = sample_preparation, 
                   data_collection = data_collection, data_analysis = data_analysis, 
                   report_writing = report_writing)
 
  # Get all unique people
  people <- unique(unlist(all_data))
  
  # For each person, find which lists they're in
  result <- sapply(people, function(person) {
    categories <- names(all_data)[sapply(all_data, function(x) person %in% x)]
    paste0(person, " (", paste(categories, collapse = ", "), ")")
  })
  
  staff_contribution <- unname(result)
  #replace "_" with " " in staff_contribution
  staff_contribution <- gsub("_", " ", staff_contribution)
  
  #collapse staff_contribution into a single string with ", " separator
  staff_contribution <- paste(staff_contribution, collapse = ", ")
  
  cat(file = stderr(), "Function create_staff_contribution...end", "\n")
  return(staff_contribution)
}


#-------------------------------------------------------------------------------------------

create_searched_databases <- function(database_list) {
  cat(file = stderr(), "Function create_searched_databases...", "\n")
  #  
  database_list <- c("mouse", "human", "rat")

  
  database_string <- "The MS/MS data was searched against a "
  for (i in 1:length(database_list)) {
    if (i == 1) {
      database_string <- stringr::str_c(database_string, database_list[i])
    } else if (i == length(database_list)) {
      database_string <- stringr::str_c(database_string, ", ", database_list[i])  # No trailing comma
    } else {
      database_string <- stringr::str_c(database_string, ", ", database_list[i])
    }
  }
  
  database_string_end <- 'a common contaminant/spiked protein database (bovine albumin, bovine casein, yeast ADH, etc.) and an equal number of reversed-sequence "decoys" for false discovery rate determination.'
  
  full_string <- stringr::str_c(database_string, " ", database_string_end)

  
  cat(file = stderr(), "Function create_searched_databases...end", "\n") 
  
  return(full_string)
  
}


#-------------------------------------------------------------------------------------------

create_modifications <- function(session, input, output, db_path) {
  cat(file = stderr(), "Function create_modifications...", "\n")
  
  library(readxl)
  base_path <- "/mnt/DocShare/Proteomics_Report_Templates/lists"
  
  df_mods <- read_excel(stringr::str_c(base_path, "/modifications.xlsx"), col_names = TRUE)

  df_start <- read_table('precursor_start', db_path)
  
  #IAA check
  if (any(grepl(df_mods$mod_sp[1], df_start$PrecursorId, fixed = TRUE))) {
    mod_string <- "Database search parameters included fixed modification on Cys (carbamidomethyl)"
  }else
    {
    mod_string <- "Database search parameters included variable modifications on "
    }
  
  first_variable <- 0
  for (i in 2:nrow(df_mods)) {
    if (any(grepl(df_mods$mod_sp[i], df_start$PrecursorId, fixed = TRUE))) {
      if (first_variable ==0) {
        mod_string <- stringr::str_c(mod_string, ", variable modification on ", df_mods$mod_name[i])
        first_variable <- 1
      }else {
        mod_string <- stringr::str_c(mod_string, ", ", df_mods$mod_name[i])
      }
    }
   }

   
  #replace last "," in mod_string with "and"
  mod_string <- sub(", ([^,]*)$", " and \\1", mod_string)

  cat(file = stderr(), "Function create_modifications...end", "\n")
  
  return(mod_string)
}


#---------------------------------------------------------------
template_parameters <- function(session, input, output, db_path, df_template_R) {
  cat(file = stderr(), "Function template_parameters...", "\n")
  
  #  df_template <- df_template_R
  params <- get_params(db_path)
  
  rawfile <- get_param("raw_data_paths", db_path)
  project_number <- sub(".*/RawData/(\\d+)/.*", "\\1", rawfile)
  google_data_list <- get_google_data(project_number)
  order_id <- unlist(google_data_list[["order_id"]])
  pamlims_data_list <- get_pamlims_data(order_id)
  df_info <- extract_method_info(db_path)
  sample_groups <- read_table('sample_groups', db_path)
  sample_groups_filtered <- sample_groups[!grepl("^SPQC$", sample_groups$Group, ignore.case = TRUE), ]
  
  df <- data.frame(Parameter = character(),
                   Value = character(),
                   stringsAsFactors = FALSE)

  #basic stats
  param_list <- df_template_R$key

  #add param_list to df with empty values
  for (param in param_list) {
    df <- rbind(df, data.frame(Parameter = param, Value = "", stringsAsFactors = FALSE))
  }
  
  #fill in values
  df[df$Parameter=="R_project_number",]$Value <- project_number
  
  df_cv <- read_table('protein_sltmm_cv', db_path)
  
  df[df$Parameter=="R_spqc_cv",]$Value <- round(mean(df_cv[, grepl("spqc", names(df_cv), ignore.case = TRUE)]), digits = 1)
  df[df$Parameter=="R_sample_cv",]$Value <- round(mean(unlist(df_cv[, !grepl("spqc", names(df_cv), ignore.case = TRUE) & 
                                    grepl("_CV", names(df_cv), ignore.case = TRUE)])), digits = 1)
  
  df[df$Parameter=="R_resuspend_volume",]$Value <- google_data_list[["resuspend_volume"]]
  df[df$Parameter=="R_trypsin_amount",]$Value <- google_data_list[["trypsin_amount"]]
  
  sample_amount <- google_data_list[["sample_amount"]]
  #if sample_amount is a number then multiply by 100 and add "%"
  if (!is.na(as.numeric(sample_amount))) {
    sample_amount <- paste0(as.numeric(sample_amount) * 100, "%")
  }
  df[df$Parameter=="R_sample_amount",]$Value <- sample_amount
  
  
  #filter sample_groups to remove rows where sample_groups$Group is "SPQC" case insensitive
  
  df[df$Parameter=="R_sample_number",]$Value <- nrow(sample_groups_filtered)
  df[df$Parameter=="R_sample_descriptions",]$Value <- format_sample_counts(sample_groups_filtered)
  df[df$Parameter=="R_staff_contribution",]$Value <- create_staff_contribution(session, input, output)
  
  df[df$Parameter=="R_report_type",]$Value <- input$report_type
  
  df[df$Parameter=="R_today_date",]$Value <- trimws(format(Sys.Date(), "%B %e, %Y"))
  df[df$Parameter=="R_report_file_name",]$Value <- stringr::str_c(project_number, "_Report_", format(Sys.Date(), "%m%d%y"), ".pdf")
  df[df$Parameter=="R_excel_file_name",]$Value <- stringr::str_c(project_number, "_SupplementalData_",toupper(params$stat_norm), "_", format(Sys.Date(), "%m%d%y"), ".xlsx")
  
  
  if (params$norm_exclude !="none") {
    df[df$Parameter=="R_exclude_norm_txt",]$Value <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/data_analysis/exclude_norm.txt" )
  } else {
    df[df$Parameter=="R_exclude_norm_txt",]$Value <- ""
  }
  
  if (params$stat_norm =="sltmm") {
    df[df$Parameter=="R_norm_impute_strategy_txt",]$Value <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/data_analysis/norm_sltmm.txt" )
  } else {
    df[df$Parameter=="R_norm_impute_strategy_txt",]$Value <- ""
  }
  
  if (params$noise_type !="none") {
    df[df$Parameter=="R_filter_noise",]$Value <- read_replacement_text(df_template_R[df_template_R$key=="R_filter_noise",]$source )
  } else {
    df[df$Parameter=="R_filter_noise",]$Value <- ""
  }
  
  if (params$filter_x_percent) {
    df[df$Parameter=="R_filter_2_50",]$Value <- read_replacement_text(df_template_R[df_template_R$key=="R_filter_2_50",]$source )
  } else {
    df[df$Parameter=="R_filter_2_50",]$Value <- ""
  }
  
  if (params$checkbox_misaligned) {
    df[df$Parameter=="R_filter_misalign",]$Value <- read_replacement_text(df_template_R[df_template_R$key=="R_filter_misalign",]$source )
  } else {
    df[df$Parameter=="R_filter_misalign",]$Value <- ""
  }
  
  if (params$precursor_quality) {
    df[df$Parameter=="R_filter_precursor_quality",]$Value <- read_replacement_text(df_template_R[df_template_R$key=="R_filter_precursor_quality",]$source )
  } else {
    df[df$Parameter=="R_filter_precursor_quality",]$Value <- ""
  }
  
  if (params$precursor_spqc_ratio) {
    df[df$Parameter=="R_filter_spqc",]$Value <- read_replacement_text(df_template_R[df_template_R$key=="R_filter_spqc",]$source )
  } else {
    df[df$Parameter=="R_filter_spqc",]$Value <- ""
  }
  
  if (params$impute_type == "duke") {
    df[df$Parameter=="R_imputation_strategy_txt",]$Value <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/data_analysis/imputation_duke.txt")
  } else {
    df[df$Parameter=="R_imputation_strategy_txt",]$Value <- ""
  }
  
  if (params$rollup_method == "iq_maxlfq") {
    df[df$Parameter=="R_rollup_strategy_txt",]$Value <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/data_analysis/rollup_maxlfq.txt")
  } else {
    df[df$Parameter=="R_rollup_strategy_txt",]$Value <- ""
  }

  
  df[df$Parameter=="R_ms1_resolution",]$Value <- df_info$Value[df_info$Label == "Scan 1 - FT Resolution:"]
  df[df$Parameter=="R_ms1_scan",]$Value <- stringr::str_c(df_info$Value[df_info$Label == "Scan 1 - massRange"][1], "-", 
                                                         df_info$Value[df_info$Label == "Scan 1 - massRange"][2])
  df[df$Parameter=="R_ms1_agc",]$Value <- format(as.numeric(df_info$Value[df_info$Label == "Scan 1 - AGC Target:"]), scientific = TRUE)
  df[df$Parameter=="R_dia_window",]$Value <- as.character(round(as.numeric(df_info$Value[df_info$Label == "DIA window"]), digits = 1))
  df[df$Parameter=="R_dia_range",]$Value <- stringr::str_c(df_info$Value[df_info$Label == "DIA range1"], "-", 
                                                          df_info$Value[df_info$Label == "DIA range2"])
  df[df$Parameter=="R_ms2_agc",]$Value <- format(as.numeric(df_info$Value[df_info$Label == "Scan 2 - AGC Target:"]), scientific = TRUE)
  df[df$Parameter=="R_ms2_fill",]$Value <- as.character(round(as.numeric(df_info$Value[df_info$Label == "Scan 2 - Max. Ion Time (ms):"]), digits = 0))
  df[df$Parameter=="R_hcd_energy",]$Value <- as.character(round(as.numeric(df_info$Value[df_info$Label == "Scan 2 - HCD Energy:"]), digits = 0))
  df[df$Parameter=="R_run_time",]$Value <- as.character(round(as.numeric(df_info$Value[df_info$Label == "Time range"][2]), digits=0))
  df[df$Parameter=="R_injection_count",]$Value <- params$sample_number
  df[df$Parameter=="R_precursor_count",]$Value <- format(nrow(read_table('precursor_filter', db_path)), big.mark = ",")
  df[df$Parameter=="R_protein_count",]$Value <- format(nrow(read_table('protein_impute', db_path)), big.mark = ",")
  df[df$Parameter=="R_total_tables",]$Value <- (4 + as.numeric(params$comp_number))
  df[df$Parameter=="R_imputation_percent",]$Value <- params$bottom_x
  
  df[df$Parameter=="R_searched_databases",]$Value <- create_searched_databases(input$report_databases)
  df[df$Parameter=="R_search_mods",]$Value <- create_modifications(session, input, output, db_path)
  
  df[df$Parameter=="R_report_type",]$Value <- input$report_type
  df[df$Parameter=="R_dukebox_folder",]$Value <- input$dukebox_folder
  
  df[df$Parameter=="R_customer_name",]$Value <- pamlims_data_list[["customer_name"]]
  df[df$Parameter=="R_pi_name",]$Value <- pamlims_data_list[["pi_name"]]
  df[df$Parameter=="R_customer_lastname",]$Value <- pamlims_data_list[["customer_lastname"]]
  df[df$Parameter=="R_pi_lastname",]$Value <- pamlims_data_list[["pi_lastname"]]
  df[df$Parameter=="R_project_name",]$Value <- pamlims_data_list[["project_name"]]
  
  #--references
  ref_count <- 0
  if (params$rollup_method == "iq_maxlfq") {
    ref_count <- 2
    ref1 <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/reference/rollup_maxlfq_reference.txt")
    ref2 <- read_replacement_text("/mnt/DocShare/Proteomics_Report_Templates/reference/rollup_iq_reference.txt")
    df[df$Parameter=="R_reference_txt1",]$Value <- stringr::str_c("(1) ", ref1)
    df[df$Parameter=="R_reference_txt2",]$Value <- stringr::str_c("(2) ", ref2)
  } 
  
  if (ref_count ==0) {
    df[df$Parameter=="R_reference_txt1",]$Value <- ""
    df[df$Parameter=="R_reference_txt2",]$Value <- ""
    df[df$Parameter=="R_reference_txt3",]$Value <- ""
  }
  
  if (ref_count ==1) {
    df[df$Parameter=="R_reference_txt2",]$Value <- ""
    df[df$Parameter=="R_reference_txt3",]$Value <- ""
  }
  
  if (ref_count ==2) {
    df[df$Parameter=="R_reference_txt3",]$Value <- ""
  }
  
  
  cat(file = stderr(), "Function template_parameters...end", "\n")
  return(df) 
}

