create_report <- function(session, input, output, db_path) {
  cat(file = stderr(), "Function create_report...", "\n")
  library(readr)
  library(stringr)
  library(readxl)
  
  source('Shiny_Report_Functions.R')
  source('Shiny_PamLims.R')
  
  params <- get_params(db_path)

  # Paths
  base_path <- "/mnt/DocShare/Proteomics_Report_Templates"

  #template  doc_template = str_c(base_path, "/base_templates/", "proteomics_report_protein_SP_v1.docx")
  doc_template <- stringr::str_c(base_path, "/base_templates/", input$report_template)
  output_file <- stringr::str_c(params$data_path, input$report_filename)

  excel_filename <- gsub(".docx", ".xlsx", doc_template)
  df_template <- read_excel(excel_filename, col_names = TRUE)
  
  df_template_base <- df_template[!grepl("R_", df_template$key), ]
  #remove rows with NA in df_template_base$source
  df_template_base <- df_template_base[!is.na(df_template_base$source), ]
  #order so that any row with "base_" in the key comes first
  df_template_base <- df_template_base[order(grepl("base_", df_template_base$key, ignore.case = TRUE), decreasing = TRUE), ]
  
  df_template_R <- df_template[grepl("R_", df_template$key), ]
  
  # Copy template
  file.copy(doc_template, output_file, overwrite = TRUE)
  
  #----
  cat(file = stderr(), "Function create_report...1", "\n")
  
  replacements <- c()  
  for (i in 1:nrow(df_template_base)) {
    replacements <- c(replacements, read_replacement_text(df_template_base$source[i]))
  }
  
  replacement_names <- c()
  for (i in 1:nrow(df_template_base)) {
    replacement_names <- c(replacement_names, stringr::str_c("{{", df_template_base$key[i] ,"}}"))
  }
  
  names(replacements) <- replacement_names
  
  # Single call to replace everything
  cat("Processing", length(replacements), "replacements...\n")
  replace_text_placeholders_batch_robust(output_file, output_file, replacements)
  
  #-----
  cat(file = stderr(), "Function create_report...2", "\n")
  
  df_R_parameters <- template_parameters(session, input, output, db_path, df_template_R)
  
  test_df_R_parameters <<- df_R_parameters  #df_R_parameters <- test_df_R_parameters
  
  replacements <- c()  
  for (i in 1:nrow(df_R_parameters)) {
    replacements <- c(replacements, df_R_parameters$Value[i])
  }
  
  replacement_names <- c()
  for (i in 1:nrow(df_R_parameters)) {
    replacement_names <- c(replacement_names, stringr::str_c("{{", df_R_parameters$Parameter[i] ,"}}"))
  }
  
  names(replacements) <- replacement_names
  
  cat("Processing", length(replacements), "replacements...\n")
  replace_text_placeholders_batch_robust(output_file, output_file, replacements)
  
  cat("\n✓ Document complete:", output_file, "\n")
  
  cat(file = stderr(), "Function create_report...end", "\n")
}


#-----------------------------------------------------------------------------------
create_report_old <- function(session, input, output, db_path) {
  cat(file = stderr(), "Function create_report...", "\n")
  library(readr)
  library(stringr)
  
  source('Shiny_Report_Functions.R')
  source('Shiny_PamLims.R')
  
  params <- get_params(db_path)
  
  # Paths
  base_path <- "/mnt/DocShare/Proteomics_Report_Templates"
  report_path <- file.path(base_path, "base_templates")
  sample_prep_path <- file.path(base_path, "sample_prep")
  instrument_methods_path <- file.path(base_path, "instrument_methods")
  filters_path <- file.path(base_path, "filters")
  data_analysis_path <- file.path(base_path, "data_analysis")
  results_path <- file.path(base_path, "results")
  reference_path <- file.path(base_path, "reference")
  
  doc_template <- file.path(report_path, "proteomics_report_v1.docx")
  output_file <- file.path(base_path, "report_test.docx")
  
  # Copy template
  file.copy(doc_template, output_file, overwrite = TRUE)
  
  # Build replacements list directly
  replacements <- list(
    "{{sample_prep_txt}}" = read_replacement_text(file.path(sample_prep_path, "strap_standard.txt")),
    "{{tip_loading_txt}}" = read_replacement_text(file.path(instrument_methods_path, "tip_loading_peptide_quant.txt")),
    "{{ms_method_txt}}" = read_replacement_text(file.path(instrument_methods_path, "spd30_dia.txt")),
    "{{filter_intro_txt}}" = read_replacement_text(file.path(filters_path, "filter_intro.txt")),
    "{{exclude_norm_txt}}" = read_replacement_text(file.path(data_analysis_path, "exclude_norm.txt")),
    "{{norm_impute_strategy_txt}}" = read_replacement_text(file.path(data_analysis_path, "norm_sltmm.txt")),
    "{{basic_stats_txt}}" = read_replacement_text(file.path(results_path, "results_protein_precursor_counts.txt")),
    "{{initial_analysis_comment_txt}}" = read_replacement_text(file.path(results_path, "initial_analysis_comment_protein.txt")),
    "{{imputation_strategy_txt}}" = read_replacement_text(file.path(data_analysis_path, "imputation_duke.txt")),
    "{{rollup_strategy_txt}}" = read_replacement_text(file.path(data_analysis_path, "rollup_maxlfq.txt")),
    "{{reference_txt1}}" = read_replacement_text(file.path(reference_path, "rollup_maxlfq_reference.txt")),
    "{{reference_txt2}}" = read_replacement_text(file.path(reference_path, "rollup_iq_reference.txt")),
    "{{reference_txt3}}" = read_replacement_text(file.path(reference_path, "empty_reference.txt"))
  )
  
  # Single call to replace everything
  cat("Processing", length(replacements), "replacements...\n")
  replace_text_placeholders_batch_robust(output_file, output_file, replacements)
  
  
  df_cv <- read_table('protein_sltmm_cv', db_path)
  cv_spqc <- mean(df_cv[, grepl("spqc", names(df_cv), ignore.case = TRUE)])
  cv_samples <- mean(unlist(df_cv[, !grepl("spqc", names(df_cv), ignore.case = TRUE) & 
                                    grepl("_CV", names(df_cv), ignore.case = TRUE)]))
  rawfile <- get_param("raw_data_paths", db_path)
  project_number <- sub(".*/RawData/(\\d+)/.*", "\\1", rawfile)
  
  google_data_list <- get_google_data(project_number)
  resuspend_volume <- google_data_list[["resuspend_volume"]]
  order_id <- unlist(google_data_list[["order_id"]])
  sample_amount <- google_data_list[["sample_amount"]]
  #if sample_amount is a number then multiply by 100 and add "%"
  if (!is.na(as.numeric(sample_amount))) {
    sample_amount <- paste0(as.numeric(sample_amount) * 100, "%")
  }
  sample_groups <- read_table('sample_groups', db_path)
  #filter sample_groups to remove rows where sample_groups$Group is "SPQC" case insensitive
  sample_groups_filtered <- sample_groups[!grepl("^SPQC$", sample_groups$Group, ignore.case = TRUE), ]
  sample_number <- nrow(sample_groups_filtered)
  sample_descriptions <- format_sample_counts(sample_groups_filtered)
  staff_contribution <- create_staff_contribution(session, input, output)
  pamlims_data_list <- get_pamlims_data(order_id)
  report_type <- input$report_type
  #today date in MMDDYY format
  today_date <- format(Sys.Date(), "%m%d%y")
  report_file_name <- stringr::str_c(project_number, "_Report_", today_date, ".pdf")
  excel_file_name <- stringr::str_c(project_number, "_SupplementalData_",toupper(params$stat_norm), "_", today_date, ".xlsx")
  
  if (params$noise_type !="none") {
    filter_noise <- read_replacement_text(file.path(filters_path, "filter_noise.txt"))
  } else {
    filter_noise <- ""
  }
  
  if (params$filter_x_percent) {
    filter_2_50 <- read_replacement_text(file.path(filters_path, "filter_2_50.txt"))
  } else {
    filter_2_50 <- ""
  }
  
  if (params$checkbox_misaligned) {
    filter_misalign <- read_replacement_text(file.path(filters_path, "filter_misalign.txt"))
  } else {
    filter_misalign <- ""
  }
  
  if (params$precursor_quality) {
    filter_precursor_quality <- read_replacement_text(file.path(filters_path, "filter_precursor_quality.txt"))
  } else {
    filter_precursor_quality <- ""
  }
  
  if (params$precursor_spqc_ratio) {
    filter_spqc <- read_replacement_text(file.path(filters_path, "filter_spqc.txt"))
  } else {
    filter_spqc <- ""
  }
  
  df_info <- extract_method_info(db_path)
  
  replacements <- list(
    "{{ms1_resolution}}" = df_info$Value[df_info$Label == "Scan 1 - FT Resolution:"],
    "{{ms1_scan}}" = stringr::str_c(df_info$Value[df_info$Label == "Scan 1 - massRange"][1], "-", 
                                    df_info$Value[df_info$Label == "Scan 1 - massRange"][2]),
    "{{ms1_agc}}" = df_info$Value[df_info$Label == "Scan 1 - AGC Target:"],
    "{{dia_window}}" = as.character(round(as.numeric(df_info$Value[df_info$Label == "DIA window"]), digits = 1)),
    "{{dia_range}}" = stringr::str_c(df_info$Value[df_info$Label == "DIA range1"], "-", 
                                     df_info$Value[df_info$Label == "DIA range2"]),
    "{{ms2_agc}}" = df_info$Value[df_info$Label == "Scan 2 - AGC Target:"],
    "{{ms2_fill}}" = as.character(round(as.numeric(df_info$Value[df_info$Label == "Scan 2 - Max. Ion Time (ms):"]), digits = 0)),
    "{{hcd_energy}}" = as.character(round(as.numeric(df_info$Value[df_info$Label == "Scan 2 - HCD Energy:"]), digits = 0)),
    "{{run_time}}" = as.character(round(as.numeric(df_info$Value[df_info$Label == "Time range"][2]), digits=0)),
    "{{injection_count}}" = params$sample_number,
    "{{precursor_count}}" = format(nrow(read_table('precursor_filter', db_path)), big.mark = ","),
    "{{protein_count}}" = format(nrow(read_table('protein_impute', db_path)), big.mark = ","),
    "{{total_tables}}" = (4 + as.numeric(params$comp_number)),
    "{{imputation_percent}}" = params$bottom_x,
    "{{sample_cv}}" = round(cv_samples, digits = 0),
    "{{spqc_cv}}" = round(cv_spqc, digits = 0),
    "{{today_date}}" = trimws(format(Sys.Date(), "%B %e, %Y")),
    "{{project_number}}" = project_number,
    "{{resuspend_volume}}" = resuspend_volume,
    "{{sample_amount}}" = sample_amount,
    "{{sample_number}}" = sample_number,
    "{{sample_descriptions}}" = sample_descriptions,
    "{{staff_contribution}}" = staff_contribution,
    "{{customer_name}}" = pamlims_data_list[["customer_name"]],
    "{{pi_name}}" = pamlims_data_list[["pi_name"]],
    "{{customer_lastname}}" = pamlims_data_list[["customer_lastname"]],
    "{{pi_lastname}}" = pamlims_data_list[["pi_lastname"]],
    "{{project_name}}" = pamlims_data_list[["project_name"]],
    "{{filter_noise}}" = filter_noise,
    "{{filter_2_50}}" = filter_2_50,
    "{{filter_misalign}}" = filter_misalign,
    "{{filter_precursor_quality}}" = filter_precursor_quality,
    "{{filter_spqc}}" = filter_spqc,
    "{{report_type}}" = report_type,
    "{{report_file_name}}" = report_file_name,
    "{{excel_file_name}}" = excel_file_name,
    "{{dukebox_folder}}" = input$dukebox_folder
  )
  
  
  cat("Processing", length(replacements), "replacements...\n")
  replace_text_placeholders_batch_robust(output_file, output_file, replacements)
  
  cat("\n✓ Document complete:", output_file, "\n")
  
  cat(file = stderr(), "Function create_report...end", "\n")
}
