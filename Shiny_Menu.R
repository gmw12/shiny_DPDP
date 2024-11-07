cat(file = stderr(), "Shiny_Menu.R", "\n")

#----------------------------------------------------------------------------------------- 
load_menu <- function(session, input, output) {
  cat(file = stderr(), "Function load_menu...", "\n")
  
  if (site_user != "dpmsr") {
    if (params$raw_data_format == "") {
      load_menu_start(session, input, output)
    }else if (params$raw_data_format == "precursor" & params$data_output == "Protein") {
      load_menu_customer_protein(session, input, output)
    }else if (params$raw_data_format == "precursor" & params$data_output == "Peptide") {
      load_menu_customer_peptide(session, input, output)
    }else if (params$raw_data_format == "protein") {
      load_menu_customer_protein(session, input, output)
    }
  }
  
  if (site_user == "dpmsr") {
    if (params$raw_data_format == "") {
      load_menu_start(session, input, output)
    }else if (params$raw_data_format == "precursor" & params$data_output == "Protein") {
      load_menu_precursor_to_protein(session, input, output)
    }else if (params$raw_data_format == "precursor" & params$data_output == "Peptide") {
      load_menu_peptide(session, input, output)
    }else if (raw_data_format == "protein") {
      load_menu_all(session, input, output)
    }
  }

  cat(file = stderr(), "Function load_menu...end", "\n")
}

#----------------------------------------------------------------------------------------- 
load_menu_start <- function(session, input, output) {
  
  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })
  
  output$menu_admin <- renderMenu({ 
    menuItem("Admin", tabName = "admin")
  })
  
}

#----------------------------------------------------------------------------------------- 
load_menu_precursor_to_protein <- function(session, input, output) {
  
  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })
  
  output$menu_parameters <- renderMenu({ 
    menuItem("Parameters", tabName = "parameters")
  })
  
  output$menu_noise <- renderMenu({ 
    menuItem("Noise", tabName = "noise")
  })
  
  output$menu_filter <- renderMenu({ 
    menuItem("Filter", tabName = "filter")
  })
  
  output$menu_normalize <- renderMenu({ 
    menuItem("Normalize", tabName = "normalize")
  })
  
  output$menu_impute <- renderMenu({ 
    menuItem("Impute", tabName = "impute")
  })
  
  output$menu_rollup <- renderMenu({ 
    menuItem("Rollup", tabName = "rollup")
  })
  
  output$menu_qc <- renderMenu({ 
    menuItem("QC", tabName = "qc")
  })
  
  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Protein Plots", tabName = "stats_protein_plots"))
  })
  
  output$menu_pathway <- renderMenu({ 
    menuItem("Pathway", tabName = "pathway", startExpanded = FALSE,
             menuItem("Setup", tabName = "pathway_setup"),
             menuItem("Wiki Pathways", tabName = "pathway_wiki"),
             menuItem("Go Profile", tabName = "pathway_go_profile"),
             menuItem("Go Analysis", tabName = "pathway_go_analysis"),
             menuItem("Go Volcano", tabName = "pathway_go_volcano"),
             menuItem("StringDB", tabName = "pathway_string"),
             menuItem("StringDB Enrich", tabName = "pathway_string_enrich")
    )
  })
  
  output$menu_admin <- renderMenu({ 
    menuItem("Admin", tabName = "admin")
  })
  
}

#----------------------------------------------------------------------------------------- 
load_menu_all <- function(session, input, output) {

  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })
  
  output$menu_parameters <- renderMenu({ 
    menuItem("Parameters", tabName = "parameters")
  })
  
  output$menu_noise <- renderMenu({ 
    menuItem("Noise", tabName = "noise")
  })
  
  output$menu_filter <- renderMenu({ 
    menuItem("Filter", tabName = "filter")
  })
  
  output$menu_normalize <- renderMenu({ 
    menuItem("Normalize", tabName = "normalize")
  })
  
  output$menu_impute <- renderMenu({ 
    menuItem("Impute", tabName = "impute")
  })
  
  output$menu_rollup <- renderMenu({ 
    menuItem("Rollup", tabName = "rollup")
  })
  
  output$menu_qc <- renderMenu({ 
    menuItem("QC", tabName = "qc")
  })
  
  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Protein Plots", tabName = "stats_protein_plots"))
  })
  
  output$menu_pathway <- renderMenu({ 
    menuItem("Pathway", tabName = "pathway", startExpanded = FALSE,
             menuItem("Setup", tabName = "pathway_setup"),
             menuItem("Wiki Pathways", tabName = "pathway_wiki"),
             menuItem("Go Profile", tabName = "pathway_go_profile"),
             menuItem("Go Analysis", tabName = "pathway_go_analysis"),
             menuItem("Go Volcano", tabName = "pathway_go_volcano"),
             menuItem("StringDB", tabName = "pathway_string"),
             menuItem("StringDB Enrich", tabName = "pathway_string_enrich")
    )
  })
  
  output$menu_phos <- renderMenu({ 
    menuItem("Phos", tabName = "phos", startExpanded = FALSE,
             menuItem("Setup", tabName = "phos_setup"),
             menuItem("Motif", tabName = "phos_motif"),
    )
  })
  
  output$menu_admin <- renderMenu({ 
    menuItem("Admin", tabName = "admin")
  })

}

#----------------------------------------------------------------------------------------- 
load_menu_peptide <- function(session, input, output) {
  
  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })
  
  output$menu_parameters <- renderMenu({ 
    menuItem("Parameters", tabName = "parameters")
  })
  
  output$menu_noise <- renderMenu({ 
    menuItem("Noise", tabName = "noise")
  })
  
  output$menu_filter <- renderMenu({ 
    menuItem("Filter", tabName = "filter")
  })
  
  output$menu_normalize <- renderMenu({ 
    menuItem("Normalize", tabName = "normalize")
  })
  
  output$menu_impute <- renderMenu({ 
    menuItem("Impute", tabName = "impute")
  })
  
  output$menu_qc <- renderMenu({ 
    menuItem("QC", tabName = "qc")
  })
  
  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Peptide Plots", tabName = "stats_peptide_plots"))
  })

  output$menu_phos <- renderMenu({ 
    menuItem("Phos", tabName = "phos", startExpanded = FALSE,
             menuItem("Fasta", tabName = "phos_setup"),
             menuItem("Motif", tabName = "phos_motif"),
             menuItem("MEME_Momo", tabName = "phos_momo"))
  })
  
  output$menu_admin <- renderMenu({ 
    menuItem("Admin", tabName = "admin")
  })
  
}

#---------------------------------

#----------------------------------------------------------------------------------------- 
load_menu_protein_inpute <- function(session, input, output) {
  
  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })
  
  output$menu_parameters <- renderMenu({ 
    menuItem("Parameters", tabName = "parameters")
  })
  
  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Protein Plots", tabName = "stats_protein_plots"))
  })
  
  output$menu_pathway <- renderMenu({ 
    menuItem("Pathway", tabName = "pathway", startExpanded = FALSE,
             menuItem("Setup", tabName = "pathway_setup"),
             menuItem("Wiki Pathways", tabName = "pathway_wiki"),
             menuItem("Go Profile", tabName = "pathway_go_profile"),
             menuItem("Go Analysis", tabName = "pathway_go_analysis"),
             menuItem("Go Volcano", tabName = "pathway_go_volcano"),
             menuItem("StringDB", tabName = "pathway_string"),
             menuItem("StringDB Enrich", tabName = "pathway_string_enrich")
    )
  })
  
  output$menu_admin <- renderMenu({ 
    menuItem("Admin", tabName = "admin")
  })
  
}

#----------------------------------------------------------------------------------------- 
load_menu_customer_protein <- function(session, input, output) {
  
  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Protein Plots", tabName = "stats_protein_plots"))
  })
  
  output$menu_pathway <- renderMenu({ 
    menuItem("Pathway", tabName = "pathway", startExpanded = FALSE,
             menuItem("Setup", tabName = "pathway_setup"),
             menuItem("Wiki Pathways", tabName = "pathway_wiki"),
             menuItem("Go Profile", tabName = "pathway_go_profile"),
             menuItem("Go Analysis", tabName = "pathway_go_analysis"),
             menuItem("Go Volcano", tabName = "pathway_go_volcano"),
             menuItem("StringDB", tabName = "pathway_string"),
             menuItem("StringDB Enrich", tabName = "pathway_string_enrich")
    )
  })

  
}
#----------------------------------------------------------------------------------------- 
load_menu_customer_peptide <- function(session, input, output) {
  
  output$menu_load <- renderMenu({ 
    menuItem("Load", tabName = "load")
  })

  output$menu_stats <- renderMenu({ 
    menuItem("Stats", tabName = "stats", startExpanded = FALSE,
             menuItem("Setup", tabName = "stats_setup"),
             menuItem("Comparisons", tabName = "stats_compare"),
             menuItem("Graphs", tabName = "stats_plots"),
             menuItem("Data", tabName = "stats_data"),
             menuItem("Peptide Plots", tabName = "stats_peptide_plots"))
  })
  
  output$menu_phos <- renderMenu({ 
    menuItem("Phos", tabName = "phos", startExpanded = FALSE,
             menuItem("Fasta", tabName = "phos_setup"),
             menuItem("Motif", tabName = "phos_motif"),
             menuItem("MEME_Momo", tabName = "phos_momo"))
  })
  

}