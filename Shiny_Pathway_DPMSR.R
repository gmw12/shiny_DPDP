cat(file = stderr(), "Shiny_Pathway_DPMSR.R", "\n")
# downloads and saves Pathway files, does not need to be done every analysis.  Files saved with *.R files
#----------------------------------------------------------------------------------------- 

get_pathway_files(params)

#-------------------------------------------------------------------------------------------------------------  
get_pathway_files <- function(params) {
  cat(file = stderr(), "Function get_pathway_files...", "\n")
  #showModal(modalDialog("Downloading Pathway Files...", footer = NULL))  
  
  arg_list <- list(params)
  
  bg_get_pathway_files <- callr::r_bg(func = get_pathway_files_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_get_pathway_files.txt"), supervise = TRUE)
  bg_get_pathway_files$wait()
  print_stderr("error_get_pathway_files.txt", db_path)
  
  cat(file = stderr(), "Function get_pathway_files...end", "\n")
  #removeModal()
  
}
#-----------------------------------------------------------------------------------------------------------------

get_pathway_files_bg <- function(params) {
  cat(file = stderr(), "Function get_pathway_files_bg...", "\n")

  #listOrganisms()
  #string_species <- get_STRING_species(version=10)
  # look up tax ID https://www.ncbi.nlm.nih.gov/taxonomy

  library(org.Hs.eg.db) 
  library(org.Mm.eg.db)
  library(org.Rn.eg.db)
  
  tax_list <- c("Human", "Mouse", "Rat")
  tax_db_list <- c(org.Hs.eg.db, org.Mm.eg.db, org.Rn.eg.db)
  organism_list <- c("Homo sapiens", "Mus musculus", "Rattus norvegicus")
  go_list <- c("human", "mouse", "rat")
  base_dir <- '/home/greg/Shiny/shiny_DPDP/'
  
  for (i in (1:length(tax_list))){
    
    tax_choice <- tax_list[i]
    tax_db <- tax_db_list[i]
    organism_choice <- organism_list[i]
    go_choice <- go_list[i]
    
    cat(file=stderr(), stringr::str_c(tax_choice, " ", organism_choice, " ", go_choice), "\n" , "\n")
    
    wp.gmt <- rWikiPathways::downloadPathwayArchive(date = "20240910", organism=organism_choice, format = "gmt", destpath = base_dir)
    wp2gene <- clusterProfiler::read.gmt(stringr::str_c(base_dir, wp.gmt))
    
    if (version$major < 4){
      wp2gene <- wp2gene |> tidyr::separate(ont, c("name","version","wpid","org"), "%")
    }else {
      wp2gene <- try(wp2gene |> tidyr::separate(term, c("name","version","wpid","org"), "%") )
    }
    
    Uniprot <- ViSEAGO::Uniprot2GO()
    myGENE2GO <- ViSEAGO::annotate(go_choice, Uniprot)
    
    #save(wp2gene, file=stringr::str_c(base_dir,"Pathway_wp2gene_",tax_choice))
    #save(myGENE2GO, file=stringr::str_c(base_dir,"Pathway_myGENE2GO_",tax_choice))
    
  }
  cat(file = stderr(), "Function get_pathway_files_bg...end", "\n")
}
