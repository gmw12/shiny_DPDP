
cat(file = stderr(), "Shiny_install.R.... starting....", "\n")

package_list <- c('devtools', 'tidyr', 'httr', 'png', 'tidyverse', 'dplyr', 'fs', 'effsize',
                  'colourpicker', 'tibble', 'stringr', 'readxl', 'randomcoloR', 'gplots',
                  'ggpubr', 'rgl', 'pca3d', 'robustbase', 'cluster', 'factoextra',
                  'igraph', 'shiny', 'shinyWidgets', 'shinyFiles', 'shinyBS', 'rhandsontable', 
                  'shinyjs', 'shinyalert', 'DT', 'ggraph', 'imp4p', 'Peptides',
                  'flexdashboard', 'openxlsx', 'stringi', 'jsonlite', 'remotes', 
                  'BiocManager', 'rAmCharts', 'future', 'promises', 'miscTools', 'reticulate', 'iq',
                  'inflection', 'foreach', "doParallel", "shinybrowser", "RSQLite")


biocmanager_list = c('impute', 'topGO', 'clusterProfiler', 'GSEABase', 'rWikiPathways', 
                     'STRINGdb', 'limma', 'edgeR', 'pcaMethods', 'gridExtra', 'MASS', 'vsn',
                     'preprocessCore', 'org.Hs.eg.db', 'org.Mm.eg.db', 'org.Rn.eg.db', 'ViSEAGO')


cat(file = stderr(), "Shiny_install.R.... package_list", "\n")
# loop to install require packages
for (pack in package_list) {
  print(pack)
  if (pack %in% rownames(installed.packages())) {   
    print("not installing")
  }else{
    print("installing")
    #install.packages(pack, dependencies = TRUE, lib = "/home/R_Packages") 
    install.packages(pack, dependencies = TRUE) 
  }
}

library(devtools)

cat(file = stderr(), "Shiny_install.R.... bioconductor_list", "\n")
#loop to install required BioConductor packages

cat(file = stderr(), "Shiny_install.R.... bioconductor_list", "\n")
#loop to install required BioConductor packages
for (pack in biocmanager_list) {
  print(pack)
  if (pack %in% rownames(installed.packages())) {   
    print("not installing")
  }else{
    print("installing")
    #BiocManager::install(pack, dependencies = TRUE, lib = "/home/dpmsr/R/library_4.3") 
    BiocManager::install(pack, dependencies = TRUE) 
  }
}   


#-------------------------------------------------------------------------------------------
if ('ViSEAGO' %in% rownames(installed.packages())) {
  print("not installing")
}else {
  #manual Viseago install
  viseago_list1 = c('AnnotationDbi', 'AnnotationForge', 'biomaRt','DiagrammeR',   
                    'fgsea', 'GOSemSim', 'GO.db',   'topGO')
  
  viseago_list2 = c('R.utils', 'dynamicTreeCut','heatmaply', 'igraph', 'plotly', 'dendextend',  'UpSetR')
  
  cat(file = stderr(), "Shiny_viseago_install.R.... package_list", "\n")
  # loop to install require packages
  for (pack in viseago_list2) {
    print(pack)
    if (pack %in% rownames(installed.packages())) {   
      print("not installing")
    }else{
      print("installing")
      install.packages(pack, dependencies = TRUE) 
    }
  }
  
  for (pack in viseago_list1) {
    print(pack)
    if (pack %in% rownames(installed.packages())) {   
      print("not installing")
    }else{
      print("installing")
      BiocManager::install(pack, dependencies = TRUE) 
    }
  }  
  
  install_dir <- paste(getwd(), "/viseago", sep = "")
  command_viseago <- paste("git clone https://forgemia.inra.fr/umr-boa/viseago.git ", install_dir, sep = "" )
  system(command_viseago)
  
  # build package (from R console) 
  cat(file = stderr(), "viseago build....gw", "\n")
  viseago_file <- devtools::build("viseago", vignettes = FALSE)
  
  # install package (from R console)
  cat(file = stderr(), "viseago install....", "\n")
  install.packages(viseago_file, repos = NULL, type = "source", dependencies = TRUE)
  
  cat(file = stderr(), "viseago cleanup....", "\n")
  cat(file = stderr(), "viseago complete....", "\n")
  
}
#--------------------------------------------------

cat(file = stderr(), "Shiny_install.R.... github", "\n")


devtools::install_github('omarwagih/rmotifx', dependencies = TRUE) 

devtools::install_github("jmwozniak/PTMphinder", dependencies = TRUE)

cat(file = stderr(), "Shiny_install.R.... END", "\n")