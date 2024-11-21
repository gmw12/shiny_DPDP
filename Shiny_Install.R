
cat(file = stderr(), "Shiny_install.R.... starting....", "\n")

package_list <- c('devtools', 'tidyr', 'httr', 'png', 'tidyverse', 'dplyr', 'fs', 'effsize',
                  'colourpicker', 'tibble', 'stringr', 'readxl', 'randomcoloR', 'gplots',
                  'ggpubr', 'rgl', 'pca3d', 'robustbase', 'cluster', 'factoextra',
                  'igraph', 'shiny', 'shinyWidgets', 'shinyFiles', 'rhandsontable', 
                  'shinyjs', 'shinyalert', 'DT', 'ggraph', 'imp4p', 'Peptides',
                  'flexdashboard', 'openxlsx', 'stringi', 'jsonlite', 'remotes', 
                  'BiocManager', 'rAmCharts', 'future', 'promises', 'miscTools', 'reticulate', 'iq',
                  'inflection', 'foreach', "doParallel", "shinybrowser", "RMySQL")


biocmanager_list = c('impute', 'topGO', 'clusterProfiler', 'GSEABase', 'rWikiPathways', 
                     'STRINGdb', 'limma', 'edgeR', 'pcaMethods', 'gridExtra', 'MASS', 'vsn',
                     'preprocessCore', 'org.Hs.eg.db', 'org.Mm.eg.db', 'org.Rn.eg.db')


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


#manual Viseago install
viseago_list1 = c('AnnotationDbi', 'AnnotationForge', 'biomaRt','DiagrammeR',   
                  'fgsea', 'GOSemSim', 'GO.db',   'topGO')

viseago_list2 = c('DT',' R.utils', 'dynamicTreeCut','heatmaply', 'igraph', 'plotly', 'dendextend',  'UpSetR' )

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
    #BiocManager::install(pack, dependencies = TRUE, lib = "/home/dpmsr/R/library_4.3") 
    BiocManager::install(pack, dependencies = TRUE) 
  }
}  

install_dir <- paste(getwd(), "/viseago", sep = "")
command_viseago <- paste("git clone https://forgemia.inra.fr/umr-boa/viseago.git ", install_dir, sep = "" )
system(command_viseago)

# build package (from R console) 
cat(file = stderr(), "viseago build....", "\n")
viseago_file <- devtools::build("viseago")

# install package (from R console)
cat(file = stderr(), "viseago install....", "\n")
install.packages(viseago_file, repos = NULL, type = "source", dependencies = TRUE)

cat(file = stderr(), "viseago cleanup....", "\n")
system(paste("rm -R ", install_dir, sep=""))
rm("install_dir", "command_viseago", "viseago_file")
cat(file = stderr(), "viseago complete....", "\n")

cat(file = stderr(), "Shiny_install.R.... github", "\n")

library(devtools)

devtools::install_github('omarwagih/rmotifx', dependencies = TRUE) 

devtools::install_github("jmwozniak/PTMphinder", dependencies = TRUE)

#BiocManager::install("STRINGdb", lib="/home/dpmsr/R/library")

#install_version("rvcheck", version = "0.1.8", repos = "http://cran.us.r-project.org")

cat(file = stderr(), "Shiny_install.R.... END", "\n")

#install.packages("RMySQL", dependencies = TRUE, lib = "/home/dpmsr/R/library_4.3")     