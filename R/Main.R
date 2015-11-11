########################################################################
################                                        ################
################      Setting the Working Directory     ################
################                                        ################
########################################################################
  if(file.exists("c:/scripts/R/HCAPForestModel.R"))
  {source("c:/scripts/R/HCAPForestModel.R")}
  wd <- getwd()
  wdcode <- file.path(wd, 'R')
  wddata <- file.path(wd, 'Data')
  
########################################################################
################                                        ################
################     Defining Script Control Parms      ################
################                                        ################
########################################################################
  
 verbose = T #Do we want to explicity state what is going on or no
 if(!("first.run.for.packs" %in% ls())) {first.run.for.packs = "Y"}
########################################################################
################                                        ################
################     Installing and loading the Packs   ################
################                                        ################
########################################################################
  if (first.run.for.packs == "Y") {
      pkg <- c("tree")
      inst <- pkg %in% installed.packages()  
      if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
      lapply(pkg,library,character.only=TRUE)  
      rm(inst,pkg)  
      first.run.for.packs = "N"}
########################################################################
################                                        ################
################       Reading in Varios Functions      ################
################                                        ################
########################################################################
  
  source(file.path(wdcode, "HCAPFunctions.R"))
  
    
########################################################################
################                                        ################
################           Reading In the Data          ################
################                                        ################
########################################################################

vprint("Reading in the the Reduced file data")  
  hcap_r <- read.table(file.path(wddata, "FinalOutPutforTree-Reduced.csv"), header = T, sep = "|", stringsAsFactors = T)

vpring("creating a names vector")    
  names.hcap_r <- names(hcap_r)
  
vprint("generating a tree on all the variables")
  tree.hcap_r <- tree()
