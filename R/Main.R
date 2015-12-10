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
  
  
  #only want to test the packages at first running of the script    
 if(!("first.run.for.packs" %in% ls())) {
   Starting.R.Var <- c(ls(),"Starting.R.Var")
   first.run.for.packs = "Y"}
  
########################################################################
################                                        ################
################     Installing and loading the Packs   ################
################                                        ################
########################################################################
 
   if (first.run.for.packs == "Y") {
      pkg <- c("tree","randomForest","gbm","glmnet")
      inst <- pkg %in% installed.packages()  
      if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
      lapply(pkg,library,character.only=TRUE)  
      rm(inst,pkg)  
      first.run.for.packs = "N"}
  
########################################################################
################                                        ################
################       Reading in Varios Functions      ################
################                                        ################
#######################################################################
  
  source(file.path(wdcode, "HCAPFunctions.R"))
  
    
########################################################################
################                                        ################
################           Reading In the Data          ################
################                                        ################
########################################################################

if(FALSE)  
{vprint("reading in Reduced file Data")
  hcap_r <- read.table(file.path(wddata, "FinalOutPutforTree-Reduced.csv"), header = T, sep = "|", stringsAsFactors = T)
  vars.hcap_r <- read.table(file.path(wddata,"hcap_r_names.csv"), sep = ",",header = T)
} else {
    vprint("Reading in the full file data")
  hcap_r <- read.csv(file.path(wddata, "FinalOutPutforTree.csv"), header = T, sep = "|", stringsAsFactors = T)
  vars.hcap_r <- read.csv(file.path(wddata,"hcap_names.csv"), sep = ",",header = T, as.is = T)
  
   }

#######################################################################
###############                                        ################
###############             cleaning the data          ################
###############                                        ################
#######################################################################  

 
  
vprint("removing excessivley long factor variabls from Data.frames")
  factor.length = sapply(hcap_r,level_length)
  hcap_r = hcap_r[,factor.length <= 25]
  vprint("removing the following factors due to excessive level size")
  vars.hcap_r[factor.length > 25,1]
  vars.hcap_r <- vars.hcap_r[factor.length <= 25,]
  rm(factor.length)
  
vprint("creating a names vector")    
  names.hcap_r <- names(hcap_r) 
  depn <- names.hcap_r[vars.hcap_r[,2] == 'd']

vprint("Removing Depn Var Outliers")
  boxplot_output <- boxplot(hcap_r[,depn])
  hcap_r <- hcap_r[!(hcap_r[,depn] %in% boxplot_output$out),]
  rm(boxplot_output)
  
vprint("Adding in High Med Low Depn Var")
  hcap_r$HMLDepn <- "M"
  hcap_r[hcap_r[,depn]<=quantile(hcap_r[,depn],c(.1)),"HMLDepn"] <- "L"
  hcap_r[hcap_r[,depn]>=quantile(hcap_r[,depn],c(.9)),"HMLDepn"] <- "H"
  hcap_r[,"HMLDepn"] <- factor(hcap_r[,"HMLDepn"])
  vars.hcap_r[nrow(vars.hcap_r)+1,] <- c("HMLDepn",rep("i",ncol(vars.hcap_r)-1))
vprint("Adding in ")
########################################################################
################                                        ################
################           running single trees         ################
################                                        ################
########################################################################
 
vprint("generating a tree on the selected variables")
  tree.formula <- create.formula(names.hcap_r[vars.hcap_r[,2] == 'd'], names.hcap_r[vars.hcap_r[,2] == 'i'] )
  tree.hcap_r <- tree(tree.formula, data = hcap_r)
  
vprint("plotting the tree")  
  plot(tree.hcap_r)
  text(tree.hcap_r, pretty= 0)
  
vprint("seeing if we need to prune")  
  cv.hcap_r = cv.tree(tree.hcap_r) 
  plot(cv.hcap_r$size, cv.hcap_r$dev, type="b")
  
vprint("if we need to prune")  
  if(TRUE)#change to false if we don't need to prune
    {
        prune.tree.hcap_r = prune.tree(tree.hcap_r, best = 4)
        plot(prune.tree.hcap_r)
        text(prune.tree.hcap_r, pretty = 0)
        tree.hcap_r <- prune.tree.hcap_r
  }


vprint("That's all fine and good, but let's test the validity of the tree")
  #Creating a training set
    hcap_r.train <- sample(1:nrow(hcap_r), nrow(hcap_r)/2)
  #building the tree with training set
    tree.train.hcap_r <-tree(tree.formula, data = hcap_r, subset = hcap_r.train)
  # predicting the values using the testing set (the other half)
    pred.hcap_r = predict(tree.hcap_r, newdata = hcap_r[-hcap_r.train,])
    actual.hcap_r = hcap_r[-hcap_r.train,which(vars.hcap_r[,2] == 'd') ]
  #plotting the predicated values with the actual values
    plot(pred.hcap_r, actual.hcap_r)
    abline(0,1)
  #let's calculate the mean squared error
    mean((pred.hcap_r - actual.hcap_r)^2)
    # The sqrt root will tell use average diveregence
    sqrt(mean((pred.hcap_r - actual.hcap_r)^2))
    plot(tree.hcap_r)
    text(tree.hcap_r, pretty= 0)
  
    #basic COR matrix
    #10-fold cros validation instead of test/train -> 50
    #DSA
    #send Alberto An introduction to Statistical Learning
vprint("now creating a random forest")
    rtree.hcap_r = randomForest(tree.formula, data = hcap_r,  importance = TRUE)
    importance(rtree.hcap_r)
    varImpPlot(rtree.hcap_r)
    
   
    #Next Steps
      #Reduced Correlated Variables
    #10 - fold cross validation method
    #external => Internal 
    #glmNet, (R- Package, looks at reducing variable space)
      #random forest first then glmnet
      #or both then cross validation and test MSE
    
    #good source from Berkley
    
  #2015-11-23  
    #create a variable that indicates if a store NSLEPerSqr Foot is outlyer High, nor, low
          # <10% is lower
          #higher then 90% is high
          #exclude the two highest two stores.
  #2015-11-30
    #run correlation matrix on external var used
    #Save the Tree/Plots/Corr and pass them to WMA
    #get gmlNet to work for external variables | gaussian
