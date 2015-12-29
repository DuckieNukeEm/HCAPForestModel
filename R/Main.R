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
  savedata <- file.path(wd,"Saved Objects")
  
########################################################################
################                                        ################
################     Defining Script Control Parms      ################
################                                        ################
########################################################################
    
 verbose = T #Do we want to explicity state what is going on or no
 VarCol = 6  
  
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
      pkg <- c("tree","randomForest","gbm","glmnet","data.table", "caret")
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
  #VarCol <- readline("Which Value cOlumn would you like to use: ")
  #VarCol <- as.numeric(VarCol) + 1
  
  
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

 depn <- names.hcap_r[vars.hcap_r[,VarCol] == 'd']
  
vprint("removing excessivley long factor variabls from Data.frames")
  factor.length = sapply(hcap_r,level_length)
  hcap_r = hcap_r[,factor.length <= 25]
  vprint("removing the following factors due to excessive level size")
  vars.hcap_r[factor.length > 25,1]
  vars.hcap_r <- vars.hcap_r[factor.length <= 25,]
  rm(factor.length)
  

vprint("Removing Depn Var Outliers")
  boxplot_output <- boxplot(hcap_r[,depn])
  vprint("removing the following outlier stores")
    (hcap_r[(hcap_r[,depn] %in% boxplot_output$out),"STORE_NBR"]  )
  hcap_r <- hcap_r[!(hcap_r[,depn] %in% boxplot_output$out),]
  rm(boxplot_output)

if(FALSE) { #change to true if you want to add HMLDepn var  
vprint("Adding in High Med Low Depn Var")
  hcap_r$HMLDepn <- "M"
  hcap_r[hcap_r[,depn]<=quantile(hcap_r[,depn],c(.1)),"HMLDepn"] <- "L"
  hcap_r[hcap_r[,depn]>=quantile(hcap_r[,depn],c(.9)),"HMLDepn"] <- "H"
  hcap_r[,"HMLDepn"] <- factor(hcap_r[,"HMLDepn"])
  vars.hcap_r[nrow(vars.hcap_r)+1,] <- c("HMLDepn",rep("i",ncol(vars.hcap_r)-1))
}
 
vprint("creating a names vector")    
  names.hcap_r <- names(hcap_r) 
  
  
  
########################################################################
################                                        ################
################           running single trees         ################
################                                        ################
########################################################################
 
vprint("generating a tree on the selected variables")
  tree.formula <- create.formula(names.hcap_r[vars.hcap_r[,VarCol] == 'd'], names.hcap_r[vars.hcap_r[,VarCol] == 'i'] )
  tree.hcap_r <- tree(tree.formula, data = hcap_r)
  
vprint("plotting the tree")  
  plot(tree.hcap_r)
  text(tree.hcap_r, pretty= 0)
  

vprint("Prune?")  
  if(FALSE)#change to false if we don't need to prune
    {
        vprint("seeing if we need to prune")  
        cv.hcap_r = cv.tree(tree.hcap_r) 
         plot(cv.hcap_r$size, cv.hcap_r$dev, type="b")
    
    
        prune.tree.hcap_r = prune.tree(tree.hcap_r, best = 4)
        plot(prune.tree.hcap_r)
        text(prune.tree.hcap_r, pretty = 0)
        tree.hcap_r <- prune.tree.hcap_r
  }

vprint("Test Validity?")
if(FALSE) #Change to True if you want to test validity
    {
    vprint("That's all fine and good, but let's test the validity of the tree")
      #Creating a training set
        hcap_r.train <- sample(1:nrow(hcap_r), nrow(hcap_r)/2)
      #building the tree with training set
        tree.train.hcap_r <-tree(tree.formula, data = hcap_r, subset = hcap_r.train)
      # predicting the values using the testing set (the other half)
        pred.hcap_r = predict(tree.hcap_r, newdata = hcap_r[-hcap_r.train,])
        actual.hcap_r = hcap_r[-hcap_r.train,which(vars.hcap_r[,VarCol] == 'd') ]
      #plotting the predicated values with the actual values
        plot(pred.hcap_r, actual.hcap_r)
        abline(0,1)
      #let's calculate the mean squared error
        mean((pred.hcap_r - actual.hcap_r)^2)
        # The sqrt root will tell use average diveregence
        sqrt(mean((pred.hcap_r - actual.hcap_r)^2))
        plot(tree.hcap_r)
        text(tree.hcap_r, pretty= 0)
}
starttime <- proc.time()
vprint("now creating a random forest")
    rtree.hcap_r = randomForest(tree.formula, data = hcap_r,  importance = TRUE)
    importance(rtree.hcap_r)
    varImpPlot(rtree.hcap_r)
    summary(lm_importance(rtree.hcap_r))
proc.time() - starttime

if(FALSE) {
tftr.hcap_r <- rfcv(hcap_r[,vars.hcap_r[,VarCol] == 'i'], hcap_r[,vars.hcap_r[,VarCol] == 'd'], cv.fold=10)

vprint("10-fold cross validation method")
   train_control <-  trainControl(method = 'cv', number = 10)
   cvrtree.hcap_r <- train(tree.formula, data = hcap_r, trControl = train_control, method = 'rf')
}
