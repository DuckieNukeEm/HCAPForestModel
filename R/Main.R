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
 if(!("first.run.for.packs" %in% ls())) {first.run.for.packs = "Y"}
  
########################################################################
################                                        ################
################     Installing and loading the Packs   ################
################                                        ################
########################################################################
 
   if (first.run.for.packs == "Y") {
      pkg <- c("tree","randomForest","gbm")
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


vprint("reading in the variable list that list the dep and indep vars")  
  vars.hcap_r <- read.table(file.path(wddata,"hcap_r_names.csv"), sep = ",")

########################################################################
################                                        ################
################           running single trees         ################
################                                        ################
########################################################################
  
    
  
vprint("creating a names vector")    
  names.hcap_r <- names(hcap_r)
  

vprint("generating a tree on the selected variables")
  tree.formula <- create.formula(names.hcap_r[vars.hcap_r[,2] == 'd'], names.hcap_r[vars.hcap_r[,2] == 'i'])
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
    importance(tree.hcap_r)
    #basic COR matrix
    #10-fold cros validation instead of test/train -> 50
    #DSA
    
vprint("now creating a random forest")
    rtree.hcap_r = randomForest(tree.formula, data = hcap_r,  subset = hcap_r.train, importance = TRUE)
    
    
    result.data <- as.data.frame(matrix(0, ncol = 3, nrow = 1000))    
    names(result.data) <- c("MSE","mtry","tree.size")
    mtry.sample <- sample(1:16, 1000, replace = T)
    tree.sample <- sample(100:2000, 1000, replace = T)
    hcap_r.test = hcap_r[-hcap_r.train, names.hcap_r[vars.hcap_r[,2] == 'd']]
    hcap_r.testset = hcap_r[-hcap_r.train]
    i = 1
    for (i in 1:1000)
    {
      vprint(i)
      rtree.hcap_r = randomForest(tree.formula, data = hcap_r,  subset = hcap_r.train, mtry = mtry.sample[i], ntree=tree.sample[i])
      yhat.bag = predict(rtree.hcap_r, hcap_r.test)
      
      result.data[i,] = c(mean((yhat.hcap_r-hcap_r.test)^2), mtry.sample[i], tree.sample[i])
    }
    
    
    
    