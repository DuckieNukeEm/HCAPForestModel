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
 VarCol =  6 
  
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
      pkg <- c("tree",
               "gbm","glmnet","data.table", 
               "caret","readr","Boruta","rFerns",
               "forestFloor","dplyr","sparcl","randomForest",
               "archetypes")
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
  vars.hcap_r <- read.table(file.path(wddata,"hcap_r_names_term.csv"), sep = ",",header = T)
} else {
    vprint("Reading in the full file data")
 hcap_r <- read.csv(file.path(wddata, "FinalOutPutforTree.csv"), header = T, sep = "|", stringsAsFactors = T)
 vars.hcap_r <- read.csv(file.path(wddata,"hcap_names_Term.csv"), sep = ",",header = T, as.is = T)

   }

#######################################################################
###############                                        ################
###############             cleaning the data          ################
###############                                        ################
#######################################################################  
 names.hcap_r <- names(hcap_r) 
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
  
  
  vpring("Removing Stores where Shrink is Positive")
  vprint("The following stores have positive shrink and will be removed")
  (hcap_r[hcap_r$ShrinkPerSqrFoot > 0,]$STORE_NBR)
  hcap_r = hcap_r[hcap_r$ShrinkPerSqrFoot <= 0, ]
}
 
if(FALSE) {#Change to true if you want to add AvrgHoursWorked var
  hcap_r$AvgHours <- (hcap_r$Total.Hours.Listed - hcap_r$VacationHours - hcap_r$Holiday.Hours - hcap_r$Sick.Hours - hcap_r$PTO.Hours - hcap_r$OverTime.Hours)/hcap_r$SumWinCount
  if(!("AvgHours" %in% vars.hcap_r[,1])) {
  vars.hcap_r[nrow(vars.hcap_r)+1,] <- c("AvgHours",rep("i",ncol(vars.hcap_r)-1))
  }
}
  
if(TRUE){ #fixing the 0 to 6mo and 15yr+ tenure group
  hcap_r$X.6moPerTenure = hcap_r$X90.6moPerTenure + hcap_r$X.90PerTenure 
  hcap_r$X.6moPerTenureSqr = hcap_r$X.6moPerTenure*hcap_r$X.6moPerTenure
  hcap_r$X.15yrPerTenure = hcap_r$X16.20yPerTenure+ hcap_r$X20.yPerTenure 
  hcap_r$X.15yrPerTenureSqr = hcap_r$X.15yrPerTenure*hcap_r$X.15yrPerTenure
  
}
  
if(FALSE) {#changing ZIPMonths into binomal
  hcap_r$ZPMonthsin2013 <- ifelse(hcap_r$ZPMonthsin2013 > 0,1,0)
  
  }
vprint("creating a names vector")    
  names.hcap_r <- names(hcap_r) 
  
  
  
########################################################################
################                                        ################
################           running single trees         ################
################                                        ################
########################################################################
 
  if(FALSE) #change to true to du an hclust analysisi
  {
   # hcap_r = hcap_r_bck
    
    
      stdize = function(x){
        x_mean = mean(x)
        x_sd = sd(x)
        return((x - x_mean)/x_sd)
        }
    
      hcap_r_std = data.frame(apply(
        hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']], 2,
        stdize))
      
      
    dd = dist(hcap_r_std)
    hcap_r.hclust = hclust(dd, method = "ward.D2")
    plot(hcap_r.hclust)
    hcap_r$clusterGroup = cutree(hcap_r.hclust,5)
    library(sparcl)
    ColorDendrogram(hcap_r.hclust, y = cutree(hcap_r.hclust,5), labels = names(cutree(hcap_r.hclust,5)), branchlength = 80)
    names.hcap_r <- names(hcap_r) 
    t = aggregate(hcap_r[,c("AvgAge",names.hcap_r[vars.hcap_r[,VarCol] == 'i'])], by = list(hcap_r$clusterGroup), mean)
    hcap_r_bck = hcap_r
    hcap_r_bck$res = 0
    
    if(FALSE){ #Messing around with Archtatypal analysisi
      #Base function for doing an Archetype
        a = archetypes(data = hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']], 5, verbose = T )
      #stepArchetypes allowes the archetypes above to be run n times (nrep) to help fix the local minima issues
      #additionally, k can be specified as a vector, so you can work through various k, then use a skree plot to determine the
      #best k  
        a = stepArchetypes(data = hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']], k = 1:8, verbose = T, nrep = 4)
        screeplot(a)
        #looks like 5 groups are the best
      a_f = bestModel(a[[5]])
      parameters(a_f)
      barplot(a_f,hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']], percentiles = T)
      pcplot(a_f,hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']])
      xyplot(a_f,hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']], chulll=chull(hcap_r[, names.hcap_r[vars.hcap_r[,VarCol] == 'i']]))
      archmap(a_f, col = hcap_r_bck$clusterGroup)
      
      #addding it to our data
        t = coef(a_f)
      hcap_r_bck$ArchCluster = max.col(t)
    }
  
  }
  
  
  
  if(FALSE) #change to true for a ~.
  {
    tree.formula = create.formula(names.hcap_r[vars.hcap_r[,VarCol] == 'd'], names.hcap_r[vars.hcap_r[,VarCol] != 'd'])
  } else {
    tree.formula <- create.formula(names.hcap_r[vars.hcap_r[,VarCol] == 'd'], names.hcap_r[vars.hcap_r[,VarCol] == 'i'] )
    
  }
vprint("generating a tree on the selected variables")
  if(FALSE)  #Change to TRUE if you want to run a join model
  {
    tree.formula <- create.formula("res", c(lm_importance_vars(rtree.hcap_r,30),lm_importance_vars(rtreeres.hcap_r,30)))
  }

    tree.hcap_r <- tree(tree.formula, data = hcap_r)
  
vprint("Running a Boruta Model")      
if(FALSE){ #Change to TRue if want to run a Boruta model
  
    boruta.hcap_r = Boruta(tree.formula, data = hcap_r, getImp = getImpFerns )
    boruta_b =  attStats(boruta.hcap_r)
    con_b = attStats(boruta.hcap_r)[attStats(boruta.hcap_r)$decision == 'Confirmed',]
    con_b[order(con_b$meanImp,decreasing = T ),]
    boruta_confirmed = row.names(con_b[order(con_b$meanImp,decreasing = T ),])
    
    
    #running a regression on the important variables from Boruta
    boruta_lm.hcap_r = glm(create.formula(names.hcap_r[vars.hcap_r[,VarCol] == 'd'], boruta_confirmed), data=hcap_r,family= "binomial" )
    summary(boruta_lm.hcap_r)
      }
 


  
vprint("plotting the tree")  
  plot(tree.hcap_r)
  text(tree.hcap_r, pretty= 0)
  
hcap_r = hcap_r_bck


hcap_r = hcap_r_bck %>% filter(clusterGroup == 2)
vprint("now creating a random forest")
    set.seed(1) 
    rtree.hcap_r = randomForest(tree.formula, data = hcap_r,  importance = TRUE,keep.inbag  = T)
    randomForest::importance(rtree.hcap_r)
    varImpPlot(rtree.hcap_r)
    summary(lm_importance(rtree.hcap_r, 30))
  #  summary(lm_importance(rtree.hcap_r, 30, regression_type = 'binomial'))



if(FALSE){#change to true to use forest floor)
 ff = forestFloor(rtree.hcap_r, X = hcap_r[,c("NSLEPerSqrFoot", names.hcap_r[vars.hcap_r[,VarCol] == 'i'])])
 Col = fcol(ff, orderByImportance = T)
 plot(ff, 1:15, col = Col )

show3d (ff, c(1,2), col = Col, orderByImportance = T)
 
}

if(FALSE){#Change to True if you want to pull out the Importantce from Baruta and RF}
   imp = compare_vars(rtree.hcap_r, boruta.hcap_r)
} 


if(FALSE) {#Change to true for GLMNET 
  if(FALSE) {#Turn to True to run GLMNET}
    vprint("Now working glmnet")
      glmnet.hcap_r <-glmnet(as.matrix(hcap_r[,vars.hcap_r[VarCol]=='i' & !ls_factor(hcap_r)]), as.matrix(hcap_r[,vars.hcap_r[VarCol]=='d']))
    vprint("plotting glmnet")
      plot(glmnet.hcap_r)
    vprint("looking at the coeff")
      coef(glmnet.hcap_r, s = min(glmnet.hcap_r$lambda))
  }
  
  if(FALSE) {#Turn to true to turn on Cross Validate GLMNet
    vprint("CV glmnet")
      cvglmnet.hcap_r = cv.glmnet(as.matrix(hcap_r[,vars.hcap_r[VarCol]=='i' & !ls_factor(hcap_r)]), as.matrix(hcap_r[,vars.hcap_r[VarCol]=='d']))
      plot(cvglmnet.hcap_r)
      coef(cvglmnet.hcap_r, s="lambda.min")
    vprint("Now selecting the variables that were optimized")
      whichVariable <- (as.numeric(coef(cvglmnet.hcap_r$glmnet.fit, s = cvglmnet.hcap_r$lambda.min))[-1] !=  0)
      
      sum(whichVariable)
      colnames(as.matrix(hcap_r[,vars.hcap_r[VarCol]=='i' & !ls_factor(hcap_r)]))[whichVariable]
      selectedVars <- data.frame(whichVariable)
      selectedVars$glmpicked <- colnames(as.matrix(hcap_r[,vars.hcap_r[VarCol]=='i' & !ls_factor(hcap_r)]))
  }
}
if(FALSE){#Chnage to true if you would like to save the results
      save.the.trees(savedata,30)
}

#######
#
# Switching to Internal Model Below
#
#######


vprint("Generating the Residuals")
    if(TRUE) #Change to TRUE to run on the residuals from the previous Model
      {
        if(TRUE) #Change to TRUE to use the Random Forset
        {hcap_r$res <- lm_importance(rtree.hcap_r, 30)$residuals}
        if(FALSE) #Change to False to use the Boruta GLM Model
        {hcap_r$res = boruta_lm$residuals}
    } else {hcap_r$res <- lm(create.formula(names.hcap_r[vars.hcap_r[,VarCol] == 'd'], t1 ), hcap_r)$residuals}
    
  if(TRUE) {#change to TRUE if you want to change the res into a binomial split
    quant_l  = as.numeric(quantile(lm_importance(rtree.hcap_r, 30)$residuals, probs = seq(0,1,.2))[5])
     hcap_r$res = ifelse(hcap_r$res >= quant_l, 1, 0)
    #  hcap_r$res = factor(hcap_r$res)
    }
  
  vprint("creatinga new formula")
    VarCol2 = 3
    names.hcap_r <- names(hcap_r)
    if(!("res" %in% vars.hcap_r[,1]))
      {vars.hcap_r[nrow(vars.hcap_r)+ 1,] <- c("res",rep("",ncol(vars.hcap_r)-1))}
    tree.formulares <- create.formula("res", names.hcap_r[vars.hcap_r[,VarCol2] == 'i'] )
  
  vprint("creating residual tree")
    treeres.hcap_r <- tree(tree.formulares, data = hcap_r)
    plot(treeres.hcap_r)
    text(treeres.hcap_r, pretty= 0)
  
  vprint("creating residual Forest")
    if(FALSE){ 
      hcap_r$ASPPPerSqrFoot = ifelse(hcap_r$ASPPPerSqrFoot > 0.08, 1, 0)
      hcap_r$DiscountPerSqrFoot = ifelse(hcap_r$DiscountPerSqrFoot  > 1, 1, 0)
      hcap_r$FTChurnRatio = ifelse(hcap_r$FTChurnRatio > 1.3, 1, 0)
      hcap_r$ASPPDISCCHURN = ifelse(hcap_r$FTChurnRatio==hcap_r$DiscountPerSqrFoot  & hcap_r$DiscountPerSqrFoot == hcap_r$ASPPPerSqrFoot & hcap_r$ASPPPerSqrFoot == 1, 1,0  )
      tree.formulares = create.formula("res", c("ASPPPerSqrFoot","DiscountPerSqrFoot","FTChurnRatio", "PTChurnRatio"))
      
      hcap_r$res = factor(hcap_r$res)
      
      
      hcap_r$ran = runif(nrow(hcap_r))
      hcap_r$pred = NA
      rtreeres.hcap_r = randomForest(tree.formulares, data = hcap_r[hcap_r$ran <= 0.5,],  importance = TRUE,keep.inbag  = T)
      hcap_r[hcap_r$ran > 0.5,"pred"] = predict(rtreeres.hcap_r, newdata = hcap_r[hcap_r$ran > 0.5,] )
      
      table(hcap_r[hcap_r$ran > 0.5, c("res","pred")])
    }
    rtreeres.hcap_r = randomForest(tree.formulares, data = hcap_r,  importance = TRUE,keep.inbag  = T)
    importance(rtreeres.hcap_r)
    varImpPlot(rtreeres.hcap_r, main = "Random Forest Importance on Residuals")
    summary(lm_importance(rtreeres.hcap_r, 30))
    summary(lm_importance(rtreeres.hcap_r, 30, regression_type = 'binomial'))
   
    hcap_r_bck[row.names(hcap_r),]$res = hcap_r$res
    save.the.resin(savedata,10)
    
    if(FALSE){#change to true to use forest floor)
      ff = forestFloor(rtreeres.hcap_r, X = hcap_r[,names.hcap_r[vars.hcap_r[,VarCol2] == 'i']])
      Col = fcol(ff, orderByImportance = T)
      plot(ff, 1:12, col = Col )
      
      show3d (ff, c(1,9), col = Col, orderByImportance = T)
      
    }  
    if(FALSE)
    {
      save.forest(data_cov(hcap_r_bck, TRUE), savedata)
      
    }
  
  proc.time() - starttime
  
 # summary(lm_importance(rtreeres.hcap_r, 30))
  


  



  
