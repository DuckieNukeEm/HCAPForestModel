#######################################################################
#This file contains the functions that will be needed for tree's
#######################################################################

#This is the print function for the verbose
vprint <- function(...){
  if(verbose) {print(...)}
}

create.formula <- function(Dep , indep, sep.value = "+", dep.sep.value = sep.value ){
      dep.side <- paste(Dep,collapse = dep.sep.value)
      indep <- indep[!is.na(indep)]
     
      indep.side <- paste(indep, collapse = sep.value)
      return (as.formula(paste(dep.side, indep.side, sep = "~")))
}

level_length = function(x){
    if (is.factor(x))
      {return(length(levels(x)))
    } else {return(0)}
}

###Function to tream leading and trailing whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

cleanup = function(List.Not.to.delete = Starting.R.Var){
  ending.vars <<- ls()
  ending.vars <<- ending.vars[!(ending.vars %in% List.Not.to.delete)]
  ending.vars <<- c(ending.vars, "ending.vars")
  #rm(list = ending.vars, envir = .GlobalEnv)
  rm(list = ending.vars, inherits = T)
  
  
}   
#a function to remove all factors from the matrix before a correlation is computer
data_cov <- function(df, conditions){
    return(cor(df[, !ls_factor(df) & conditions]))
}

#a function that returns which elements are factors
ls_factor <- function(df) {
  df.factor <- sapply(df, is.factor)
  return(df.factor)
}

####Bunch of Save Functions
save.forest <- function(rf, save.dir, file.name = "")
{
  if(class(rf)[1]=="tree")
  { if(file.name == "") {file.name ="Tree"}
    write.csv(rf$frame, file = file.path(save.dir, paste(c(file.name,".csv"), collapse = "")))
  } else if (class(rf)[1]=="randomForest.formula")
    {if(file.name == "") {file.name ="Importance Plot"}
    write.csv(rf$importance, file = file.path(save.dir, paste(c(file.name,".csv"), collapse = "")))
  } else if (class(rf)[1]=="matrix") {
    if(file.name == "") {file.name ="Matrix"}
    write.csv(rf, file = file.path(save.dir, paste(c(file.name,".csv"), collapse = "")))
  }else
  {stop("Can only accept class of 'tree','randomForest', or 'matrix'")}
}

save.forest_image <- function(rt, save.dir, save.type = "jpeg", Name = ""){
  if(Name==""){Name=class(rt)[1]}
   if (save.type == "jpeg") {
    jpeg(file.path(save.dir, paste(c(Name,"jpg"), collapse = "." )), width = 1000, height = 1000, quality = 100  )
  } else if (save.type == "png") {
    png(file.path(save.dir, paste(c(Name,"png"), collapse = "." )), width = 1000, height = 1000)
  } else if (save.type == "wmf") {
    win.metafile(file.path(save.dir, paste(c(Name,"wmf"), collapse = "." )))
  } else if (save.type == "pdf") {
    pdf(file.path(save.dir, paste(c(Name,"pdf"), collapse = "." )))
  } else {print("error, please select one (jpeg, png, wmf,pdf)")
    return(-1)}


  
  if(class(rt)[1]== "tree")
    { plot(rt, main = "Tree")
      text(rt, pretty = 0 , cex = 1)
  } else if (class(rt)[1]=="randomForest.formula") {varImpPlot(rt, main = "Random Forest")
  }  else {dev.off()
    stop("This function only works for objects of class `randomForest' or 'tree")}
  
  dev.off()
  
}


save.image <- function(rt, save.dir, save.type = "jpeg", Name = "", forest_floor = FALSE){
  if(Name==""){Name=class(rt)[1]}
  if(forest_floor & class(rt)[1]=="randomForest.formula"){
    t = as.vector(unlist(strsplit(as.character(formula(rt$terms))[3]," + ", fixed = T)))
    ff = forestFloor(rt, X = hcap_r[,trim(t)])
    Col = fcol(ff, orderByImportance = T)
    if(Name==class(rt)[1]) {Name = "Forest Floor Plot"}
  }
  if (save.type == "jpeg") {
    jpeg(file.path(save.dir, paste(c(Name,"jpg"), collapse = "." )), width = 1000, height = 1000, quality = 100  )
  } else if (save.type == "png") {
    png(file.path(save.dir, paste(c(Name,"png"), collapse = "." )), width = 1000, height = 1000)
  } else if (save.type == "wmf") {
    win.metafile(file.path(save.dir, paste(c(Name,"wmf"), collapse = "." )))
  } else if (save.type == "pdf") {
    pdf(file.path(save.dir, paste(c(Name,"pdf"), collapse = "." )))
  } else {stop("error, please select one (jpeg, png, wmf,pdf)")}
  
  
  if(class(rt)[1]== "tree")
  { plot(rt, main = "Tree")
    text(rt, pretty = 0 , cex = 1)
  } else if (forest_floor) { plot(ff, 1:15, col = Col )
  
  } else if (class(rt)[1]=="randomForest.formula") {varImpPlot(rt, main = "Random Forest")
  }  else {rt}
  
  dev.off()
  
} 

save.forest_regression<-function(rt, save.dir, file.name = ""){
  if(file.name =="") {file.name = class(rt)[1]}
  #redorects tje output
  sink(file = file.path(save.dir, paste(c(file.name,".txt"), collapse = "")))
  
  if(class(rt)[1] %in% c('lm','glm')){
    print("Regression Ran:")
    print(formula(rt$terms))
    print(summary(rt))
  }else if (class(rt)[1]=="forest"){
    print(rt)
    print(summary(rt))
  }else if (class(rt)[1]=="randomForest.formula"){
    print(rt)
    print(rt$importance)
} else {print(rt)}
  sink()
  
}
 

#this function runs a linear (or binomaila, or gaussian) regression on the importance function
lm_importance <- function(rt, var.number = 0, sort.by = "X.IncMSE", sort.decrease = TRUE, regression_type = 'lm'){

       lm.formula <- lm_formula(rt, var.number, sort.by, sort.decrease)
       
       
       
       df.names = names(data.frame(randomForest::importance(rt)))
       if(df.names[2] == "X1") {regression_type = 'binom'}
       
       
       df = get(as.character(rt$call[3]))
       if(regression_type == 'lm')
       {
        rt.lm <- lm(lm.formula, data=df)
       } else if (regression_type == 'binom') {
         # clearing out factors if I need too
         if(is.factor(df[,unlist(strsplit(as.character(lm.formula),  '~')[2])]))
         {
           df[,unlist(strsplit(as.character(lm.formula),  '~')[2])] = as.integer(df[,unlist(strsplit(as.character(lm.formula),  '~')[2])]) - 1 
         }
        rt.lm =  glm(lm.formula, data=df, family = "binomial")
         
       } else { 
         # clearing out factors if I need too
         if(is.factor(df[,unlist(strsplit(as.character(lm.formula),  '~')[2])]))
         {
           df[,unlist(strsplit(as.character(lm.formula),  '~')[2])] = as.integer(df[,unlist(strsplit(as.character(lm.formula),  '~')[2])]) 
         }
         
         rt.lm =  glm(lm.formula, data=df, family =  "binomial")
       
       }
       #hcap_r[,unlist(strsplit(as.character(tree.formula),  '~')[2])]
        return(rt.lm)
        
}


lm_formula <- function(rt, var.number = 0, sort.by = "X.IncMSE", sort.decrease = TRUE){
      if (!inherits(rt, "randomForest")) 
      {stop("This function only works for objects of class `randomForest'")}
      if(!(sort.by %in% c("X.IncMSE","IncNodePurity")))
      {stop("Please select 'X.IncMSE', 'IncNodePurty' to sort by")}
        
      
  
      df.importance <- data.frame(randomForest::importance(rt))
      if(names(df.importance)[1]=='No' & sort.by == "X.IncMSE") {sort.by = "MeanDecreaseAccuracy"
      } else if(names(df.importance)[2] == "X1"& sort.by == "X.IncMSE") {sort.by = "MeanDecreaseAccuracy"}
      if(var.number == 0 | var.number > nrow(df.importance)){var.number = nrow(df.importance)}
      ord <-  order(df.importance[, sort.by], decreasing = sort.decrease)[1:var.number]
      
      return (create.formula(all.vars(rt$terms)[1],lm_importance_vars(rt, var.number, sort.by, sort.decrease) ))
} 

compare_vars = function( for1, for2, var.number = 0, sort.by = "X.IncMSE", sort.decrease = TRUE, compare_type = 'intersection'){
  if (!inherits(for1, "randomForest") & !inherits(for1, "Boruta") ) 
  {stop("the first function must be of class `randomForest' or 'Boruta")}
  if (!inherits(for2, "randomForest") & !inherits(for2, "Boruta") ) 
  {stop("the second function must be of class `randomForest' or 'Boruta")}
  
  if( inherits(for1, "randomForest") & inherits(for2, "randomForest"))
  {
    vars1 = lm_importance_vars(for1,var.number, sort.by, sort.decrease)
    vars2 = lm_importance_vars(for2,var.number, sort.by, sort.decrease)
    return(intersection(vars1, vars2))
  } else if( inherits(for1, "Boruta") & inherits(for2, "Boruta"))
  {
    vars1 = attStats(for1)[attStats(for1)$decision == 'Confirmed',]
    vars1 = row.names(vars1[order(vars1$meanImp,decreasing = T ),])
    
    vars2 = attStats(for2)[attStats(for2)$decision == 'Confirmed',]
    vars2 = row.names(vars2[order(vars2$meanImp,decreasing = T ),])
    
  } else if(inherits(for1, "Boruta")) {
    
    vars1 = attStats(for1)[attStats(for1)$decision == 'Confirmed',]
    vars1 = row.names(vars1[order(vars1$meanImp,decreasing = T ),])
    
    vars2 = lm_importance_vars(for2,var.number, sort.by, sort.decrease)
    
  } else if(inherits(for1, "randomForest") ){
    
    vars1 = lm_importance_vars(for1,var.number, sort.by, sort.decrease)
    
    vars2 = attStats(for2)[attStats(for2)$decision == 'Confirmed',]
    vars2 = row.names(vars2[order(vars2$meanImp,decreasing = T ),])
    
  } else {stop("Sowwy, no can do bro, no can do (And I'm not sure why...")}
    
  
  if (compare_type == 'diff')
  {return(setdiff(vars1,vars2))
  } else if (compare_type == 'union') {
    return(union(vars1,vars2))
  } else if (compare_type == 'equal')
    return(setequal(vars1, vars2))
  else {return(intersect(vars1, vars2))}
  
  
  
}

lm_importance_vars<- function(rt, var.number = 0, sort.by = "X.IncMSE", sort.decrease = TRUE){
  if (!inherits(rt, "randomForest")) 
  {stop("This function only works for objects of class `randomForest'")}
  if(!(sort.by %in% c("X.IncMSE","IncNodePurity","MeanDecreaseAccuracy")))
  {stop("Please select 'X.IncMSE' or 'IncNOdePurty' or 'MeanDecreaseAccuracy' to sort by")}
  
  df.importance <- data.frame(randomForest::importance(rt))
  
  if(names(df.importance)[1]=='No' & sort.by == "X.IncMSE") {sort.by = "MeanDecreaseAccuracy"}
  if(var.number == 0 | var.number > nrow(df.importance)){var.number = nrow(df.importance)}
  ord <-  order(df.importance[, sort.by], decreasing = sort.decrease)[1:var.number]
  return(row.names(df.importance[ord,]))
}
#This functions calculates a vif on all variables (Well most) of a df
vif <- function(data.in) {
        data.names <- names(data.in)
        #cleaning up anything with only one factor
          bad <- NULL
          for( val in data.names) {
              if (nlevels(hcap_r[,val]) == 1L) 
                  {bad <- c(bad, val)}
            }
        
          if(length(bad))
          { 
              cat("removing the following from VIF: " , bad)
              data.names <- data.names[-which(data.names %in% bad)]
          }
          rm(bad)
          
        #creating the out vif grid
        vif_init<-expand.grid(var = data.names, vif.value = 0)
        
        
        #calculating the grid
        for (val in data.names){
          #if (nlevels(data.in[,val]) == 1L) {next}
          print(paste(c(val,which(data.names == val),"of", length(data.names)), collapse = " "))
          form <- paste(data.names[-which(val %in% data.names)] , collapse = '+')
          form <- formula(paste(val, '~', form))
          vif_init[vif_init$var == val, 2] = VIF(lm(form, data = data.in))  
        }
        return(vif_init)
          
      }
      
      
      



save.the.trees<-function(save.dir, reg.lengh = 0 ){
  
  if(("hcap_r" %in% ls(envir = .GlobalEnv))) {save.forest(data_cov(hcap_r, TRUE), save.dir)
    print("Saved Cor Matrix")}
  if(('vars.hcap_r' %in% ls(envir=.GlobalEnv))) {
    if(('VarCol' %in% ls(envir=.GlobalEnv)))  
    {write.csv(vars.hcap_r[,c(1,VarCol)], file.path(save.dir,"Variables Selection.csv"))
    }else{write.csv(vars.hcap_r[,], file.path(save.dir,"Variables Selection.csv"))}
    print("variables list saved")
  }
  
  if(("tree.hcap_r" %in% ls(envir = .GlobalEnv))) {save.forest_regression(tree.hcap_r, save.dir)
    print("Saved tree")
    save.image(tree.hcap_r, save.dir )
    print("Saved Tree Image")
  }
  if(("rtree.hcap_r" %in% ls(envir = .GlobalEnv))){save.forest_regression(rtree.hcap_r, save.dir)
    print("Saved random Forest")
    save.image(rtree.hcap_r, save.dir )
    save.image(rtreeres.hcap_r, save.dir, save.type = 'png',Name = "Forest_Floor_RF_plot", forest_floor = T )
    print("Saved Random forest Image")
    save.forest_regression(lm_importance(rtree.hcap_r, var.number = reg.lengh),save.dir)
    save.forest_regression(lm_importance(rtree.hcap_r, var.number = reg.lengh, regression_type = 'binomial'),save.dir)
    print("Saved Regression")
    
  }
  
  if(("cvglmnet.hcap_r" %in% ls(envir = .GlobalEnv))) {
    save.forest_regression(coef(cvglmnet.hcap_r, s="lambda.min"), save.dir, file.name = "glmNet Output")
    print("Saved cvGlmNet coeff")
    save.forest_regression(colnames(as.matrix(hcap_r[,vars.hcap_r[VarCol]=='i' & !ls_factor(hcap_r)]))[whichVariable]
                           , save.dir, file.name = "glmNet Output List")
    print("Saved cvGlmNet List")
    
  }
}



save.the.resin<-function(save.dir, reg.lengh = 0, cor = FALSE ){
  
  if((cor == T & "hcap_r" %in% ls(envir = .GlobalEnv))) {save.forest(data_cov(hcap_r, TRUE), save.dir)
    print("Saved Cor Matrix")}
  
  if(('vars.hcap_r' %in% ls(envir=.GlobalEnv))) {
    if(('VarCol2' %in% ls(envir=.GlobalEnv)))  
    {write.csv(vars.hcap_r[,c(1,VarCol2)], file.path(save.dir,"Variables Selection for Residuals.csv"))
    }else{write.csv(vars.hcap_r[,], file.path(save.dir,"Variables Selection for Residuals.csv"))}
    print("variables list saved")
  }
  
  if(("treeres.hcap_r" %in% ls(envir = .GlobalEnv))) {save.forest_regression(treeres.hcap_r, save.dir, "tree resin")
    print("Saved the tree resin")
    save.image(treeres.hcap_r, save.dir, Name = "tree resin" )
    print("Saved Tree resin Image")
  }
  if(("rtreeres.hcap_r" %in% ls(envir = .GlobalEnv))){save.forest_regression(rtreeres.hcap_r, save.dir, "randomForest.formula.resin")
    print("Saved random Forest resin")
    save.image(rtreeres.hcap_r, save.dir, Name = "randomforest.formula.resin" )
    save.image(rtreeres.hcap_r, save.dir, save.type = 'png', Name = "Forest_Floor_RF_resin_plot", forest_floor = T )
    print("Saved Random forest resin Image")
    save.forest_regression(lm_importance(rtreeres.hcap_r, var.number = reg.lengh),save.dir, file.name = "lm resin")
    save.forest_regression(lm_importance(rtreeres.hcap_r, var.number = reg.lengh, regression_type = 'binomial'),save.dir, file.name = "Binomial lm resin")
    
    sink()
    print("Saved resin Regression")
  }
  
  if(("cvglmnetres.hcap_r" %in% ls(envir = .GlobalEnv))) {
    save.forest_regression(coef(cvglmnetres.hcap_r, s="lambda.min"), save.dir, file.name = "glmNet resin Output")
    print("Saved cvGlmNet resin coeff")
    save.forest_regression(colnames(as.matrix(hcap_r[,vars.hcap_r[VarCol2]=='i' & !ls_factor(hcap_r)]))[whichVariableres]
                           , save.dir, file.name = "glmNet Output resin List")
    print("Saved cvGlmNet resin List")
    
  }
}

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
