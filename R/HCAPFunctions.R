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

cleanup = function(List.Not.to.delete = Starting.R.Var){
  ending.vars <<- ls()
  ending.vars <<- ending.vars[!(ending.vars %in% List.Not.to.delete)]
  ending.vars <<- c(ending.vars, "ending.vars")
  #rm(list = ending.vars, envir = .GlobalEnv)
  rm(list = ending.vars, inherits = T)
  
  
}   
#a function to remove all factors from the matrix before a correlation is computer
data_cov <- function(df, conditions){
    df.factor <- sapply(df, is.factor)
    return(cor(df[, !df.factor & conditions]))
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


save.image <- function(rt, save.dir, save.type = "jpeg", Name = ""){
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
  
  rt
  
  dev.off()
  
} 

save.forest_regression<-function(rt, save.dir, file.name = ""){
  if(file.name =="") {file.name = class(rt)[1]}
  #redorects tje output
  sink(file = file.path(save.dir, paste(c(file.name,".txt"), collapse = "")))
  
  if(class(rt)[1]=="lm"){
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
    save.forest_image(tree.hcap_r, save.dir )
    print("Saved Tree Image")
    }
  if(("rtree.hcap_r" %in% ls(envir = .GlobalEnv))){save.forest_regression(rtree.hcap_r, save.dir)
    print("Saved random Forest")
    save.forest_image(rtree.hcap_r, save.dir )
    print("Saved Random forest Image")
    save.forest_regression(lm_importance(rtree.hcap_r, var.number = reg.lengh),save.dir)
    print("Saved Regression")
  }
}

#this function runs a linear regression on the importance function
lm_importance <- function(rt, var.number = 0, sort.by = "X.IncMSE", sort.decrease = TRUE){
  if (!inherits(rt, "randomForest")) 
    {stop("This function only works for objects of class `randomForest'")}
  if(!(sort.by %in% c("X.IncMSE","IncNodePurity")))
     {stop("Please select 'X.IncMSE' or 'IncNOdePurty' to sort by")}
  
  df.importance <- data.frame(importance(rt))
  if(var.number == 0 ){var.number = nrow(df.importance)}
  ord <-  order(df.importance[, sort.by], decreasing = sort.decrease)[1:var.number]
  
 lm.formula <- create.formula(all.vars(rt$terms)[1], row.names(df.importance[ord,]))
  rt.lm <- lm(lm.formula, data=get(as.character(rt$call[3])))
  return(rt.lm)
  
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






