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

data_cov <- function(df, conditions){
    df.factor <- sapply(df, is.factor)
    return(cor(df[, !df.factor & conditions]))
}

rlm <- function()
