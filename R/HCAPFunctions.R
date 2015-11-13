#######################################################################
#This file contains the functions that will be needed for tree's
#######################################################################

#This is the print function for the verbose
vprint <- function(...){
  if(verbose) {print(...)}
}

create.formula <- function(Dep , indep, sep.value = "+", dep.sep.value = sep.value ){
      dep.side <- paste(Dep,collapse = dep.sep.value)
      indep.side <- paste(indep, collapse = sep.value)
      return (as.formula(paste(dep.side, indep.side, sep = "~")))
}
