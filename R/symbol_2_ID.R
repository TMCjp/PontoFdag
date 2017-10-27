##' creat term symbol character that name attr is term ID
##'
##' creat term symbol character that name attr is term ID
##' @title symbol_2_ID
##' @param ontology ontology object
##' @return character, term symbol character that name attr is term ID
##' @author tmcjp

symbol_2_ID <- function(ontology){
  termID <- getAllTermIds(ontology)
  labels <- alist()
  for(i in 1:length(termID)){
    labels[[i]] <- getTermNameById(ontology, termID[i])
  }

  labels <- unlist(labels)
  attr(labels, "names") <- termID

  return(labels)
}
