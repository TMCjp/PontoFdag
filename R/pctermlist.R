##' creat term list that Including parent and child node correspondence
##'
##' creat term list that Including parent and child node correspondence
##' @title pctermlist
##' @param ontology ontology object
##' @param ontoterm ontology all term ID
##' @param symbol_2_ID character, term symbol character that name attr is term ID
##' @return list, Including parent and child node correspondence
##' @author tmcjp

pctermlist <- function(ontology, ontoterm, symbol_2_ID){

  c_To_p <- alist()
  TermParents <- alist()
  termID <- getAllTermIds(ontology)

  for(i in 1:length(ontoterm)){
    TermParents <- getTermParents(ontology, ontoterm[[i]])

    if(length(TermParents) != 0){
      Parents <- as.character()
      for(n in 1:length(TermParents)){
        Parents[n] <- getLabel(TermParents[[n]])

        suppressWarnings(m <- which(symbol_2_ID == Parents[n]))
        if(length(m) == 0){
          suppressWarnings(Parents[n] <- NA)
        } else {
          suppressWarnings(Parents[n] <- termID[m])
        }
      }

      attr(Parents, "names") <- rep('is_a', n)

      c_To_p[[i]] <- Parents

    } else {
      c_To_p[[i]] <- NA
    }

    # <- termID[i]
  }

  attr(c_To_p, 'names') <- termID


  for(i in 1:length(c_To_p)){

    n <- length(c_To_p[[i]])

    if(n != 1){

      location <- as.character()
      for(m in 1:n){
        location[m] <- as.logical( is.na(c_To_p[[i]][m]) )
      }
      math <- which(location == "TRUE")
      if(length(math) != 0){
        c_To_p[[i]] <- c_To_p[[i]][-math]
      }

    } else {

      if( as.logical( is.na(c_To_p[[i]]) ) ){
        c_To_p[[i]] <- NA
      }

    }

  }


  if(length(which(c_To_p == 'NA')) > 0){
    c_To_p <- c_To_p[-which(c_To_p == 'NA')]
  } else {
    c_To_p <- c_To_p
  }

  return(c_To_p)
}
