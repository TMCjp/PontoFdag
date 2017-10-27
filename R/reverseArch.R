##' reverse Arch the DAG graphNEL
##'
##' reverse Arch the DAG graphNEL
##' @title reverseArch
##' @param dirGraph graphNEL, a graphnel object of directed network
##' @param useAlgo character, which method to be used
##' @param useWeights logical, Whether to use weight
##' @return graphNEL, a DAG network graphNEL that includes matrix
##' @author tmcjp

reverseArch <- function(dirGraph,
                        useAlgo = 'sparse',
                        useWeights = TRUE) {


  if(edgemode(dirGraph) != 'directed')
    stop('The graph is not directed. Nothing to do')


  if(numEdges(dirGraph) == 0) {
    message('Nothing to do:')
    return(dirGraph)
  }



  if(useAlgo == "sparse") {
    nodNames <- nodes(dirGraph)
    return(sparseM2Graph(t(graph2SparseM(dirGraph, useWeights)),
                         nodNames, edgemode = "directed"))
  }

  return(as(t(as(dirGraph, 'matrix')), 'graphNEL'))
}
