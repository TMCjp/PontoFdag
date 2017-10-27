##' build DAG as graphnel(class form package graph) object
##'
##' build DAG as graphnel(class form package graph) object,
##' this function will building DAG network form the parser of ontology which you choosed.
##' @title buildDAG_graphnel
##' @param knownNodes character vector, nodes are used for backtracking
##' @param adjLookUP list, Including parent and child node correspondence
##' @param ONTO.ROOT character, the root node of ontology
##' @return graphNEL list, DAG network as graphnel class
##' @importFrom graph
##' @export
##' @author tmcjp


buildDAG_graphnel <- function(knownNodes, adjLookUP, ONTO.ROOT) {

  nodeLookUp <- new.env(hash = T, parent = emptyenv())

  isNodeInDAG <- function(node) {
    return(exists(node, envir = nodeLookUp, mode = 'logical', inherits = FALSE))
  }
  setNodeInDAG <- function(node) {
    assign(node, TRUE, envir = nodeLookUp)
  }



  edgeEnv <- new.env(hash = T, parent = emptyenv())

  envAddEdge <- function(u, v, type) {
    assign(v, switch(type, isa = 0, partof = 1, -1), envir = get(u, envir = edgeEnv))
  }


  buildInducedGraph <- function(node) {

    if(isNodeInDAG(node))
      return(1)

    setNodeInDAG(node)
    assign(node, new.env(hash = T, parent = emptyenv()), envir = edgeEnv)


    if(node == ONTO.ROOT)
      return(2)


    adjNodes <- adjLookUP[[node]]


    if(length(adjNodes) == 0)
      message('\n There are no adj nodes for node: ', node)


    for(i in 1:length(adjNodes)) {
      x <- as.character(adjNodes[i])
      envAddEdge(node, x, names(adjNodes[i]))
      buildInducedGraph(x)
    }

    return(0)
  }


  lapply(knownNodes, buildInducedGraph)


  .graphNodes <- ls(edgeEnv)
  .edgeList <- eapply(edgeEnv,
                      function(adjEnv) {
                        aux <- as.list(adjEnv)
                        return(list(edges = match(names(aux), .graphNodes),
                                    weights = as.numeric(aux)))
                      })


  library(graph)
  graph.topo <- new('graphNEL',
                    nodes = .graphNodes,
                    edgeL = .edgeList,
                    edgemode = 'directed')

  return(graph.topo)

}
