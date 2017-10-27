##' return the nodes in the Induced Graph
##'
##' return the nodes in the Induced Graph
##' @title nodesInInducedGraph
##' @param dag graphNEL, a graphnel object of DAG network
##' @param startNodes character vector, nodes are used for backtracking
##' @return list, all nodes in the Induced Graph
##' @author tmcjp

nodesInInducedGraph <- function(dag,
                                startNodes) {

  nodeLookUp <- new.env(hash = T,
                        parent = emptyenv()
  )



  nodesDAG <- dag@nodes



  buildInducedGraph <- function(node) {

    if(exists(node, envir = nodeLookUp, mode = 'logical', inherits = FALSE))
      return(1)


    assign(node, TRUE, envir = nodeLookUp)

    adjNodes <- nodesDAG[dag@edgeL[[node]]$edges]


    if(length(adjNodes) == 0)
      return(2)

    for(i in 1:length(adjNodes))
      buildInducedGraph(adjNodes[i])

    return(0)
  }


  lapply(startNodes, buildInducedGraph)


  return(ls(nodeLookUp))
}







##' return subGraph form DAG network by startNodes
##'
##' return subGraph form DAG network by startNodes
##' @title inducedGraph
##' @param dag graphNEL, a graphnel object of DAG network
##' @param startNodes character vector, nodes are used for backtracking
##' @return graphNEL list, DAG network as graphnel class
##' @author tmcjp

inducedGraph <- function(dag,
                         startNodes) {
  return(
    subGraph(
      nodesInInducedGraph(dag, startNodes),
      dag)
  )
}
