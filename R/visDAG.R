##' creat html of DAG graph by visNetwork
##'
##' creat html of DAG graph by visNetwork
##' @title visDAG
##' @param DAGmatrix graphNEL, a DAG network graphNEL that includes matrix
##' @param symbol_2_ID character, term symbol to term ID
##' @param knownNodes character vector, nodes are used for backtracking
##' @param shapes character, DAG network shape
##' @return html, the html of DAG network graph
##' @author tmcjp

visDAG <- function(DAGmatrix, symbol_2_ID, knownNodes, shapes = 'dot'){

  # ###conbind reverseArch DAG function to visDAG function
  # DAGmatrix <- reverseArch(DAGmatrix, useAlgo = 'sparse', useWeights = TRUE)

  nodeinfo <- cbind(name=DAGmatrix@nodes, num=c(0:(length(DAGmatrix@nodes)-1)))
  nodeinfo <- as.data.frame(nodeinfo)
  nodeinfo$name <- as.character(nodeinfo$name)
  nodeinfo$num <- as.numeric(as.character(nodeinfo$num))


  tat <- igraph::igraph.from.graphNEL(DAGmatrix)
  edgeinfo <- igraph::as_edgelist(tat)
  colnames(edgeinfo) <- c('From', 'To')
  edgeinfo <- as.data.frame(edgeinfo)
  library(plyr)
  edgeinfo$From <- mapvalues(edgeinfo$From, nodeinfo$name, nodeinfo$num, warn_missing = F)
  edgeinfo$To <- mapvalues(edgeinfo$To, nodeinfo$name, nodeinfo$num, warn_missing = F)


  shape <- rep(shapes, length(DAGmatrix@nodes))
  math <- match(nodeinfo$name, attr(symbol_2_ID, 'names'))


  color <- rep('#87CEFA', length(DAGmatrix@nodes))
  math_2 <- match(knownNodes, nodeinfo$name)
  color[math_2] <- '#FF6347'


  des <- symbol_2_ID[math]
  nodeinfo$name <- des
  nodeinfo <- cbind(nodeinfo, shape=shape, color=color)



  library(visNetwork)
  library(dplyr)

  network <- visNetwork(

    nodes = nodeinfo %>% dplyr::rename("label" = name) %>% dplyr::rename('id' = num),
    edges = edgeinfo %>% dplyr::rename("from" = From, "to" = To)
    # width = 650, height = 550
  ) %>%
    visNodes(
      font = list(color = '#343434', size = 18)
      # shape = "image",
      # image = "http://cdn0.iconfinder.com/data/icons/octicons/1024/mark-github-128.png"
    ) %>%
    visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
    # visGroups(groupname = "No enrichment", shape = "dot") %>%
    # visGroups(groupname = "enrichment", shape = "triangle") %>%
    # visGroups(groupname = "Significant enrichment", shape = "star") %>%
    # visLegend(width = 0.1, position = "right", main = NULL, ncol = 1,
    #           stepX = 100, stepY = 100
    # ) %>%
    # visOptions(selectedBy = "group", manipulation = TRUE
    # ) %>%
    visInteraction(dragNodes = T, dragView = T, zoomView = T) %>%
    visIgraphLayout(layout = "layout_as_tree")


  network$x$nodes$y <- -network$x$nodes$y

  return(network)

}
