\name{buildDAG_graphnel}
\alias{buildDAG_graphnel}
\title{buildDAG_graphnel}
\usage{
  buildDAG_graphnel(knownNodes, adjLookUP, ONTO.ROOT)
}
\arguments{
  \item{knownNodes}{character vector, nodes are used to backtrack}
  \item{adjLookUP}{list, Including parent and child node correspondence}
  \item{ONTO.ROOT}{character, the root node of ontology}
}
\value{
  graphNEL list, DAG network as graphnel class
}
\description{
  build DAG as graphnel(class form package graph) object
}
\details{
  build DAG as graphnel(class form package graph) object,
  this function will building DAG network form the parser of ontology which you choosed.
}
\author{
  tmcjp
}
