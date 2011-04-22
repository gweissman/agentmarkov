# R code file for agentmarkov package
# version 0.0.1
# Licensed under GPLv2 or later
# Author: Gary Weissman
# Contact: gary@babelgraph.org

# create markov model object
setClass(
	Class='markov', 
	representation=representation(
    model.name='character',
		num.states='numeric',
		m='matrix', 
		state.names='character'))

setGeneric('getMatrix',function(object,...)
	standardGeneric('getMatrix'))
    
setMethod('getMatrix','markov',
          function(object,...) {
            print(object@m) } )
            
setGeneric('setMatrix',function(object,...)
  standardGeneric('setMatrix'))  
            
setMethod('setMatrix',
          signature='markov',
          definition=function(object,newmat,...) {
            rsame <- nrow(object@m) == nrow(newmat)
            csame <- ncol(object@m) == ncol(newmat)
            if (rsame && csame) {
              object@m <- newmat; return(object) }
            else { print('Error: Cannot replace new matrix. Wrong dimensions.') }
          } )
          
setMethod("show","markov",
          function(object) {
            cat('Markov Model:',object@model.name,'\n') 
            cat('States:',object@num.states,'\n')
            tmp <- as.data.frame(object@m)
            rownames(tmp)<-object@state.names
            colnames(tmp)<-object@state.names
            cat('Transition Matrix:\n')
            print(tmp)
} )
            
# functions to interact with markov model
newMM <- function(d=2,
                  name='Sample Model',
                  mm=matrix(1/d,ncol=d,nrow=d),
                  states=paste('state.',1:d,sep=''))
          {
            new(Class='markov',
            model.name=name,
          	num.states=d,
          	m=mm,
          	state.names=states) 
          }

  
# create cohort model object  
setClass(
  Class='cohort', 
	representation=representation(
    cohort.name='character',
		size='numeric',
		data='list'))
    
# functions to interact with the cohort
newCohort <- function(size=10,
                name='Sample Cohort',
                info=list(),...) {
                  
        new(Class='cohort',
        cohort.name=name,
        size=size,
        data=info)
}

# create graph object model
setGeneric('graphMM',function(object,...)
  standardGeneric('graphMM'))
  
setMethod('graphMM','markov',
  function(object,style=1,...) {
    if (!require(igraph)) { print('ERROR: Needs igraph library') }
    else {
      rownames(object@m) <- object@state.names
      colnames(object@m) <- object@state.names
      gmm <- graph.adjacency(object@m,weighted=TRUE)
      gmm$layout <- switch(style,layout.reingold.tilford(gmm),
                layout.fruchterman.reingold(gmm))
      plot(gmm,vertex.shape='square',
        main=paste('Markov transition diagram for',object@model.name),
        edge.curved=TRUE,edge.label=E(gmm)$weight,vertex.color='goldenrod',
        vertex.label=V(gmm)$name)
    }
})

# functions to interact with graph model

# run and display model
runModel <- function(cc,mm,time.steps=100) {

}

# run and save model to animation
