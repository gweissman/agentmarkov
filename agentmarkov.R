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
		state.names='character'),
    prototype(model.name='joe',
    num.states=3,
    m=matrix(1/3,nrow=3,ncol=3),
    state.names=paste('state.',1:3)))

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
          
# setGeneric('print',function(object,...)
#   standardGeneric('print'))
#           
# setMethod(f='print',
#           signature='markov',
#           definition=function(object,...) {
#           cat('Markov Model:',object@model.name,'\n') 
#           cat('States:',object@num.states,'\n')
#           tmp <- object@m; names(tmp)<-object@state.names
#           cat('Transition Matrix:\n')
#           print(tmp)
#           } )
            
# functions to interact with markov model
newMM <- function(d=5,
                  name='NewModel',
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
  
# functions to interact with the cohort

# create graph object model
setGeneric('graphMM',function(object,...)
  standardGeneric('graphMM'))
  
setMethod('graphMM','markov',
  function(object,...) {
    if (!require(igraph)) { print('ERROR: Needs igraph library') }
    else {
      
      rownames(object@m) <- object@state.names
      colnames(object@m) <- object@state.names
      gmm <- graph.adjacency(object@m,weighted=TRUE)
      plot(gmm,layout=layout.reingold.tilford,vertex.shape='square',
        main=paste('Markov transition diagram for',object@model.name),
        edge.curved=TRUE,edge.label=E(gmm)$weight,vertex.color='goldenrod')
      
    }
})


# functions to interact with graph model

# run and display model

# run and save model to animation
