# R code file for agentmarkov package
# version 0.0.1
# Licensed under GPLv2 or later
# Author: Gary Weissman
# Contact: gary@babelgraph.org

# create markov model object
setClass(
	Class="markov", 
	representation=representation(
		states='numeric'
		m='matrix', 
		names='character'))

# functions to interact with markov model
newMM <- function(d,...) {
  new(Class='markov',
	states=d,
	m=matrix(1/d,ncol=d,nrow=d),
	names=paste('state.',1:d,sep='')) 
}
  
# create cohort model object  
  
# functions to interact with the cohort

# create graph object model

# functions to interact with graph model

# run and display model

# run and save model to animation
