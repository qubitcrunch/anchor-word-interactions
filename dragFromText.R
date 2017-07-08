library(shiny)
library(shinyDND)
require(tm)
require(stringr)
require(foreach)
require(RJSONIO)

source("interface_code/Interface_Utilities.R")
setwd("InteractiveTools/R")
source("importsInterface.R")
setwd("../..")

steps_btw_training	<-	10000
gibbs_iterations	<-	10
runApp("interface_code/interface_anchor_words")

#runApp("interface_fineGrainedV4", host="0.0.0.0",launch.browser=FALSE,port=8095)
