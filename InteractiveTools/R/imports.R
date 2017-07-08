# rm(list=ls())
require(Rcpp)
require(RcppEigen)
require(Matrix)
require(lda)
require(igraph)


sourceCpp("../src/gibbsCpp.cpp")
sourceCpp("../src/likelihood.cpp")
sourceCpp("../src/initializeCpp.cpp")

source("generator.synthetic.R")

source("add.constraints.R")
source("add.constraints.with.zs.R")
source("initialize.interactive.maps.R")

source("document.assignments.R")
source("corpus.assignments.R")


source("interactive.lda.collapsed.gibbs.sampler.hard.R")
source("add.constraints.without.zs.edge.control.R")

source("uniform.sample.constraints.R")
source("sample.doc.buckets.constraints.R")

source("precision.R")
source("recall.R")
source("accuracy.R")
source("error.R")
