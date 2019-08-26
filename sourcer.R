###sourcer###

#source all the necessary functions for the entire script to have a coherent structure in the project
#Install necessary packages
pkgs <-c('plot3D','ecr','ggplot2','reticulate','plot3Drgl',"roxygen2", "httr", "jsonlite", "htmltools", "kknn", "mlr", "keras", "tensorflow", "sf", "PrevMap", "randomForest", "xgboost", "data.table")

for(p in pkgs) {
  if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}  
}

for(p in pkgs) {
  suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
}
rm('p','pkgs')  

#install_keras()
source("R_Client.R") #apirequest
source("generationMaster.R") #sampling approach
source("visMaster.R") #script for visualisation aides
source("supportvector.R") # svm implemented
source("keras.R") # ann implemented
source("xgboost.R") # xgboost implemented
source("rf.R") # random forest implemented
source("knn.R") # knn implemented
source("multiOptimization.R") # functions for multiobjective optimisation including sourcing of python scripts etc. 

#Source python for optimization purposes
use_python("/usr/local/bin/python")
source_python('nsga.py')
