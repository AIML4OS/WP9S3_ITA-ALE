library(parallel)
library(ranger)

# set wd-------
setwd("//pc.istat.it/xendesktop/DaaS/ilaria.bombelli/Desktop/GruppiDiLAvoro/Progetto_ESSNET/Random_Forest/Application")

#load input data: ---------
load("Output/01.input_RF_sperim3.RData")

# fit the model on training data -----------
mtry=ncol(data_train)-3-2
#ncol(data_train)-3 is just the number of covariates (-Y, -CODICE_INDIVIDUO, -MS)

start_time <- Sys.time()
rf_sperim3=ranger::ranger(
  formula = TITSTU_CDIFF_22
  ~. - CODICE_INDIVIDUO - MS,
  data = data_train,
  num.trees = 50,
  mtry= mtry,
  #default is the (rounded down) square root of the number variables. Number of variables to be sampled for the splitting at each tree
  min.node.size = 4, # a-priori set
  sample.fraction = 1, #bootstrap (sampling with replacement)
  #max.depth = 5, 
  importance = "impurity", #Gini index
  probability = TRUE, 
  respect.unordered.factors='partition',
  # When treating a categorical variable as nominal
  num.threads = detectCores()
)

end_time <- Sys.time() 
execution_time <- end_time - start_time 
print(execution_time)


save(rf_sperim3, start_time, end_time, execution_time, 
     file = "Output/RF_sperim3.RData")

