library(parallel)
library(ranger)
library(mlr)
library(dplyr)
library(tuneRanger)

# set wd-------
setwd("//pc.istat.it/xendesktop/DaaS/ilaria.bombelli/Desktop/GruppiDiLAvoro/Progetto_ESSNET/Random_Forest/Application")

#load input data: ---------
load("Output/01.input_RF_sperim3.RData")


# fit the model on training data -----------
start_time_tuning <- Sys.time()

sample.task = makeClassifTask(data = data_train %>% select(-CODICE_INDIVIDUO, -MS), target = "TITSTU_CDIFF_22")
# Create a classification task.
#da pacchetto mlr
# Estimate runtime
#estimateTimeTuneRanger(sample.task)

tuning =tuneRanger(sample.task, 
                   measure = list(multiclass.brier), 
                   num.trees = 50, #default is 1000
                   num.threads = detectCores(), 
                   iters = 70,
                   tune.parameters = c( "mtry", "min.node.size"),
                   #"sample.fraction"),
                   # we cannot include num.trees inside tune parameters
                   
                   save.file.path = NULL)
end_time_tuning <- Sys.time() 
execution_time_tuning <- end_time_tuning - start_time_tuning
print(execution_time_tuning)

# tuning$recommended.pars$mtry
# tuning$recommended.pars$min.node.size
# tuning$recommended.pars$sample.fraction (if asked)
# tuning$model$learner.model$num.trees (if asked)


save(execution_time_tuning,tuning, file = "Output/tuning.RData")

