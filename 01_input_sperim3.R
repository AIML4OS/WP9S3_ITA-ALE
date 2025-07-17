library(dplyr) #data manipulation
library(tidyr) #data manipulation
library(tidyverse) #data manipulation
library(fastDummies) # to create dummies from factors
library(plotly) #plots
library(tidymodels) #ROC, AUC
library(mlr)
library(haven) #sas dataset
library(ranger) # for random forest model
library(tuneRanger)
library(caret) # for confusion matrix
library(caTools) # for data splitting
library(xlsx) #to export results in xlsx

# set wd-------
setwd("//pc.istat.it/xendesktop/DaaS/ilaria.bombelli/Desktop/GruppiDiLAvoro/Progetto_ESSNET/Random_Forest/Application")

#load data -------
#load data -------
# dataset of records not sampled
base22_ER <- read.csv(file= "//hyperv4balbo/MLMEBMEC/Ensemble/dati/dati22_ER.csv")
# 4136629

base22_ER_NOMS <- base22_ER %>% filter(is.na(TITSTU_CDIFF_22))


# dataset of sampled records (records belonging to the MS) and expanded (repeated a number of times given by record's sampling weights)
base22_ER_MS <- read.csv(file= "//hyperv4balbo/MLMEBMEC/Ensemble/dati/dati22_ER_MSexp.csv")
# 4137319
summary(base22_ER_MS$PESO_CAL_PROV)

#
length(unique(base22_ER_MS$CODICE_INDIVIDUO))
# 176982 (number of unique records in MS)
prova= base22_ER %>% filter(!is.na(TITSTU_CDIFF_22))
length(unique(prova$CODICE_INDIVIDUO))
# 176982


length(unique(base22_ER_NOMS$CODICE_INDIVIDUO))
#3959647
nrow(base22_ER_NOMS)
# 3959647

# merge
setdiff(colnames(base22_ER_MS), colnames(base22_ER_NOMS))
base22_ER_NOMS <- base22_ER_NOMS[,colnames(base22_ER_MS)]
table(base22_ER_MS$TITSTU_CDIFF_22,  useNA="ifany")
table(base22_ER_NOMS$TITSTU_CDIFF_22, useNA="ifany")
base22_all= rbind(base22_ER_MS, base22_ER_NOMS)
# here: 
#- 4137319 record expanded from MS (i.e. 176982 unique record)
#- 3959647 record NO MS 
# key identifier is titstudio NA or not
# add the flag
base22_all$MS = ifelse(is.na(base22_all$TITSTU_CDIFF_22), 0, 1)
table(base22_all$MS, base22_all$TITSTU_CDIFF_22, useNA = "ifany")





# data manipulation ---------
str(base22_all)
#focus only on some variables:

base22_all = base22_all %>% select(
  #demographic variables
  CODICE_INDIVIDUO, 
  COD_REG_RESIDENZA, 
  COD_PROV_RESIDENZA,
  ETA_22, SESSO,
  FL_ITA,
  
  # Popolazione di appartenenza
  POP_ABC_2021,
  # Education variable from APR4 (4 categories) - 2019, 2020, 2022
  #TS_APR4_2019,
  TS_APR4_2020, 
  TS_APR4_2022,
  # Education level on 2018, 2019, 2020, 2021 (from Bit o Censimento 2011)
  #G_ISTR_2018_CDIFF_CORR,   G_ISTR_2019_CDIFF_CORR, 
  G_ISTR_2020_CDIFF_CORR,  
  G_ISTR_2021_CDIFF_CORR,
  # Attained level of Education observed from the Master Sample MS 2019, 2021, 2022
  #TITSTU_CDIFF_19, TITSTU_CDIFF_21, 
  TITSTU_CDIFF_22,
  # additional variables
  #school and year of attendance - a.s. 18/19
  # VAR19, VAR20, 
  VAR21, 
  VAR22,
  # #year of attendance - a.s. 17/18
  # FR18_ANNO_CORSO, FR19_ANNO_CORSO, FR20_ANNO_CORSO, FR21_ANNO_CORSO, FR22_ANNO_CORSO,
  # # Codice istituto - a.s. 17/18
  # FR18_CODICE_ISTITUTO, FR19_CODICE_ISTITUTO, FR20_CODICE_ISTITUTO, FR21_CODICE_ISTITUTO, FR22_CODICE_ISTITUTO, 
  # # School grade - a.s. 17/18
  # FR18_CODICE_STATO, FR19_CODICE_STATO,  FR20_CODICE_STATO, 
  FR21_CODICE_STATO,
  FR22_CODICE_STATO,
  # #evening school - a.s. 17/18
  # FR18_SERALE, FR19_SERALE, FR20_SERALE, 
  FR21_SERALE,
  FR22_SERALE,
  # Attendance status - a.s. 17/18
  # FR18_STATO_FREQUENZA, FR19_STATO_FREQUENZA, FR20_STATO_FREQUENZA,  
  FR21_STATO_FREQUENZA,
  FR22_STATO_FREQUENZA,
  # #type of school - a.s.17/18
  # FR18_TIPO_SCU, FR19_TIPO_SCU,  FR20_TIPO_SCU, 
  FR21_TIPO_SCU,  
  FR22_TIPO_SCU,
  # #flag=1 if School is Liceo - a.s. 17/18
  # FR18_TIPO_SCUOLA, FR19_TIPO_SCUOLA, FR20_TIPO_SCUOLA, FR21_TIPO_SCUOLA,  FR22_TIPO_SCUOLA
  MS
)

str(base22_all)



# build VAR22_ord from VAR22 (make it ordinal)
# E-1	E-2	E-3	E-4	E-5	M-1	M-2	M-3	S3-1	S3-2	S3-3	S3-4	S5-1	S5-2	S5-3	S5-4	S5-5	S5-6	I2-1	I2-2	I2-3	I2-4	I2-5	L3-1	L3-2	L3-3	L3-5+	L2-1	L2-2	L2-3	L2-4+	L5-1	L5-2	L5-3	L5-4	L5-5	L5-6+	D-1	D-2	D-3	D-4	D-5	NC-1	NC-2	NC-3	NC-4	NC-5	99

base22_all = base22_all %>% mutate(VAR22_ord= case_when(
  VAR22=="E-1" ~ "1", 
  VAR22=="E-2" ~ "2",
  VAR22=="E-3" ~ "3", 
  VAR22=="E-4" ~ "4", 
  VAR22=="E-5" ~ "5", 
  VAR22=="M-1" ~ "6", 
  VAR22=="M-2" ~ "7", 
  VAR22=="M-3" ~ "8", 
  VAR22=="S3-1" ~ "9", 
  VAR22=="S3-2" ~ "10", 
  VAR22=="S3-3" ~ "11", 
  VAR22=="S3-4" ~ "12", 
  VAR22=="S5-1" ~ "13", 
  VAR22=="S5-2" ~ "14",
  VAR22=="S5-3" ~ "15", 
  VAR22=="S5-4" ~ "16",
  VAR22=="S5-5" ~ "17",
  VAR22=="S5-6" ~ "18", 
  VAR22=="I2-1" ~ "19",
  VAR22=="I2-2" ~ "20",
  VAR22=="I2-3" ~ "21", 
  VAR22=="I2-4" ~ "22", 
  VAR22=="I2-5" ~ "23", 
  VAR22=="L3-1" ~ "24", 
  VAR22=="L3-2" ~ "25", 
  VAR22=="L3-3" ~ "26", 
  VAR22=="L3-5+" ~ "27",
  VAR22=="L2-1" ~ "28",
  VAR22=="L2-2" ~ "29", 
  VAR22=="L2-3" ~ "30",
  VAR22=="L2-4+" ~ "31",
  VAR22=="L5-1" ~ "32",
  VAR22=="L5-2" ~ "33", 
  VAR22=="L5-3" ~ "34",
  VAR22=="L5-4" ~ "35",
  VAR22=="L5-5" ~ "36",
  VAR22=="L5-6+" ~ "37", 
  VAR22=="D-1" ~ "38",
  VAR22=="D-2" ~ "39",
  VAR22=="D-3" ~ "40",
  VAR22=="D-4" ~ "41",
  VAR22=="D-5" ~ "42",
  VAR22=="NC-1" ~ "43",
  VAR22=="NC-2" ~ "44",
  VAR22=="NC-3" ~ "45",
  VAR22=="NC-4" ~ "46",
  VAR22=="NC-5" ~ "47",
  .default = "99" ))


# build VAR21_ord from VAR21 (make it ordinal)
base22_all = base22_all %>% mutate(VAR21_ord= case_when(
  VAR21=="E-1" ~ "1", 
  VAR21=="E-2" ~ "2",
  VAR21=="E-3" ~ "3", 
  VAR21=="E-4" ~ "4", 
  VAR21=="E-5" ~ "5", 
  VAR21=="M-1" ~ "6", 
  VAR21=="M-2" ~ "7", 
  VAR21=="M-3" ~ "8", 
  VAR21=="S3-1" ~ "9", 
  VAR21=="S3-2" ~ "10", 
  VAR21=="S3-3" ~ "11", 
  VAR21=="S3-4" ~ "12", 
  VAR21=="S5-1" ~ "13", 
  VAR21=="S5-2" ~ "14",
  VAR21=="S5-3" ~ "15", 
  VAR21=="S5-4" ~ "16",
  VAR21=="S5-5" ~ "17",
  VAR21=="S5-6" ~ "18", 
  VAR21=="I2-1" ~ "19",
  VAR21=="I2-2" ~ "20",
  VAR21=="I2-3" ~ "21", 
  VAR21=="I2-4" ~ "22", 
  VAR21=="I2-5" ~ "23", 
  VAR21=="L3-1" ~ "24", 
  VAR21=="L3-2" ~ "25", 
  VAR21=="L3-3" ~ "26", 
  VAR21=="L3-5+" ~ "27",
  VAR21=="L2-1" ~ "28",
  VAR21=="L2-2" ~ "29", 
  VAR21=="L2-3" ~ "30",
  VAR21=="L2-4+" ~ "31",
  VAR21=="L5-1" ~ "32",
  VAR21=="L5-2" ~ "33", 
  VAR21=="L5-3" ~ "34",
  VAR21=="L5-4" ~ "35",
  VAR21=="L5-5" ~ "36",
  VAR21=="L5-6+" ~ "37", 
  VAR21=="D-1" ~ "38",
  VAR21=="D-2" ~ "39",
  VAR21=="D-3" ~ "40",
  VAR21=="D-4" ~ "41",
  VAR21=="D-5" ~ "42",
  VAR21=="NC-1" ~ "43",
  VAR21=="NC-2" ~ "44",
  VAR21=="NC-3" ~ "45",
  VAR21=="NC-4" ~ "46",
  VAR21=="NC-5" ~ "47",
  .default = "99" ))



base22_all = base22_all %>% mutate(FR22_CODICE_STATO_new= case_when(
  FR22_CODICE_STATO %in% c("M1", "M2", "SL","SM","SA","SB","SC","SD","SP","SR","SS","SV","DL","DA","DF","DU","SF","A1","TS","A2","EE","MA","CP","SI")  ~ "NC",
  .default = FR22_CODICE_STATO ))

base22_all = base22_all %>% mutate(FR21_CODICE_STATO_new= case_when(
  FR21_CODICE_STATO %in% c("M1", "M2", "SL","SM","SA","SB","SC","SD","SP","SR","SS","SV","DL","DA","DF","DU","SF","A1","TS","A2","EE","MA","CP","SI")  ~ "NC",
  .default = FR21_CODICE_STATO ))


base22_all = base22_all %>% select(-VAR22, -VAR21, -FR22_CODICE_STATO, -FR21_CODICE_STATO)

# convert some variables from numeric to factor
base22_all = base22_all %>% mutate(across(c(where(is.numeric), -ETA_22, -TS_APR4_2022, -TS_APR4_2020, -G_ISTR_2020_CDIFF_CORR, -G_ISTR_2021_CDIFF_CORR, -TITSTU_CDIFF_22
                                          #-PESO_CAL_PROV
), as.factor)) %>% mutate(across(where(is.character), as.factor))

str(base22_all)
# 'data.frame':	8096966 obs. of  22 variables:
#   $ CODICE_INDIVIDUO      : Factor w/ 4136629 levels "10","101","141",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ COD_PROV_RESIDENZA    : Factor w/ 9 levels "33","34","35",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ ETA_22                : int  44 44 44 44 44 44 44 44 44 44 ...
# $ SESSO                 : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
# $ FL_ITA                : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ POP_ABC_2021          : Factor w/ 3 levels "A","B","C": 3 3 3 3 3 3 3 3 3 3 ...
# $ TS_APR4_2020          : int  4 4 4 4 4 4 4 4 4 4 ...
# $ TS_APR4_2022          : int  4 4 4 4 4 4 4 4 4 4 ...
# $ G_ISTR_2020_CDIFF_CORR: int  NA NA NA NA NA NA NA NA NA NA ...
# $ G_ISTR_2021_CDIFF_CORR: int  NA NA NA NA NA NA NA NA NA NA ...
# $ TITSTU_CDIFF_22       : int  7 7 7 7 7 7 7 7 7 7 ...
# $ FR21_SERALE           : Factor w/ 2 levels "0","1": NA NA NA NA NA NA NA NA NA NA ...
# $ FR22_SERALE           : Factor w/ 2 levels "0","1": NA NA NA NA NA NA NA NA NA NA ...
# $ FR21_STATO_FREQUENZA  : Factor w/ 9 levels "1","2","3","4",..: NA NA NA NA NA NA NA NA NA NA ...
# $ FR22_STATO_FREQUENZA  : Factor w/ 4 levels "1","2","3","4": NA NA NA NA NA NA NA NA NA NA ...
# $ FR21_TIPO_SCU         : Factor w/ 28 levels "","1E","1M","EE",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ FR22_TIPO_SCU         : Factor w/ 28 levels "","1E","1M","EE",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ MS                    : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ VAR22_ord             : Factor w/ 48 levels "1","10","11",..: 48 48 48 48 48 48 48 48 48 48 ...
# $ VAR21_ord             : Factor w/ 47 levels "1","10","11",..: 47 47 47 47 47 47 47 47 47 47 ...
# $ FR22_CODICE_STATO_new : Factor w/ 16 levels "","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ FR21_CODICE_STATO_new : Factor w/ 18 levels "","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
class(base22_all$G_ISTR_2021_CDIFF_CORR)
#table(base22_all$VAR22)
# base22_all_orig=base22_all
set.seed(123)
#base22_all=base22_all[sample(1:nrow(base22_all), 1000),]



#NA imputation-----
#with out-of-range valuesn (int 99, factor "99")

# from BLANK values to NA
#check where/if blank: 
apply(base22_all, 2, function(x) unique(x))

# FR22_TIPO_SCU with blank
base22_all$FR22_TIPO_SCU[base22_all$FR22_TIPO_SCU==""]=NA
base22_all$FR22_TIPO_SCU=droplevels(base22_all$FR22_TIPO_SCU)

# FR21_TIPO_SCU with blank
base22_all$FR21_TIPO_SCU[base22_all$FR21_TIPO_SCU==""]=NA
base22_all$FR21_TIPO_SCU=droplevels(base22_all$FR21_TIPO_SCU)


# FR22_CODICE_STATO_new with blank
base22_all$FR22_CODICE_STATO_new[base22_all$FR22_CODICE_STATO_new==""]=NA
base22_all$FR22_CODICE_STATO_new=droplevels(base22_all$FR22_CODICE_STATO_new)

# FR21_CODICE_STATO_new with blank
base22_all$FR21_CODICE_STATO_new[base22_all$FR21_CODICE_STATO_new==""]=NA
base22_all$FR21_CODICE_STATO_new=droplevels(base22_all$FR21_CODICE_STATO_new)

# count NA
summary(base22_all)
apply(base22_all, 2, function(x) sum(is.na(x)))
# 
# CODICE_INDIVIDUO     COD_PROV_RESIDENZA     ETA_22 
# 0                      0                      0 
# SESSO                 FL_ITA           POP_ABC_2021 
# 0                      0                      0 
# TS_APR4_2020           TS_APR4_2022 G_ISTR_2020_CDIFF_CORR 
# 6806684                6640899                 485998 
# G_ISTR_2021_CDIFF_COR TITSTU_CDIFF_22       FR21_SERALE 
# 481434                3959647                7701641 
# FR22_SERALE   FR21_STATO_FREQUENZA   FR22_STATO_FREQUENZA 
# 7696222                7128911                7222677 
# FR21_TIPO_SCU          FR22_TIPO_SCU               MS 
# 7149595                7225117                      0 
# VAR22_ord              VAR21_ord  FR22_CODICE_STATO_new 
# 0                      0                6009751 
# FR21_CODICE_STATO_new 
# 6009759 

# convert NA to out-of-range values.

base22_all = base22_all %>% mutate(
  TS_APR4_2020 = if_else(is.na(TS_APR4_2020), 99, TS_APR4_2020),
  TS_APR4_2022 = if_else(is.na(TS_APR4_2022), 99, TS_APR4_2022),
  G_ISTR_2020_CDIFF_CORR = if_else(is.na(G_ISTR_2020_CDIFF_CORR), 99, G_ISTR_2020_CDIFF_CORR),
  G_ISTR_2021_CDIFF_CORR = if_else(is.na(G_ISTR_2021_CDIFF_CORR), 99, G_ISTR_2021_CDIFF_CORR), 
  #VAR22 = fct_na_value_to_level(VAR22, level="99"),
  #originale ma trasforma in character: if_else(is.na(VAR22), "99", VAR22), #non necessario perchÃÂÃÂÃÂÃÂ© VAR22 non ha NA
  FR21_SERALE = fct_na_value_to_level(FR21_SERALE, level="99"),
  FR22_SERALE = fct_na_value_to_level(FR22_SERALE, level="99"),
  #originale ma traforma in character: if_else(is.na(FR22_SERALE), "99", FR22_SERALE),
  FR21_STATO_FREQUENZA = fct_na_value_to_level(FR21_STATO_FREQUENZA, level = "99"),
  FR22_STATO_FREQUENZA = fct_na_value_to_level(FR22_STATO_FREQUENZA, level = "99"),
  #if_else(is.na(FR22_STATO_FREQUENZA), "99", FR22_STATO_FREQUENZA),
  FR21_TIPO_SCU = fct_na_value_to_level(FR21_TIPO_SCU, level="99"), 
  FR22_TIPO_SCU = fct_na_value_to_level(FR22_TIPO_SCU, level="99"), 
  #originale ma trasforma in character: if_else(is.na(FR22_TIPO_SCU), "99", FR22_TIPO_SCU), #non necessario perchÃÂÃÂÃÂÃÂ© VAR22 non ha NA
  FR21_CODICE_STATO_new = fct_na_value_to_level(FR21_CODICE_STATO_new, level="99"),
  FR22_CODICE_STATO_new = fct_na_value_to_level(FR22_CODICE_STATO_new, level="99")
  #originale ma trasforma in character: if_else(is.na(FR22_CODICE_STATO_new), "99", FR22_CODICE_STATO_new)
  
)

summary(base22_all)

apply(base22_all, 2, function(x) sum(is.na(x)))  #no NA
apply(base22_all, 2, function(x) unique(x))

# 
# VAR22_ord from factor to numeric
base22_all$VAR22_ord_n=as.numeric(base22_all$VAR22_ord)
#VAR21_ord from factor to numeric
base22_all$VAR21_ord_n=as.numeric(base22_all$VAR21_ord)

base22_all$VAR22_ord=NULL
base22_all$VAR21_ord=NULL

#
str(base22_all)
# adesso: 
# 'data.frame':	8096966 obs. of  22 variables:
#   $ CODICE_INDIVIDUO      : Factor w/ 4136629 levels "10","101","141",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ COD_PROV_RESIDENZA    : Factor w/ 9 levels "33","34","35",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ ETA_22                : int  44 44 44 44 44 44 44 44 44 44 ...
# $ SESSO                 : Factor w/ 2 levels "F","M": 2 2 2 2 2 2 2 2 2 2 ...
# $ FL_ITA                : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ POP_ABC_2021          : Factor w/ 3 levels "A","B","C": 3 3 3 3 3 3 3 3 3 3 ...
# $ TS_APR4_2020          : num  4 4 4 4 4 4 4 4 4 4 ...
# $ TS_APR4_2022          : num  4 4 4 4 4 4 4 4 4 4 ...
# $ G_ISTR_2020_CDIFF_CORR: num  99 99 99 99 99 99 99 99 99 99 ...
# $ G_ISTR_2021_CDIFF_CORR: num  99 99 99 99 99 99 99 99 99 99 ...
# $ TITSTU_CDIFF_22       : int  7 7 7 7 7 7 7 7 7 7 ...
# $ FR21_SERALE           : Factor w/ 3 levels "0","1","99": 3 3 3 3 3 3 3 3 3 3 ...
# $ FR22_SERALE           : Factor w/ 3 levels "0","1","99": 3 3 3 3 3 3 3 3 3 3 ...
# $ FR21_STATO_FREQUENZA  : Factor w/ 10 levels "1","2","3","4",..: 10 10 10 10 10 10 10 10 10 10 ...
# $ FR22_STATO_FREQUENZA  : Factor w/ 5 levels "1","2","3","4",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ FR21_TIPO_SCU         : Factor w/ 28 levels "1E","1M","EE",..: 28 28 28 28 28 28 28 28 28 28 ...
# $ FR22_TIPO_SCU         : Factor w/ 28 levels "1E","1M","EE",..: 28 28 28 28 28 28 28 28 28 28 ...
# $ MS                    : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
# $ FR22_CODICE_STATO_new : Factor w/ 15 levels "1","2","3","3P",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ FR21_CODICE_STATO_new : Factor w/ 17 levels "1","2","3","3P",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ VAR22_ord_n           : num  48 48 48 48 48 48 48 48 48 48 ...
# $ VAR21_ord_n           : num  47 47 47 47 47 47 47 47 47 47 ..



# make NOMINAL categorical variable dummy, i.e. one variable for each category: 
#POP_ABC_2021, FR22_STATO_FREQUENZA, FR21_STATO_FREQUENZA, FR22_SERALE,FR21_SERALE FR22_TIPO_SCU, FR21_TIPO_SCU, FR22_CODICE_STATO_new, FR21_CODICE_STATO_new
base22_all_orig=base22_all
base22_all <- fastDummies::dummy_cols(base22_all, select_columns = c("COD_PROV_RESIDENZA", "POP_ABC_2021", "FR22_STATO_FREQUENZA", "FR21_STATO_FREQUENZA", "FR22_SERALE", "FR21_SERALE", "FR22_TIPO_SCU", "FR21_TIPO_SCU",  "FR22_CODICE_STATO_new", "FR21_CODICE_STATO_new"), remove_selected_columns=TRUE)
# check: 
ncol(base22_all)
ncol(base22_all_orig)-10+
  nlevels(base22_all_orig$COD_PROV_RESIDENZA)+
  nlevels(base22_all_orig$FR22_STATO_FREQUENZA)+
  nlevels(base22_all_orig$FR21_STATO_FREQUENZA)+
  nlevels(base22_all_orig$POP_ABC_2021)+
  nlevels(base22_all_orig$FR21_SERALE)+
  nlevels(base22_all_orig$FR22_SERALE)+
  nlevels(base22_all_orig$FR21_TIPO_SCU)+
  nlevels(base22_all_orig$FR22_TIPO_SCU)+
  nlevels(base22_all_orig$FR21_CODICE_STATO_new)+
  nlevels(base22_all_orig$FR22_CODICE_STATO_new)

summary(base22_all)
str(base22_all)
# new variables are not factor---> transform them

base22_all=base22_all %>% mutate(across(c(where(is.numeric), -ETA_22,
                                        -TS_APR4_2020, -TS_APR4_2022,
                                        -G_ISTR_2020_CDIFF_CORR,
                                        -G_ISTR_2021_CDIFF_CORR,
                                        -TITSTU_CDIFF_22,
                                        -VAR21_ord_n,
                                        -VAR22_ord_n,
                                        #-PESO_CAL_PROV
), as.factor)) %>% mutate(across(where(is.character), as.factor))

str(base22_all)




# split train test according to flag MS.


data_train <- base22_all[base22_all$MS==1, ] #master sample (where the attained level of education, i.e. TITSTU_CDIFF_CORR (target variables), is observed). It is equivalent to base22_ER_MS
data_test <- base22_all[base22_all$MS==0, ] # beyond the sample
#

dim(data_train) # dimension/shape of train dataset
head(data_train)
dim(data_test)  # dimension/shape of test dataset
head(data_test)

save(base22_all, data_train, data_test, file="Output/01.input_Rf_sperim3.RData")
