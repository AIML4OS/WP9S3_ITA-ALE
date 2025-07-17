library(xlsx)
library(metrica)
library(caret)
library(ranger)
library(dplyr)
library(tidyr)
options(scipen = "999")
# set wd-----------
setwd("//pc.istat.it/xendesktop/DaaS/ilaria.bombelli/Desktop/GruppiDiLAvoro/Progetto_ESSNET/Random_Forest/Application")

# load output---------
load("Output/RF_sperim3.RData")

# load input--------
load("Output/01.input_RF_sperim3.RData")


# use the model to make predictions on test data --------------

rf_test= predict(rf_sperim3, data=data_test)
rf_test_prob = predictions(rf_test)

#check 
dim(rf_test_prob)
summary(rowSums(rf_test_prob))

#predictions
set.seed(123)
# sample the class from the predicted distribution:
pred_rf = apply(rf_test_prob, 1, function(x) { return(sample(colnames(rf_sperim3$predictions), size=1, prob=x, replace=FALSE))})


pred_rf=as.factor(pred_rf)
data_test$TITSTU_CDIFF_22=as.factor(data_test$TITSTU_CDIFF_22) 
table(data_test$TITSTU_CDIFF_22, useNA = "ifany")# tutti NA
pred_rf_df=data.frame(pred_rf=pred_rf)

#feature importance------
importance(rf_sperim3)
imp=importance(rf_sperim3)[order(importance(rf_sperim3), decreasing = TRUE)]
png(filename="Output/01_feature_imp_rf_sperim3.png", width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200)
barplot(imp,names.arg=names(imp), angle=45, las = 2, 
        cex.names = 0.5, main="Feature importance. Gini")
dev.off()

head(imp)

imp2=data.frame(Variable=names(imp), Gini_index=imp)
write.xlsx(x=imp2, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Feature importance", row.names = FALSE) 


# performance MACRO: 
# evaluate distribution
# benchmark: distribution of ALE in the expanded MS
# comparison with obtained ALE, i.e. predicted (for beyond the sample units) +  Observed (for sampled units)


# 1) Predicted RF +  Observed in MS vs Observed in the MS expanded

# distribution-------------------



# whole procedure: predicted ALE for non sampled units and observed for sampled units
nonMS=data_test
nonMS$TITSTU_CDIFF_22=pred_rf_df$pred_rf
MS=data_train %>% distinct()
all=rbind(MS, nonMS)
length(unique(all$CODICE_INDIVIDUO))
nrow(all)


rm(list=setdiff(ls(),list("all", "data_train", "data_test", "MS", "nonMS", "pred_rf_df")))
distr=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)
distr$abs_pred_freq=unname(table(all$TITSTU_CDIFF_22, useNA="ifany"))
distr$abs_obs_freq=unname(table(data_train$TITSTU_CDIFF_22, useNA="ifany"))
distr$rel_pred_freq=unname(round(prop.table(table(all$TITSTU_CDIFF_22, useNA="ifany")),3))
distr$rel_obs_freq=unname(round(prop.table(table(data_train$TITSTU_CDIFF_22, useNA="ifany")),3))
distr$diff_rel_freq=distr$rel_obs_freq-distr$rel_pred_freq
distr$rel_diff_rel_freq=round((distr$rel_obs_freq-distr$rel_pred_freq)/distr$rel_obs_freq,3)

write.xlsx(x=distr, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution", row.names = FALSE, append = TRUE) 


# Separately by Population
all_A = all %>% filter(POP_ABC_2021_A=="1")
length(unique(all_A$CODICE_INDIVIDUO))
nrow(all_A)
all_B = all %>% filter(POP_ABC_2021_B=="1")
length(unique(all_B$CODICE_INDIVIDUO))
nrow(all_B)
all_C = all %>% filter(POP_ABC_2021_C=="1")
length(unique(all_C$CODICE_INDIVIDUO))
nrow(all_C)
# check
nrow(all_A) + nrow(all_B) + nrow(all_C)
nrow(all)
#
data_train_A=data_train %>% filter(POP_ABC_2021_A=="1")
data_train_B=data_train %>% filter(POP_ABC_2021_B=="1")
data_train_C=data_train %>% filter(POP_ABC_2021_C=="1")
nrow(data_train_A)+nrow(data_train_B)+nrow(data_train_C)
nrow(data_train)
# 
distr_A=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_A$abs_pred_freq=unname(table(all_A$TITSTU_CDIFF_22, useNA="ifany"))
distr_A$abs_obs_freq=unname(table(data_train_A$TITSTU_CDIFF_22, useNA="ifany"))
distr_A$rel_pred_freq=unname(round(prop.table(table(all_A$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_A$rel_obs_freq=unname(round(prop.table(table(data_train_A$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_A$diff_rel_freq=distr_A$rel_obs_freq-distr_A$rel_pred_freq
distr_A$rel_diff_rel_freq=round((distr_A$rel_obs_freq-distr_A$rel_pred_freq)/distr_A$rel_obs_freq,3)

write.xlsx(x=distr_A, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-Pop_A", row.names = FALSE, append = TRUE) 
#
distr_B=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_B$abs_pred_freq=unname(table(all_B$TITSTU_CDIFF_22, useNA="ifany"))
distr_B$abs_obs_freq=unname(table(data_train_B$TITSTU_CDIFF_22, useNA="ifany"))
distr_B$rel_pred_freq=unname(round(prop.table(table(all_B$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_B$rel_obs_freq=unname(round(prop.table(table(data_train_B$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_B$diff_rel_freq=distr_B$rel_obs_freq-distr_B$rel_pred_freq
distr_B$rel_diff_rel_freq=round((distr_B$rel_obs_freq-distr_B$rel_pred_freq)/distr_B$rel_obs_freq,3)

write.xlsx(x=distr_B, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-Pop_B", row.names = FALSE, append = TRUE) 
# 
distr_C=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_C$abs_pred_freq=unname(table(all_C$TITSTU_CDIFF_22, useNA="ifany"))
distr_C$abs_obs_freq=unname(table(data_train_C$TITSTU_CDIFF_22, useNA="ifany"))
distr_C$rel_pred_freq=unname(round(prop.table(table(all_C$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_C$rel_obs_freq=unname(round(prop.table(table(data_train_C$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_C$diff_rel_freq=distr_C$rel_obs_freq-distr_C$rel_pred_freq
distr_C$rel_diff_rel_freq=round((distr_C$rel_obs_freq-distr_C$rel_pred_freq)/distr_C$rel_obs_freq,3)

write.xlsx(x=distr_C, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-Pop_C", row.names = FALSE, append = TRUE) 

# Plots----------------
library(ggplot2)

# distribuzione TITSTU predetta (predetta per non MS e osservata per MS)
# predetti
all$TITSTU_CDIFF_22=as.factor(all$TITSTU_CDIFF_22)
ggplot(all, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted.png",device="png")

# observed

ggplot(data_train, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

ggsave("Output/sperim3_observed.png",device="png")

# side by side
plot1 <- ggplot(all, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# distribuzioni percentuali affiancate
toplot=data.frame(TITSTU_CDIFF_22=c(data_train$TITSTU_CDIFF_22, all$TITSTU_CDIFF_22))
toplot$type=c(rep("Obs in MS_exp",length(data_train$TITSTU_CDIFF_22)), rep("Pred in Pop",length(all$TITSTU_CDIFF_22)))
toplot$type=as.factor(toplot$type)

# overall percentage distribution comparison
toplot = toplot %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot=as.data.frame(toplot)
toplot = toplot %>% distinct()
# check 
toplot %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot, aes(x = TITSTU_CDIFF_22, y = prop
             , fill = type
  )) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison2.png",device="png", width = 12, height = 8, units="in")




# Separately by Population

# POP_A
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_A$TITSTU_CDIFF_22=as.factor(all_A$TITSTU_CDIFF_22)
ggplot(all_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_Pop_A.png",device="png")

# observed

ggplot(data_train_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

ggsave("Output/sperim3_observed_Pop_A.png",device="png")

# side by side
plot1 <- ggplot(all_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_Pop_A.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distribution
toplot_A=data.frame(TITSTU_CDIFF_22=c(data_train_A$TITSTU_CDIFF_22, all_A$TITSTU_CDIFF_22))
toplot_A$type=c(rep("Obs in MS_exp in Pop_A",length(data_train_A$TITSTU_CDIFF_22)), rep("Pred in Pop_A",length(all_A$TITSTU_CDIFF_22)))
toplot_A$type=as.factor(toplot_A$type)

# overall percentage distribution comparison
toplot_A = toplot_A %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_A=as.data.frame(toplot_A)
toplot_A = toplot_A %>% distinct()
# check 
toplot_A %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_A, aes(x = TITSTU_CDIFF_22, y = prop
                   , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_Pop_A.png",device="png", width = 12, height = 8, units="in")



# POP_B
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_B$TITSTU_CDIFF_22=as.factor(all_B$TITSTU_CDIFF_22)
ggplot(all_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_Pop_B.png",device="png")

# observed

ggplot(data_train_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

ggsave("Output/sperim3_observed_Pop_B.png",device="png")

# side by side
plot1 <- ggplot(all_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_Pop_B.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distributions
toplot_B=data.frame(TITSTU_CDIFF_22=c(data_train_B$TITSTU_CDIFF_22, all_B$TITSTU_CDIFF_22))
toplot_B$type=c(rep("Obs in MS_exp in Pop_B",length(data_train_B$TITSTU_CDIFF_22)), rep("Pred in Pop_B",length(all_B$TITSTU_CDIFF_22)))
toplot_B$type=as.factor(toplot_B$type)

# overall percentage distribution comparison
toplot_B = toplot_B %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_B=as.data.frame(toplot_B)
toplot_B = toplot_B %>% distinct()
# check 
toplot_B %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_B, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_Pop_B.png",device="png", width = 12, height = 8, units="in")


# POP_C
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_C$TITSTU_CDIFF_22=as.factor(all_C$TITSTU_CDIFF_22)
ggplot(all_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_Pop_C.png",device="png")

# observed

ggplot(data_train_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

ggsave("Output/sperim3_observed_Pop_C.png",device="png")

# side by side
plot1 <- ggplot(all_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_Pop_C.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distribution
toplot_C=data.frame(TITSTU_CDIFF_22=c(data_train_C$TITSTU_CDIFF_22, all_C$TITSTU_CDIFF_22))
toplot_C$type=c(rep("Obs in MS_exp in Pop_C",length(data_train_C$TITSTU_CDIFF_22)), rep("Pred in Pop_C",length(all_C$TITSTU_CDIFF_22)))
toplot_C$type=as.factor(toplot_C$type)

# overall percentage distribution comparison
toplot_C = toplot_C %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_C=as.data.frame(toplot_C)
toplot_C = toplot_C %>% distinct()
# check 
toplot_C %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_C, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_Pop_C.png",device="png", width = 12, height = 8, units="in")


















# 1) Predicted Log-Linear (official procedure) +  Observed in MS 
#vs 
#Observed in the MS expanded

# build dataset:
base22_ER <- read.csv(file= "//hyperv4balbo/MLMEBMEC/Ensemble/dati/dati22_ER.csv")

all_ll=base22_ER


all_ll$TITSTU_CDIFF_22<-all_ll$IST_IMP22
anyNA(all_ll$TITSTU_CDIFF_22)


# distribution-------------------



# offical procedure: predicted with log linear and deterministically imputed

distr=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)
distr$abs_pred_freq=unname(table(all_ll$TITSTU_CDIFF_22, useNA="ifany"))
distr$abs_obs_freq=unname(table(data_train$TITSTU_CDIFF_22, useNA="ifany"))
distr$rel_pred_freq=unname(round(prop.table(table(all_ll$TITSTU_CDIFF_22, useNA="ifany")),3))
distr$rel_obs_freq=unname(round(prop.table(table(data_train$TITSTU_CDIFF_22, useNA="ifany")),3))
distr$diff_rel_freq=distr$rel_obs_freq-distr$rel_pred_freq
distr$rel_diff_rel_freq=round((distr$rel_obs_freq-distr$rel_pred_freq)/distr$rel_obs_freq,3)

write.xlsx(x=distr, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution - LL", row.names = FALSE, append = TRUE) 


# Separately by Population
all_ll_A = all_ll %>% filter(POP_ABC_2021=="A")
length(unique(all_ll_A$CODICE_INDIVIDUO))
nrow(all_ll_A)
all_ll_B = all_ll %>% filter(POP_ABC_2021=="B")
length(unique(all_ll_B$CODICE_INDIVIDUO))
nrow(all_ll_B)
all_ll_C = all_ll %>% filter(POP_ABC_2021=="C")
length(unique(all_ll_C$CODICE_INDIVIDUO))
nrow(all_ll_C)
# check
nrow(all_ll_A) + nrow(all_ll_B) + nrow(all_ll_C)
nrow(all_ll)

# 
distr_A=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_A$abs_pred_freq=unname(table(all_ll_A$TITSTU_CDIFF_22, useNA="ifany"))
distr_A$abs_obs_freq=unname(table(data_train_A$TITSTU_CDIFF_22, useNA="ifany"))
distr_A$rel_pred_freq=unname(round(prop.table(table(all_ll_A$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_A$rel_obs_freq=unname(round(prop.table(table(data_train_A$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_A$diff_rel_freq=distr_A$rel_obs_freq-distr_A$rel_pred_freq
distr_A$rel_diff_rel_freq=round((distr_A$rel_obs_freq-distr_A$rel_pred_freq)/distr_A$rel_obs_freq,3)

write.xlsx(x=distr_A, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-LL-Pop_A", row.names = FALSE, append = TRUE) 
#
distr_B=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_B$abs_pred_freq=unname(table(all_ll_B$TITSTU_CDIFF_22, useNA="ifany"))
distr_B$abs_obs_freq=unname(table(data_train_B$TITSTU_CDIFF_22, useNA="ifany"))
distr_B$rel_pred_freq=unname(round(prop.table(table(all_ll_B$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_B$rel_obs_freq=unname(round(prop.table(table(data_train_B$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_B$diff_rel_freq=distr_B$rel_obs_freq-distr_B$rel_pred_freq
distr_B$rel_diff_rel_freq=round((distr_B$rel_obs_freq-distr_B$rel_pred_freq)/distr_B$rel_obs_freq,3)

write.xlsx(x=distr_B, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-LL-Pop_B", row.names = FALSE, append = TRUE) 
# 
distr_C=data.frame(TITSTU_CDIFF_22=c("1", "2", "3", "4", "5", "6", "7", "8"), abs_obs_freq=NA, abs_pred_freq=NA, rel_obs_freq=NA, rel_pred_freq=NA, diff_rel_freq=NA, rel_diff_rel_freq=NA)

distr_C$abs_pred_freq=unname(table(all_ll_C$TITSTU_CDIFF_22, useNA="ifany"))
distr_C$abs_obs_freq=unname(table(data_train_C$TITSTU_CDIFF_22, useNA="ifany"))
distr_C$rel_pred_freq=unname(round(prop.table(table(all_ll_C$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_C$rel_obs_freq=unname(round(prop.table(table(data_train_C$TITSTU_CDIFF_22, useNA="ifany")),3))
distr_C$diff_rel_freq=distr_C$rel_obs_freq-distr_C$rel_pred_freq
distr_C$rel_diff_rel_freq=round((distr_C$rel_obs_freq-distr_C$rel_pred_freq)/distr_C$rel_obs_freq,3)

write.xlsx(x=distr_C, file="Output/01_Metriche_rf_sperim3.xlsx", sheetName = "Distribution-LL-Pop_C", row.names = FALSE, append = TRUE) 


# Plots----------------
library(ggplot2)

# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_ll$TITSTU_CDIFF_22=as.factor(all_ll$TITSTU_CDIFF_22)
ggplot(all_ll, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_LL.png",device="png")



# side by side
plot1 <- ggplot(all_ll, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_LL.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distributions
toplot_ll=data.frame(TITSTU_CDIFF_22=c(data_train$TITSTU_CDIFF_22, all_ll$TITSTU_CDIFF_22))
toplot_ll$type=c(rep("Obs in MS_exp",length(data_train$TITSTU_CDIFF_22)), rep("Pred LL in Pop",length(all_ll$TITSTU_CDIFF_22)))
toplot_ll$type=as.factor(toplot_ll$type)

# overall percentage distribution comparison
toplot_ll = toplot_ll %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_ll=as.data.frame(toplot_ll)
toplot_ll = toplot_ll %>% distinct()
# check 
toplot_ll %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_ll, aes(x = TITSTU_CDIFF_22, y = prop
                   , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL.png",device="png", width = 12, height = 8, units="in")



# Separately by Population

# POP_A
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_ll_A$TITSTU_CDIFF_22=as.factor(all_ll_A$TITSTU_CDIFF_22)
ggplot(all_ll_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_LL_Pop_A.png",device="png")


# side by side
plot1 <- ggplot(all_ll_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_A, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_LL_Pop_A.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distributions
toplot_A=data.frame(TITSTU_CDIFF_22=c(data_train_A$TITSTU_CDIFF_22, all_ll_A$TITSTU_CDIFF_22))
toplot_A$type=c(rep("Obs in MS_exp in Pop_A",length(data_train_A$TITSTU_CDIFF_22)), rep("Pred LL in Pop_A",length(all_ll_A$TITSTU_CDIFF_22)))
toplot_A$type=as.factor(toplot_A$type)

# overall percentage distribution comparison
toplot_A = toplot_A %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_A=as.data.frame(toplot_A)
toplot_A = toplot_A %>% distinct()
# check 
toplot_A %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_A, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_Pop_A.png",device="png", width = 12, height = 8, units="in")



# POP_B
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_ll_B$TITSTU_CDIFF_22=as.factor(all_ll_B$TITSTU_CDIFF_22)
ggplot(all_ll_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_LL_Pop_B.png",device="png")

# side by side
plot1 <- ggplot(all_ll_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_B, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_LL_Pop_B.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distributions
toplot_B=data.frame(TITSTU_CDIFF_22=c(data_train_B$TITSTU_CDIFF_22, all_ll_B$TITSTU_CDIFF_22))
toplot_B$type=c(rep("Obs in MS_exp in Pop_B",length(data_train_B$TITSTU_CDIFF_22)), rep("Pred LL in Pop_B",length(all_ll_B$TITSTU_CDIFF_22)))
toplot_B$type=as.factor(toplot_B$type)

# overall percentage distribution comparison
toplot_B = toplot_B %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_B=as.data.frame(toplot_B)
toplot_B = toplot_B %>% distinct()
# check 
toplot_B %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_B, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_Pop_B.png",device="png", width = 12, height = 8, units="in")


# POP_C
# obtained distribution (predicted ALE for non sampled units and observed for sampled units)
# predicted
all_ll_C$TITSTU_CDIFF_22=as.factor(all_ll_C$TITSTU_CDIFF_22)
ggplot(all_ll_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
ggsave("Output/sperim3_predicted_LL_Pop_C.png",device="png")


# side by side
plot1 <- ggplot(all_ll_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ggtitle("predicted")+scale_x_discrete(limits=factor(1:8), labels = 1:8)

# 
plot2 <- ggplot(data_train_C, aes(x=TITSTU_CDIFF_22))+
  geom_bar(stat="count", width=0.7, fill="red")+
  theme_minimal()+ggtitle("observed")+scale_x_discrete(limits=factor(1:8), labels = 1:8)
library(gridExtra)
png(filename="Output/sperim3_observedVSpredicted_LL_Pop_C.png", width     = 5.25,
    height    = 2.25,
    units     = "in",
    res       = 1200)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# % distributions
toplot_C=data.frame(TITSTU_CDIFF_22=c(data_train_C$TITSTU_CDIFF_22, all_ll_C$TITSTU_CDIFF_22))
toplot_C$type=c(rep("Obs in MS_exp in Pop_C",length(data_train_C$TITSTU_CDIFF_22)), rep("Pred LL in Pop_C",length(all_ll_C$TITSTU_CDIFF_22)))
toplot_C$type=as.factor(toplot_C$type)

# overall percentage distribution comparison
toplot_C = toplot_C %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_C=as.data.frame(toplot_C)
toplot_C = toplot_C %>% distinct()
# check 
toplot_C %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_C, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_Pop_C.png",device="png", width = 12, height = 8, units="in")


# % distributions: benchmark (expanded MS) , official procedute (log linear), proposed procedure (Random Forest)
toplot=data.frame(TITSTU_CDIFF_22=c(data_train$TITSTU_CDIFF_22, all_ll$TITSTU_CDIFF_22, all$TITSTU_CDIFF_22))
toplot$type=c(rep("Obs in MS_exp",length(data_train$TITSTU_CDIFF_22)), rep("Pred LL",length(all_ll$TITSTU_CDIFF_22)), rep("Pred RF",length(all$TITSTU_CDIFF_22)))
toplot$type=as.factor(toplot$type)

# overall percentage distribution comparison
toplot = toplot %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot=as.data.frame(toplot)
toplot = toplot %>% distinct()
# check 
toplot %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot, aes(x = TITSTU_CDIFF_22, y = prop
                      , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2, size=2.5
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_RF_LL.png",device="png", width = 12, height = 8, units="in")

# by population
# POP_A
toplot_A=data.frame(TITSTU_CDIFF_22=c(data_train_A$TITSTU_CDIFF_22, all_ll_A$TITSTU_CDIFF_22, all_A$TITSTU_CDIFF_22))
toplot_A$type=c(rep("Obs in MS_exp in Pop_A",length(data_train_A$TITSTU_CDIFF_22)), rep("Pred LL",length(all_ll_A$TITSTU_CDIFF_22)), rep("Pred RF",length(all_A$TITSTU_CDIFF_22)))
toplot_A$type=as.factor(toplot_A$type)

# overall percentage distribution comparison
toplot_A = toplot_A %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_A=as.data.frame(toplot_A)
toplot_A = toplot_A %>% distinct()
# check 
toplot_A %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_A, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2,  size=2.5
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_RF_Pop_A.png",device="png", width = 12, height = 8, units="in")

# POP_B
toplot_B=data.frame(TITSTU_CDIFF_22=c(data_train_B$TITSTU_CDIFF_22, all_ll_B$TITSTU_CDIFF_22, all_B$TITSTU_CDIFF_22))
toplot_B$type=c(rep("Obs in MS_exp in Pop_B",length(data_train_B$TITSTU_CDIFF_22)), rep("Pred LL",length(all_ll_B$TITSTU_CDIFF_22)), rep("Pred RF",length(all_B$TITSTU_CDIFF_22)))
toplot_B$type=as.factor(toplot_B$type)

# overall percentage distribution comparison
toplot_B = toplot_B %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_B=as.data.frame(toplot_B)
toplot_B = toplot_B %>% distinct()
# check 
toplot_B %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_B, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2,  size=2.5
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_RF_Pop_B.png",device="png", width = 12, height = 8, units="in")

# POP_C
toplot_C=data.frame(TITSTU_CDIFF_22=c(data_train_C$TITSTU_CDIFF_22, all_ll_C$TITSTU_CDIFF_22, all_C$TITSTU_CDIFF_22))
toplot_C$type=c(rep("Obs in MS_exp in Pop_C",length(data_train_C$TITSTU_CDIFF_22)), rep("Pred LL",length(all_ll_C$TITSTU_CDIFF_22)), rep("Pred RF",length(all_C$TITSTU_CDIFF_22)))
toplot_C$type=as.factor(toplot_C$type)

# overall percentage distribution comparison
toplot_C = toplot_C %>% group_by(type) %>% mutate(n_by_type=n()) %>% ungroup() %>% 
  group_by(TITSTU_CDIFF_22, type) %>%
  mutate(n=n(), prop = n() / n_by_type)

toplot_C=as.data.frame(toplot_C)
toplot_C = toplot_C %>% distinct()
# check 
toplot_C %>% group_by(type) %>% summarise(sum(prop))
#plot 
ggplot(toplot_C, aes(x = TITSTU_CDIFF_22, y = prop
                     , fill = type
)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(100 * prop,2)),
            position = position_dodge(.9), vjust = -.2,  size=2.5
  )+scale_x_discrete(limits=factor(1:8), labels = 1:8)+scale_y_continuous(breaks=seq(0, 1,0.1),labels = as.character(seq(0,100, by=10)), "percentage")
ggsave("Output/sperim3_comparison_LL_RF_Pop_C.png",device="png", width = 12, height = 8, units="in")
