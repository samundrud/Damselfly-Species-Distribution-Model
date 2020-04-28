library(PresenceAbsence)
library(DAAG)

# set working directory to Data folder
setwd('./Data')




######## M modesta SDM - Logistic Regression - Across Barrier Models
#### models are trained on full data set (including presence/absence records in South America and islands)


# get training data frame
Mmodesta_tr=read.csv("Mmodesta_tr_clean.csv", header = TRUE)
head(Mmodesta_tr)
str(Mmodesta_tr)

Mmodesta_tr$barrier <- as.factor(Mmodesta_tr$barrier)


# examine presences and absences
length(Mmodesta_tr$barrier[Mmodesta_tr$Mmodesta == 1])
length(Mmodesta_tr$barrier[Mmodesta_tr$Mmodesta == 0])
# we have 101 prensence records and 52 absence records






##### LOGISTIC REGRESSION MODELS ####
## note: do seperate model for each potential competitor species, as well as all combined
## use stepwise regression to see what variables are important

##Bb: Bromeliagrion beebeanum (competitor 1)
mod.Bb=glm(factor(Mmodesta)~bio1*bio12+barrier+Bbsuit, family=binomial,data=Mmodesta_tr) 
step(mod.Bb)

##Bf: Bromeliagron fernandezianum (competitor 2)
mod.Bf=glm(factor(Mmodesta)~bio1*bio12+barrier+Bfsuit, family=binomial,data=Mmodesta_tr) 
step(mod.Bf)

##Br: Bromeliagron rehni (competitor 3)
mod.Br=glm(factor(Mmodesta)~bio1*bio12+barrier+Brsuit, family=binomial,data=Mmodesta_tr) 
step(mod.Br)


##BbBfBr: Bromeliagron ALL (all competitors combined)
mod.BbBfBr=glm(factor(Mmodesta)~bio1*bio12+barrier+BbBfBrsuit, family=binomial,data=Mmodesta_tr)
step(mod.BbBfBr)


## competitors and bio1:bio12 interactions were consistently removed by stepwise regression in all models
### Final full model: bio1+bio12 + barrier



######## Final MODEL => Climate + barrier
### build model with climate variables (biol1 and biol12) plus barrier (competition was removed by stepwise regression)
mod.across=glm(factor(Mmodesta)~ bio1+bio12+barrier, family=binomial,data=Mmodesta_tr) 
mod.across.pred=predict(mod.across,type="response") # model prediction
summary(mod.across)        #  model summary stats
anova(mod.across, test="LRT")

#####






##### EXAMINE MODEL FIT (confusion matrix, accuracy indices, ROC plot) ####

## build testing dataframe using model predictions
val_across=cbind.data.frame(seq(1:length(Mmodesta_tr$Mmodesta)),Mmodesta_tr$Mmodesta, mod.across.pred) # build dataframe w/mod1 predictions
names(val_across) <- c("ID","obs.value","mod.across")
val_across$obs.value <- as.integer(val_across$obs.value)
head(val_across)                       # examine prediction dataframe

## determine best threshold using PresenceAbsence package
#help(optimal.thresholds)   # options for optimizing threshold
# chose threshold to maximize Kappa
# i.e. the ratio between sensitivity (the proportion of correctly predicted presences) 
# and specificity (the proportion of correctly predicted absences))
mod.cut=optimal.thresholds(val_across,opt.methods=c("MaxKappa"))
mod.cut                    # examine threshold


## generate confusion matrix
#model 2
cfmat=table(val_across[[2]],factor(as.numeric(val_across$mod.across >= mod.cut$mod.across)))
cfmat       # examine


## calculate model accuracies with standard deviation=F
# all models
mod.acc=presence.absence.accuracy(val_across,threshold=mod.cut$mod.across,st.dev=F)
tss=mod.acc$sensitivity+mod.acc$specificity-1 # code TSS metric 
# true skill statistic (TSS) corrects for this dependence while still keeping all the advantages of kappa
mod.acc=cbind(mod.acc,tss)               # bind all metrics
mod.acc                         # examine accuracies


## plotting AUC
par(mfrow=c(1,1))
auc.roc.plot(val_across,color=T) # basic AUC plot; pkg PresenceAbsence
#####





##### MODEL VALIDATION (K-fold cross validation -> compare to original model using CM, ACCs and ROC plot) #####
## requires pkg DAAG

## 10-fold cross-validation
mod.across.cv10=CVbinary(mod.across,nfolds=10,print.details=F) # crossval w/10 folds
mod.across.cv10$acc.cv
mod.across.cv10=mod.across.cv10$cvhat                                   
val_across=cbind(val_across, mod.across.cv10)
head(val_across)




##   generate confusion matrix and cv10
##   using model cutpoint to maximize kappa from above

cfmatcv10=table(val_across[[2]],factor(as.numeric(val_across$mod.across.cv10 >= mod.cut$mod.across)))
cfmat; cfmatcv10   # examine & compare cfmats




## calculate model accuracies
mod.accB=presence.absence.accuracy(val_across, threshold = mod.cut$mod.across, st.dev=F)
tss=mod.accB$sensitivity+mod.accB$specificity-1 # code TSS metric
mod.accB=cbind(mod.accB,tss) # bind all metrics
mod.accB            # examine accuracies

## plotting AUC
names(val_across)
# model 2
auc.roc.plot(val_across, color=T, main="ROC Plot - Across Barrier Model") # basic AUC plot; pkg PresenceAbsence

#####


