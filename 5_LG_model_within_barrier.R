library(PresenceAbsence)
library(DAAG)
library(raster)

# set working directory to Data folder
setwd('./Data')



######## M modesta SDM - Logistic Regression - Within Barrier Models
#### models are trained on reduced data set (escluding presence/absence records in South America and islands)
### this is because the Across Barrier Model suggested that the distribution of M. modesta is strongly limited by barriers


# get training data frame
Mmodesta_trM=read.csv("Mmodesta_tr_clean.csv", header = TRUE)
head(Mmodesta_trM)
str(Mmodesta_trM)

Mmodesta_trM$barrier <- as.factor(Mmodesta_trM$barrier)



## train model on area within reach of M. modesta (exclude area with barrier == 0)
# this is within M (migration) of BAM (Biotic - Abiotic - Migration)
Mmodesta_trM <- Mmodesta_trM[which(Mmodesta_trM$barrier==1),]


length(Mmodesta_trM$barrier[Mmodesta_trM$Mmodesta == 1])
length(Mmodesta_trM$barrier[Mmodesta_trM$Mmodesta == 0])
# we have 101 prensence records and 18 absence records





#### LOGISTIC REGRESSION MODELS ####

#### mod.full => REALIZED NICHE (Climate + competition)
### build logistic regression model from relevant climate variables (biol1 and biol12) and competition (BbBfBrsuit)
# BbBfBrsuit is habitat suitability for the three potential competitors

mod.full=glm(factor(Mmodesta)~bio1*bio12+BbBfBrsuit, family=binomial,data=Mmodesta_trM)
step(mod.full)



#### mod.within (final model) => bio1 + bio12 (competition was removed by stepwise regression)

#### MODEL_step (mod1.LR.step): stepwise regression on model 1
mod.within = glm(factor(Mmodesta)~bio1 + bio12 , family=binomial,data=Mmodesta_trM)
mod.within.pred=predict(mod.within,type="response")
summary(mod.within)
anova(mod.within, test="LRT")
#####






##### EXAMINE MODEL FIT (confusion matrix, accuracy indices, ROC plot) ####

## build testing dataframe using model predictions
val_within = cbind.data.frame(seq(1:length(Mmodesta_trM$Mmodesta)), Mmodesta_trM$Mmodesta, mod.within.pred) # build dataframe w/mod1 predictions
names(val_within) <- c("ID","obs.value","mod.within")
val_within$obs.value <- as.integer(val_within$obs.value)
head(val_within)                       # examine prediction dataframe

## determine best threshold using PresenceAbsence package
#help(optimal.thresholds)   # options for optimizing threshold
# chose threshold to maximize Kappa
# i.e. the ratio between sensitivity (the proportion of correctly predicted presences) 
# and specificity (the proportion of correctly predicted absences))
mod.cut=optimal.thresholds(val_within, opt.methods=c("MaxKappa"))
mod.cut                    # examine threshold


## generate confusion matrix
cfmat=table(val_within[[2]], factor(as.numeric(val_within$mod.within >= mod.cut$mod.within)))
cfmat       # examine


## calculate model accuracies (using presence.absence.accuracy function in PresenceAbsence package)
mod.acc = presence.absence.accuracy(val_within, threshold=mod.cut$mod.within, st.dev=FALSE)
tss=mod.acc$sensitivity+mod.acc$specificity-1 # code TSS metric 
# true skill statistic (TSS) corrects for this dependence while still keeping all the advantages of kappa
mod.acc=cbind(mod.acc,tss)               # bind all metrics
mod.acc                         # examine accuracies


## plotting AUC
par(mfrow=c(1,1))
auc.roc.plot(val_within,color=T) # basic AUC plot; pkg PresenceAbsence
#####




##### MODEL VALIDATION (K-fold cross validation -> compare to original model using CM, ACCs and ROC plot) #####
## requires pkg DAAG

### K-fold cross-validation (K = 10)
mod.within.cv=CVbinary(mod.within, nfolds=10, print.details=FALSE) # crossval w/10 folds
#mod.within.cv$acc.cv
mod.within.cv = mod.within.cv$cvhat                                   
val_within=cbind(val_within, mod.within.cv)
head(val_within)



## generate confusion matrix using model cutpoint to maximize kappa from above
cfmatcv10=table(val_within[[2]],factor(as.numeric(val_within$mod.within.cv >= mod.cut$mod.within)))
cfmat; cfmatcv10   # examine & compare cfmats



## calculate model accuracies
mod.accB = presence.absence.accuracy(val_within, threshold = mod.cut$mod.within, st.dev=F)
tss = mod.accB$sensitivity + mod.accB$specificity - 1 # code TSS metric
mod.accB = cbind(mod.accB, tss) # bind all metrics
mod.accB            # examine accuracies



## plotting AUC
names(val_within)
# model 2
auc.roc.plot(val_within, color=T, main="ROC Plot - Within Barrier Model") # basic AUC plot; pkg PresenceAbsence

#####






#### GENERATE GIS LAYERS FROM MODEL PREDICTIONS (FOR FIGURES)


### 1. WITHIN BARRIER - REALIZED GEOGRAPHIC DISTRIBUTION ####

## get predictors and crease raster stack
setwd('./Predictor_layers/within_barrier')
## build stack of raster layers
grd.list=list.files(pattern=".grd") # list of pred files
grd.list # examine

layers={} # initialize layers vector
for (i in 1:length(grd.list)) {
  assign(paste("ras",i,sep=""),raster(grd.list[i])) # import predictor
  layers=c(layers,get(paste("ras",i,sep=""))) # add to layer
}

setwd('../..')
getwd()

# stack layers
predsM=stack(layers); predsM 



## predict realized spatial distribution (GIS layer)
mod_realized = predict(predsM, mod.within, type="response", fun=predict, index=2, overwrite=T)


## save spatial model predictions of realized geographic distribution
#writeRaster(mod_realized, "predictors_within.grd", bandorder='BIL', bylayer=TRUE, overwrite=TRUE)

#####



### 2. ACROSS BARRIER - POTENTIAL GEOGRAPHIC DISTRIBUTION  ####

## get predictors and crease raster stack


### get predictors for potential distribution (ACROSS BARRIER)
setwd('./Predictor_layers/across_barrier')
## build stack of raster layers
grd.list=list.files(pattern=".grd") # list of pred files
grd.list # examine

layers={} # initialize layers vector
for (i in 1:length(grd.list)) {
  assign(paste("ras",i,sep=""),raster(grd.list[i])) # import predictor
  layers=c(layers,get(paste("ras",i,sep=""))) # add to layer
}

setwd('../..')
getwd()

preds=stack(layers); preds # stack layers
names(preds)



## predict potential spatial distribution (GIS layer)
mod_potential=predict(preds, mod.within, type="response",fun=predict,index=2,overwrite=T)



## save spatial model predictions of realized geographic distribution
#writeRaster(mod_potential, "predictors_across.grd", bandorder='BIL', bylayer=TRUE, overwrite=TRUE)

#####



