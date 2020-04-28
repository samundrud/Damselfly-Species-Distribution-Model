library(corrplot) # graphical display of a correlation matrix

# set working directory to Data folder
setwd('./Data')



#### SELECT BIOCLIMATIC PREIDCTOR VARIABLES (EXAMINE VARIABLE IMPORTANCE AND PAIRWISE CORRELATIONS)




# get training data frame
damsel.tr=read.csv("Mmodesta_tr.csv", header = TRUE)
head(damsel.tr)
str(damsel.tr)





### SUBSET DATA TO ONLY INCLUDE AREA OF DISPERSAL (WITHIN BARRIER, i.e. Central America mainland)
## it is not appropriate to train model based on absence data due to dispersal limitation

# check if there are any presence records beyond Andes barrier (where barrier == 0)
damsel.tr[which(damsel.tr$barrier==0),]$Mmodesta # no, we're good

# subset training dataset to where barrier == 1 (includes all locations where damselfly can disperse to)
damsel.tr.M <- damsel.tr[which(damsel.tr$barrier==1),]

# extract potential climatic predictors
predictors <- damsel.tr.M[,21:length(names(damsel.tr.M))]
head(predictors)
vars <- names(predictors) # store predictor names





### SUMMARIZE AND EXAMINE PREDICTORS VISUALLY ####

## means by pres/abs
aggregate(predictors, by=list(damsel.tr.M$Mmodesta), mean, na.rm=T) # frame=all

# min/max
aggregate(predictors,by=list(damsel.tr.M$Mmodesta),min,na.rm=T) # frame=all
aggregate(predictors,by=list(damsel.tr.M$Mmodesta),max,na.rm=T) # frame=all

# range 
aggregate(predictors,by=list(damsel.tr.M$Mmodesta),range,na.rm=T) # frame=all


## plot each predictor vs presence/absence
par(mfrow=c(4,5), mar=c(4,4,1,1)+0.1) #it goes c(bottom, left, top, right) 
for (i in 1:length(predictors)) {
  boxplot(predictors[,i]~damsel.tr.M$Mmodesta,xlab="Absence:Presence",
        ylab=names(predictors)[i],main="")
}

#####





##### TWO-STEP PROCESS TO CHOOSE PREDICTOR VARIABLES


### 1. DETERMINE VARIABLE IMPORTANCE #####
# VARIABLE IMPORTANCE = Deviance (or likelihood ratio statistic) from GLM

# examine relationship between response and each predictor
# modeled as simple GLM (logit link; binomial response) with 'higher deviance explained' indicating "better" predictors to use

deviance=NULL
for (i in 1:length(predictors) ) {
  mod.each <- glm(damsel.tr.M$Mmodesta ~ as.matrix(predictors[i]), family = binomial)
  av.each <- anova(mod.each)
  deviance[i] <- av.each$Deviance[2]
  }
deviance

# save VARIANCE IMPORTANCE and order variables by importance
var.imp <- data.frame(vars)
var.imp$deviance <- deviance
var.imp <- var.imp[order(var.imp$deviance, decreasing = TRUE),]
var.imp
#####




### 2. CORRELATIONS AMONG PREDICTORS #####
# make sure that chosen predictor variables are not strongly correlated

# create data frame for correlation coefficients for pairwise comparisons between variables
correlations <- data.frame(vars)
r=NULL
# get correlation coefficients of all pairwise correlations
for (i in 1:length(predictors) ) {
  # correlation coefficient for each variable
  for (j in 1:length(predictors) ) {
    r[j] = cor(predictors[,i],predictors[,j], method = "pearson")
  }
  correlations[i+1] = r
  names(correlations)[i+1] <- names(predictors)[i]  
}
head(correlations)


# tidy up correlation matrix and convert to matrix
correlations <- correlations[-1] # remove first column (variable names)
row.names(correlations) <- names(correlations)
corr_mat <- data.matrix(correlations, rownames.force = NA)
head(corr_mat)


# remove all highly correlated variables (r > 0.7)
corr_mat2 <- corr_mat
corr_mat2[corr_mat2 > 0.7 | corr_mat2 < -0.7] <- NA

# plot correlation matrix
par(mfrow=c(1,1))
corrplot(corr_mat2, na.label.col = 'white')

# plot original correlation matrix (including r > 0.7)
corrplot(corr_mat, method = "number", number.cex=0.6)
corrplot(corr_mat)

#####



## compare to variable importance and choose predictor variables (also consider expert knowledge)
var.imp

## climatic predictors: bio1 (Annual Mean Temp) and bio 12 (Annual Precipitation)
# based on high importance, low correlation, and expert knowledge of the study species



# final data set with all relevant predictor variables
Mmodesta_tr <- damsel.tr[c(2:5,7,9,11,13,21,24)]
head(Mmodesta_tr)

# save final data frame for analysis
#write.csv(Mmodesta_tr, file = "Mmodesta_tr_clean.csv", row.names=FALSE)
