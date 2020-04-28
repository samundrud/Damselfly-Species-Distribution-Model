library(raster) # manipulate geographic (spatial) data in 'raster' (i.e., grid) format

# set working directory to Data folder
setwd('./Data')




##### EXTRACT BIOCLIMATIC PREIDCTORS FROM BIOCLIM AND ADD TO TRAINING DATA



## import M. modesta data
damsel=read.csv("Mmodesta_records.csv", header = TRUE)
head(damsel)




### import climatic variables from BioClim and add extact data points based on geographic coordinates of M. modesta records
setwd('./BioClim/bil,hdr')
predictors <- list.files(pattern=".bil") # these are the bioclim variables downloaded from WorldClim (image files)

# create empty dataframe to store climatic predictors
preds <- data.frame(rep(NA, length(damsel$Longitude))) 

# fill "preds" with bioclimatic predictor variables (based on geographic coordinates)
for (i in 1:length(predictors) ) {
  # convert each predictor to raster format
  var=raster(predictors[i])
  # extract predictor value based on geographic coordinates
  preds[i] = extract(var,damsel[,c("Longitude","Latitude")])
  names(preds)[i] <- names(var)
}
# can ignore warning message (unsigned assumes all values are positive, which is correct)

# check
head(preds)


# set working directory back to Data (go back two parent folders)
setwd('../..')
getwd()




### bind to training dataframe
damsel.tr=cbind(damsel,preds)
head(damsel.tr) 

# save training data frame
#write.csv(damsel.tr, file = "Mmodesta_tr.csv", row.names=FALSE)
