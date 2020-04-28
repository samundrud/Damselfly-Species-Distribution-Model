library(raster) # manipulate geographic (spatial) data in 'raster' (i.e., grid) format
library(rgdal)


# set working directory to Data folder
setwd('./Data')





### CREATING RASTER LAYERS (STACKS) FOR SPATIAL PREDICTIONS OF MODEL
## STACK INCLUDES ALL BIOCLIMATIC VARIABLES (BIOCLIM), DISPERSAL BARRIER, AND COMPETITOR LAYERS
  



## 1. DEFINE STUDY AREA AS ACROSS BARRIER (INCLUDES cENTRAL AND sOUTH AMERICA) - POTENTIAL GEOGRAPHIC NICHE 

#### import BioClim layers and create raster stack for study area ####

setwd('./BioClim/bil,hdr')

## build stack of raster layers
bil.list=list.files(pattern=".bil") # list of pred files
bil.list # examine

## convert to raster and add layers
layers={} # initialize layers vector
for (i in 1:length(bil.list)) {
  assign(paste("ras",i,sep=""),raster(bil.list[i])) # import predictor
  layers=c(layers,get(paste("ras",i,sep=""))) # add to layer
}

# stack layers
clim=stack(layers); clim


# change the extend to study area
e <- extent(-115, -35, -45, 40)
clim2 <- crop(clim, e)

# set working directory back to Data (go back two parent folders)
setwd('../..')
getwd()

#####



#### import physical barrier layer and create raster stack for study area ####

setwd('./Barrier_layers')
disp=raster("barrier2.grd")

# change extent
disp2 <- crop(disp, e)
names(disp2) <- "barrier"

# set working directory back to Data
setwd('..')
getwd()

#####



#### import competitor layers and create raster stack for study area ####

setwd('./Competitor_layers')

## build stack of raster layers
grd.list=list.files(pattern=".grd") # list of pred files
grd.list # examine

layers={} # initialize layers vector
for (i in 1:length(grd.list)) {
  assign(paste("ras",i,sep=""),raster(grd.list[i])) # import predictor
  layers=c(layers,get(paste("ras",i,sep=""))) # add to layer
}

comp=stack(layers); comp # stack layers


# change extent
comp.2 <- crop(comp, e)
names(comp.2)


# set working directory back to Data
setwd('..')
getwd()

#####




## stack all predictors
preds <- stack(clim2, disp2, comp.2)
names(preds)


## save raster stack
setwd('./Predictor_layers/across_barrier')
writeRaster(preds, "predictors_across.grd", bandorder='BIL', bylayer=TRUE, overwrite=TRUE)
setwd('../..')
getwd()







## 2. DEFINE STUDY AREA AS WITHIN BARRIER (NORTH-WEST OF ANDES AND NO ISLANDS) - REALIZED GEOGRAPHIC NICHE



## create spatial polygon that defines arera within dispersal ability
M <- rasterToPolygons(disp, fun=function(disp){disp==1} , n=4, na.rm=TRUE, digits=12, dissolve=FALSE); M


## Crop and mask all prdictor layers by M (note: this will take a while to run)
preds2 <- crop(preds, extent(M))
predsM <- mask(preds2, M)



## save raster stack
setwd('./Predictor_layers/within_barrier')
writeRaster(predsM, "predictors_within.grd", bandorder='BIL', bylayer=TRUE, overwrite=TRUE)
setwd('../..')
getwd()






