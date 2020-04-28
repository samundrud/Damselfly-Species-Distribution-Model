library(raster)
library(rgeos)
library(maptools)  


# set working directory to Data folder
setwd('./Data')



######## M. modesta SDM - Figures




## IMPORT FILES

# species records
M.modesta=read.csv("Mmodesta_tr_clean.csv", header = TRUE)
B.beebeanum=read.csv("Bromeliagrion_beebeanum.csv", header = TRUE)
B.rehni=read.csv("Bromeliagrion_rehni.csv", header = TRUE)
B.fernandezianum=read.csv("Bromeliagron_fernandezianum.csv", header = TRUE)

# GIS layers from model predictions
predictors_across=raster("predictors_across.grd")
predictors_within=raster("predictors_within.grd")



# get country outlines for Latin America from maptools library
data(wrld_simpl)
SPDF <- subset(wrld_simpl, LON=c(-115, -35), LAT = c(-45, 25))
# save extent as e
e <- extent(-115, -35, -45, 25)
countries <- crop(SPDF, e)
plot(countries, col="grey95")





##### Fig. 1: Map showing M. modesta and competitor occurrences #####

#tiff("Fig.1.tiff", width=180, height=150, units="mm", res=500)

par(mfrow=c(1,1))
par(mar=c(4.2, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right) 

plot(extent(-105, -50, -20, 25), xaxs = "i", yaxs = "i", xlab = expression(Longitude~(degree)),
     ylab = expression(Latitude~(degree)))
plot(countries, add=TRUE, border="darkgray", col="grey95")
plot(extent(-105, -50, -20, 25), xaxs = "i", yaxs = "i", add=T)
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 1)], M.modesta$Latitude[which(M.modesta$Mmodesta == 1)],
       pch=21,bg="green", cex=1) # add points to plot
points(B.beebeanum$Longitude, B.beebeanum$Latitude, pch=21, bg="blue", cex=1)
points(B.rehni$Longitude, B.rehni$Latitude, pch=21, bg="purple", cex=1)
points(B.fernandezianum$Longitude, B.fernandezianum$Latitude, pch=21, bg="orange", cex=1)
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 0)], M.modesta$Latitude[which(M.modesta$Mmodesta == 0)],
       pch=13,col="black", cex=1) # add points to plot

#dev.off()
#####




#### Fig. 2: plot relationship of M. modesta occurrence with climate variables #######


#tiff("Fig.2.tiff", width=180, height=120, units="mm", res=500)

par(mfrow=c(1,2))
par(mar=c(4.2, 4, 0.1, 0) + 0.1)#it goes c(bottom, left, top, right) 

# use subset of data where barrier == 1 (within area of dispersal)
M.modesta_within = M.modesta[M.modesta$barrier == 1,]

# plot responses to temp
real_temp <- M.modesta_within$bio1/10
plot(M.modesta_within$Mmodesta ~ real_temp, 
     ylab= expression(Presence~of~italic(M.~modesta)),
     xlab=expression(Annual~mean~temperature~(degree~C)),
     las=1)
temp = glm(factor(Mmodesta)~ real_temp, family=binomial, data = M.modesta_within) 
xweight <- seq(min(real_temp), max(real_temp), 0.01)
yweight <- predict(temp, list(real_temp = xweight),type="response", se.fit=TRUE)
lines(xweight, yweight$fit, lwd=2)
lines(xweight, yweight$fit + 1.96*yweight$se.fit, lty=2)
lines(xweight, yweight$fit - 1.96*yweight$se.fit, lty=2)


# plot response to precip
plot(M.modesta_within$Mmodesta ~ M.modesta_within$bio12, ylab="", xlab="Annual precipitation (mm)",
     las=1)
prec = glm(factor(Mmodesta)~ bio12, family=binomial, data = M.modesta_within) 
xweight <- seq(min(M.modesta_within$bio12), max(M.modesta_within$bio12), 0.01)
yweight <- predict(prec, list(bio12 = xweight),type="response", se.fit=TRUE)
lines(xweight, yweight$fit, lwd=2)
lines(xweight, yweight$fit + 1.96*yweight$se.fit, lty=2)
lines(xweight, yweight$fit - 1.96*yweight$se.fit, lty=2)

#dev.off()
#####






#### Fig. 3: realized geographic distribution of M. modesta #####

#tiff("Fig.3.tiff", width=180, height=160, units="mm", res=500)

par(mfrow=c(1,1))
par(mar=c(4.2, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right) 

plot(predictors_within, 
     xlab = expression(Longitude~(degree)),
     ylab = expression(Latitude~(degree)),
     xlim = c(-110, -65),
     ylim = c(-20, 25),
     #col=rev(heat.colors(255)),
     col=rev(terrain.colors(255)),
     legend.width=1, legend.shrink=0.5,
     legend.args=list(text='Probability of occurrence', side=4, font=1, line=2.5, cex=1)
)
# colors: rainbow, heat.colors, topo.colors, bpy.colors
plot(countries, add=TRUE, border="darkgray")

# add data points if desired
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 1)], M.modesta$Latitude[which(M.modesta$Mmodesta == 1)],
       pch=21,bg="green", cex=1) # add points to plot
points(B.beebeanum$Longitude, B.beebeanum$Latitude, pch=21, bg="blue", cex=1)
points(B.rehni$Longitude, B.rehni$Latitude, pch=21, bg="purple", cex=1)
points(B.fernandezianum$Longitude, B.fernandezianum$Latitude, pch=21, bg="orange", cex=1)
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 0)], M.modesta$Latitude[which(M.modesta$Mmodesta == 0)],
       pch=13,col="black", cex=1) # add points to plot

#dev.off()
#####




#### Fig. 4 - potential geographic distribution of M. modesta #####

#tiff("Fig.4.tiff", width=180, height=160, units="mm", res=500)
plot(predictors_across, 
     xlab = expression(Longitude~(degree)),
     ylab = expression(Latitude~(degree)),
     #xlim = c(-110, -65),
     ylim = c(-45, 25),
     #col=rev(heat.colors(255)),
     col=rev(terrain.colors(255)),
     legend.width=1, legend.shrink=0.5,
     legend.args=list(text='Probability of occurrence', side=4, font=1, line=2.5, cex=1)
)
# colors: rainbow, heat.colors, topo.colors, bpy.colors
plot(countries, add=TRUE, border="darkgray")

# add data points if desired
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 1)], M.modesta$Latitude[which(M.modesta$Mmodesta == 1)],
       pch=21,bg="green", cex=1) # add points to plot
points(B.beebeanum$Longitude, B.beebeanum$Latitude, pch=21, bg="blue", cex=1)
points(B.rehni$Longitude, B.rehni$Latitude, pch=21, bg="purple", cex=1)
points(B.fernandezianum$Longitude, B.fernandezianum$Latitude, pch=21, bg="orange", cex=1)
points(M.modesta$Longitude[which(M.modesta$Mmodesta == 0)], M.modesta$Latitude[which(M.modesta$Mmodesta == 0)],
       pch=13,col="black", cex=1) # add points to plot

#dev.off()
#######


