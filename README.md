# Damselfly-Species-Distribution-Model
Species distribution model to determine effects of climate, dispersal barriers, and competition on damselfly geographic distribution


published in Amundrud et al. 2018. Dispersal barriers and climate determine the geographic
distribution of the helicopter damselfly Mecistogaster modesta. Frishwater Biology 63: 214-223


This repository contains the R scripts and data for a species distribution model of M. modesta


Data:
1. Mmodesta_records.csv
presence and absence redords of M. modesta, as well as variables for barrier (1 = within hypothesized area of dispersal; 2 = outside of hypothesized are of dispersal) and competitor species (Bb...: value indicates probabily of species occurrence estimated from MaxEnt Models)
 
2. Bromeliagrion_beebeanum.csv
B. beebeanum (potential competitor species) occurrence records

3. Bromeliagrion_rehni.csv
B. rehni (potential competitor species) occurrence records

4. Bromeliagron_fernandezianum.csv
B fernandezianum (potential competitor species) occurrence records

5. Barrier_layers
folder containing the GIS layers for the barrier variable

6. BioClim
folder containing the GIS layers for the bioclim variables (downloaded from WorldClim)
NOTE: data needs to be downloaded from WorldClim and saved in this folder

7. Competitor_layers
folder containing the GIS layers for the geographic ranges of the potential competitor species (estimated from MaxEnt models)


The following data files are generated using the R scripts:
8. Mmodesta_tr.csv
extension of Mmodesta_records.csv that also contains the bioclimatic predictor variables from WorldClim

9. Mmodesta_tr_clean.csv
cleaned data set containing M. modesta records and predictor variables used to model M. modesta distribution

10. predictors_across.grd / predictors_across.gri
raster layer of modeled potential M. modesta distribution (includes suitable across barrier habitat)

11. predictors_within.grd / predictors_within.gri
raster layer of modeled realized M. modesta distribution (does not include suitable across barrier habitat)

12. Predictor_layers
folder that contains GIS layers for spatial model projections of M. modesta distribution



R scripts:

1_generate_raster_layers.R
generate stack of rasters from bioclimatic variables, barrier variable, and competitor variables; saved folders inside "Predictor_layers" folder

2_climate_predictor_extraction.R
extract potential climatic predictors from BioClim for relevant geographic coordinates

3_variable_selection.R
two-step process to select relevant climatic predictor variables

4_LG_model_across_barrier.R
logistic regression to determine whether species is limited by dispersal barriers

5_LG_model_within_barrier.R
logistic regression to explain factors that limit species distributions, and make spatial predictions across study area

6_Figures.R
figures and maps of realized and potential species distribution 


