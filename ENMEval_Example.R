#one option to help with model selection is ENMEval
#install.packages('ENMeval') if you don't have it
library(ENMeval)

#you will run this once all of the other scripts are done and you are ready to run Maxent

#here, I did not reproject the environmental layers, and I used the points file prior to reprojecting (although I kept the same name as the name in the maxent_models.R script)

#convert points from spatialdatapoints to df and make data frame only the lat and lon. going to do everything here in lat and lon.
#do this after running lines 69/70 from the maxent_models.R file, and this occurrs right before actually projecting
points_enm_eval <- as.data.frame(points_repro)
points_enm_eval$X <- NULL

#this will calculate the best model based on AIC after doing a string of evaluation steps, so you know what to select for features and regularization multiplier. Note that this can be very memory intensive, because it is going to build every Maxent model possible. try lower resolutions if needed (e.g. 2.5) because it is going to calculate EVERY Maxent model combination.

#add argument "categoricals=c("layer name")" when necessary, must occur prior to the algorithm argument.

enmeval_results <- ENMevaluate(points_enm_eval, predictors_maxent, n.bg=10000, method='checkerboard1', RMvalues=c(0.5, 1, 2), algorithm='maxent.jar')

#pull out the best model
#this shows best regularization and fitting types
enmeval_results@results[which(enmeval_results@results$delta.AICc==0),]

#we can then create an object which will open the html file but then also list the variables
aic.opt <- enmeval_results@models[[which(enmeval_results@results$delta.AICc==0)]]
aic.opt

#now you know which variables to use, which features, and what regularization multiplier

#IDFG protocol is also to drop and rerun if permutation importance (others use percent contribution) is less than 2%. we can also check this by doing:
var.importance(aic.opt)

#so you might want to drop other variables
