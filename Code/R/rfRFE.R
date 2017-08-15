library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');
source('randomforestCV.R');

timestamp();

set.seed(10);

balancing = "_SMOTED";
fScheme   = "_comb_pseAAC";

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
featureFile        = paste("featurized"    , fileNameSuffix, sep = "");
testFeatureFile    = paste("testFeaturized", fScheme,".rds", sep = "");
svmFile            = paste("svm"           , fileNameSuffix, sep = "");
rfmodelFile        = paste("rfmodel"       , fileNameSuffix, sep = "");

outFile            = paste("out", fScheme, balancing, ".csv", sep = "");

cat(as.character(Sys.time()),">> Reading training set features from", featureFile, "...\n");
features = readRDS(featureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;
prevModel = NULL;

featureCountList = seq(from=4000, to=1000, by=-500);

cat(as.character(Sys.time()),">> Entering random forest RFE ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features     = featurefiltering(features, rankedFeatures, max(featureCountList));

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  perf = randomForestCV(protection ~., trainingSet, TRUE, 10);
  model = perf$model;
  rankedFeatures = rownames(model$importance[order(-model$importance[,3]),]);
  
  cat(maxFeatureCount, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
  accData = rbind(accData, c(maxFeatureCount, perf$acc, perf$sens, perf$spec, perf$mcc));
  write.csv(accData, outFile);
  
  if (!is.nan(perf$mcc)) {
    if (is.null(bestPerf) || bestPerf$mcc <= perf$mcc) {
      if (!is.null(bestPerf)) {
        prevModel = bestPerf$model;
      }
      bestPerf = perf;
      cat(",<-- BEST");
    }
  }
    
  cat("\n");
}

cat("Best Result:\n");
cat("Accuracy(Test set): ", bestPerf$acc, "\n");
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n");
cat("MCC(Test set): ", bestPerf$mcc, "\n");

saveRDS(bestPerf$model, "bestModel.rds");
saveRDS(prevModel, "prevModel.rds");
