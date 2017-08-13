library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');

timestamp();

set.seed(10);

balancing = "_SMOTED";
fScheme   = "_comb_pseAAC_special";

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

cat(as.character(Sys.time()),">> Reading test set features from", testFeatureFile, "...\n");
testFeatures = readRDS(testFeatureFile);
cat(as.character(Sys.time()),">> Done\n");

cat(as.character(Sys.time()),">> Reading feature ranking from", rankedFeaturesFile, "...\n");
rankedFeatures = readRDS(rankedFeaturesFile);
cat(as.character(Sys.time()),">> Done\n");

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

featureCountList = seq(from=4000, to=2, by=-10);

cat(as.character(Sys.time()),">> Entering independent validation ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features     = featurefiltering(features, rankedFeatures, max(featureCountList));
testFeatures = featurefiltering(testFeatures, rankedFeatures, max(featureCountList));

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);
  testSet = featurefiltering(testFeatures, rankedFeatures, maxFeatureCount);
  
  trainingSet$protection = as.numeric(trainingSet$protection) - 1;
  testSet$protection = as.numeric(testSet$protection) - 1;
  
  model = randomForest(protection ~ ., trainingSet, importance = TRUE);
  rankedFeatures = rownames(model$importance[order(-model$importance[,2]),]);
  
  pred = predict(model, testSet);
  predAndTruth = prediction(as.numeric(pred), as.numeric(testSet$protection));
  
  mccSeries = ROCR::performance(predAndTruth,"mat");
  threshold = unlist(mccSeries@x.values)[[which.max(unlist(mccSeries@y.values))]];
  
  predAndTruth = prediction(as.numeric(pred >= threshold), testSet$protection);
  
  acc = unlist(ROCR::performance(predAndTruth,"acc")@y.values)[2]
  sensitivity = unlist(ROCR::performance(predAndTruth,"sens")@y.values)[2];
  specificity = unlist(ROCR::performance(predAndTruth,"spec")@y.values)[2];
  mcc = unlist(ROCR::performance(predAndTruth,"mat")@y.values)[2];
  
  perf = list(
    "acc" = acc,
    "sens" = sensitivity,
    "spec" = specificity,
    "mcc" = mcc
  )
  cat(maxFeatureCount, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
  accData = rbind(accData, c(maxFeatureCount, perf$acc, perf$sens, perf$spec, perf$mcc));
  write.csv(accData, outFile);
  
  if (!is.nan(perf$mcc)) {
    if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
      bestPerf = perf;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "featureSet"      = rankedFeatures
      )
      cat(",<-- BEST");
    }
  }
    
  cat("\n");
}

cat("Best Result for <nF> = ", bestParams$maxFeatureCount, "\n");
cat("Accuracy(Test set): ", bestPerf$acc, "\n");
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
