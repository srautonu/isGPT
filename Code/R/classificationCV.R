library(e1071)
library(ROCR)
library(randomForest)

source('featurefiltering.R');
source('svmCV.R')

timestamp();

#set.seed(10);

balancing = "_SMOTED";
fScheme   = "_comb";

# File names #
fileNameSuffix = paste(fScheme, balancing, ".rds", sep = "");

rankedFeaturesFile  = "rankedFeatures.rds" 
#rankedFeaturesFile = paste("ff"            , fileNameSuffix, sep = "");
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

# jackknife
nFolds = length(features[,1])

# 10 fold CV
#nFolds = 10

# random shuffle of features
features <- features[sample(nrow(features)),]

bestPerf = NULL;
bestParams = NULL;
accData = NULL;

# svmCostList = c(0.3, 1, 3, 10, 30, 100);
# featureCountList = seq(from=2800, to=1500, by=-50); 

svmCostList = c(10);
featureCountList = seq(from=2800, to=2800, by=50); 

cat(as.character(Sys.time()),">> Entering cross validation. Folds = ", nFolds, " ...\n");

# Reduce the feature vectors to the max size that we will be testing.
# This way the filtering cost in the loop below will be reduced.
features = featurefiltering(features, rankedFeatures, max(featureCountList));

for (maxFeatureCount in featureCountList) 
{
  trainingSet = featurefiltering(features, rankedFeatures, maxFeatureCount);

  for (svmC in svmCostList) 
  {
    perf = svmCV(protection ~ ., trainingSet, svmCost = svmC, cross = nFolds);

    cat(maxFeatureCount, ",", svmC, ",", perf$acc, ",", perf$sens, ",", perf$spec, ",", perf$mcc);
    
    accData = rbind(accData, c(maxFeatureCount, svmC, perf$acc, perf$sens, perf$spec, perf$mcc));
    write.csv(accData, outFile);
    
    if (is.null(bestPerf) || bestPerf$mcc < perf$mcc) {
      bestPerf = perf;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

cat("Best Result for <nF, C> = ", bestParams$maxFeatureCount, bestParams$svmC, "\n");
cat("Accuracy(Test set): ", bestPerf$acc, "\n");
cat("Sensitivity(Test set): ", bestPerf$sens, "\n");
cat("Specificity(Test set): ", bestPerf$spec, "\n")
cat("MCC(Test set): ", bestPerf$mcc, "\n")
