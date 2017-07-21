source('base.R');

bestAcc = 0;
bestSVM = NULL;
bestParams = NULL;
accData = NULL;

#nTrainingSet = read.csv('trainingset.csv')
#nTestSet = read.csv('testingset.csv')
#cat(as.character(Sys.time()),">> Training set entries:", length(nTrainingSet[,1]), "\n");
#cat(as.character(Sys.time()),">> Test set entries:", length(nTestSet[,1]), "\n");

#2150 features best

cat(as.character(Sys.time()),">> Entering to create SVM model", "\n")

maxfeatures = 2150
cat(as.character(Sys.time()),">> Max Features", maxfeatures, "\n")

for (maxFeatureCount in seq(from=maxfeatures, to=2150, by=-20)) 
{ 
  filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
  trainingSet = filteringRes$trainingSet;
  testSet = filteringRes$testSet;

  svmCostList = c(0.01, 0.03, 0.10, 0.30, 1, 3, 10, 30, 100);
  for (svmC in svmCostList) 
  {
    svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC, cross = 10, scale = TRUE);
    perf = svmmodel$tot.accuracy;
    
    cat(maxFeatureCount, ",", svmC, ",", perf);

    accData = rbind(accData, c(maxFeatureCount, svmC, perf));
    write.csv(accData, outFile);

    if (bestAcc < perf) {
      bestAcc = perf;
      bestSVM = svmmodel;
      bestParams = list(
        "maxFeatureCount" = maxFeatureCount,
        "svmC" = svmC
      )
      cat(",<-- BEST");
    }
    
    cat("\n");
  }
}

cat("Best Result:\n");
cat("<nF, C, Acc> = ", bestParams$maxFeatureCount, bestParams$svmC, bestAcc, "\n");
saveRDS(bestSVM, svmFile)