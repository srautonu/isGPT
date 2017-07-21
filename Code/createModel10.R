source('base.R');
library(DMwR)
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

maxfeatures = 3350
cat(as.character(Sys.time()),">> Max Features", maxfeatures, "\n")
features = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 200)
for (maxFeatureCount in seq(from=maxfeatures, to=2050, by=-20)) 
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
saveRDS(bestSVM, "10-foldCrossValidation.rds")
svmpred = predict(bestSVM, features)
svmprediction = prediction(as.numeric(svmpred), as.numeric(trainingSet$protection));

acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
F1 = unlist(ROCR::performance(svmprediction,"f")@y.values)[2]
prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)[2]
recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)[2]
sensitiviy = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];


cat("10-fold Cross Validation Testing....\n\n");
cat("Accuracy(Test set): ", acc, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Precision(Test set): ", prec, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n")
cat("MCC(Test set): ", mccv, "\n")

#source("testModel.R")
source("alltestings.R")