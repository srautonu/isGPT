source('base.R');

maxFeatureCount = 2000;
svmC = 0.03

cat(as.character(Sys.time()),">> Entering to create SVM model ...\n");


filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount);
trainingSet = filteringRes$trainingSet;

    
svmmodel = svm(protection ~ ., trainingSet, kernel = "linear", cost = svmC);
saveRDS(svmmodel, svmFile)