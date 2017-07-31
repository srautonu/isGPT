library(DMwR)

source('base.R');

svmFile = "Khaled_SVM_Classification_TenFoldCV_NoSMOTE_GoodITPerf.rds";

cat(as.character(Sys.time()),">> Reading SVM model from", svmFile, "...\n");
svmmodel = readRDS(svmFile);
cat(as.character(Sys.time()),">> Done\n");

nFeatures = length(svmmodel$SV[1,]);

filteringRes = featurefiltering(features, testFeatures, rankedFeatures, nFeatures);
trainingSet = filteringRes$trainingSet;
testSet = filteringRes$testSet;

cat(as.character(Sys.time()),">> Trained:Test Features", length(trainingSet[1,]), ":", length(testSet[1,]),"\n");

svmpred = predict(svmmodel, testSet);
svmprediction = prediction(as.numeric(svmpred), as.vector(testSet$protection));

#acc = max(unlist(performance(svmprediction,"acc")@y.values), na.rm = TRUE)
#F1 = max(unlist(performance(svmprediction,"f")@y.values), na.rm = TRUE)
#prec = max(unlist(performance(svmprediction,"prec")@y.values), na.rm = TRUE)
#recall = max(unlist(performance(svmprediction,"rec")@y.values), na.rm = TRUE)
#sensitiviy = max(unlist(performance(svmprediction,"sens")@y.values), na.rm = TRUE)
#specificity = max(unlist(performance(svmprediction,"spec")@y.values), na.rm = TRUE)
#mccv = max(unlist(performance(svmprediction,"mat")@y.values), na.rm = TRUE)

acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)[2]
F1 = unlist(ROCR::performance(svmprediction,"f")@y.values)[2]
prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)[2]
recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)[2]
sensitiviy = unlist(ROCR::performance(svmprediction,"sens")@y.values)[2];
specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)[2];
mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)[2];

cat("Independent Testing....\n\n");
cat("Accuracy(Test set): ", acc, "\n");
cat("F1-Score (Test set): ", F1, "\n");
cat("Precision(Test set): ", prec, "\n");
cat("Recall   (Test set): ", recall, "\n");
cat("Sensitivity(Test set): ", sensitiviy, "\n");
cat("Specificity(Test set): ", specificity, "\n")
cat("MCC(Test set): ", mccv, "\n")