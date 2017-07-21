source('base.R')
library(DMwR)
nFeatures = 2500
#features = DMwR::SMOTE(protection~., features, perc.over = 100,k=2, perc.under = 200)
for (maxFeatureCount in seq(from=nFeatures, to=1050, by=-50)){

filteringRes = featurefiltering(features, testFeatures, rankedFeatures, maxFeatureCount)

#trainingSet = filteringRes$testSet

jnVector = c()
bestjnmodel = NULL
bestac = 0
cat(as.character(Sys.time()),
    ">> Calculating leave one out result: \n")
smotepred = c()
wepred = c()
#svmmodel = readRDS(svmFile);
traindataset = filteringRes$trainingSet

for (i in 1:length(traindataset[, 1])) {
  trainingSet = traindataset
  testSet = trainingSet[i,]
  trainingSet = trainingSet[-i,]
  svmmodel = svm(
   as.factor(protection) ~ .,
   trainingSet,
   kernel = "linear",
   cross = 0,
   cost = 0.3,
   scale = TRUE
  )
  
  svmpred = predict(svmmodel, testSet)
  jnVector = c(jnVector, as.numeric(svmpred))
  i = i + 1
 # cat("Value of i:", i, " predicted val:", as.numeric(svmpred), " true val:",testSet$protection,"\n")
}
svmprediction = prediction(as.numeric(jnVector), as.numeric(traindataset$protection))
#acc = max(unlist(ROCR::performance(svmprediction,"acc")@y.values), na.rm = TRUE)
#F1 = max(unlist(ROCR::performance(svmprediction,"f")@y.values), na.rm = TRUE)
#prec = max(unlist(ROCR::performance(svmprediction,"prec")@y.values), na.rm = TRUE)
#recall = max(unlist(ROCR::performance(svmprediction,"rec")@y.values), na.rm = TRUE)
#sensitiviy = max(unlist(ROCR::performance(svmprediction,"sens")@y.values), na.rm = TRUE)
#specificity = max(unlist(ROCR::performance(svmprediction,"spec")@y.values), na.rm = TRUE)
#mccv = max(unlist(ROCR::performance(svmprediction,"mat")@y.values), na.rm = TRUE)
acc = unlist(ROCR::performance(svmprediction,"acc")@y.values)
F1 = unlist(ROCR::performance(svmprediction,"f")@y.values)
prec = unlist(ROCR::performance(svmprediction,"prec")@y.values)
recall = unlist(ROCR::performance(svmprediction,"rec")@y.values)
sensitiviy = unlist(ROCR::performance(svmprediction,"sens")@y.values)
specificity = unlist(ROCR::performance(svmprediction,"spec")@y.values)
mccv = unlist(ROCR::performance(svmprediction,"mat")@y.values)


cat("Results using SMOTE:\n")

cat("For Features Nubers:", maxFeatureCount,"\n");
cat("Accuracy(Test set): ", max(acc, na.rm=TRUE), "\n");
# cat("F1-Score (Test set): ", F1, "\n");
# cat("Precision(Test set): ", prec, "\n");
# cat("Recall   (Test set): ", recall, "\n");
# cat("Sensitivity(Test set): ", sensitiviy, "\n");
# cat("Specificity(Test set): ", specificity, "\n")
cat("MCC(Test set): ", max(mccv, na.rm = TRUE), "\n")
cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
a = max(acc, na.rm=TRUE)
if(bestac<a){
  bestac = a
  bestjnmodel = svmmodel
  wepred = as.numeric(jnVector)
  smotepred = as.numeric(traindataset$protection)
}
}

preddata = data.frame(wepred, smotepred)
saveRDS(preddata,"regressionjackknifetestpredictionsdata.rds")
saveRDS(bestjnmodel, "regressionbestjackknifemodel.rds")


#source('knntest.R')
