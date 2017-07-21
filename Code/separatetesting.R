source('featureimportance.R')
pifv[length(pifv)+1]="protection"
psfv[length(psfv)+1]="protection"
gapv[length(gapv)+1]="protection"
piform = featureformula(pifv)
psform = featureformula(psfv)
gapform = featureformula(gapv)
svmmodelpi = svm(as.formula(piform), features, kernel = "linear", cost = svmC, cross = 10, scale = TRUE)
svmmodelps = svm(as.formula(psform), features, kernel = "linear", cost = svmC, cross = 10, scale = TRUE)
svmmodelgap = svm(as.formula(gapform), features, kernel = "linear", cost = svmC, cross = 10, scale = TRUE)

svmpredpi = predict(svmmodelpi, testFeatures);
svmpredictionpi = prediction(as.numeric(svmpredpi), as.vector(testFeatures$protection));
cat("PI...\n")
accpi = unlist(ROCR::performance(svmpredictionpi,"acc")@y.values)[2]
mccvpi = unlist(ROCR::performance(svmpredictionpi,"mat")@y.values)[2]

svmpredps = predict(svmmodelps, testFeatures);
svmpredictionps = prediction(as.numeric(svmpredps), as.vector(testFeatures$protection));
cat("PS...\n")
accps = unlist(ROCR::performance(svmpredictionps,"acc")@y.values)[2]
mccvps = unlist(ROCR::performance(svmpredictionps,"mat")@y.values)[2]

svmpredgap = predict(svmmodelgap, testFeatures);
svmpredictiongap = prediction(as.numeric(svmpredgap), as.vector(testFeatures$protection));
cat("GAP...\n")
accgap = unlist(ROCR::performance(svmpredictiongap,"acc")@y.values)[2]
mccvgap = unlist(ROCR::performance(svmpredictiongap,"mat")@y.values)[2]
