featureFileNames =  c(
#  "featurized_nGDip",
#  "featurized_nGrams",
#  "featurized_PSF",
  "featurized_pssm_nGDip25"
);

for (featureFileName in featureFileNames) {
  
  inputFileName  = paste(featureFileName, ".rds", sep="");
  outputFileName = paste(featureFileName, ".csv", sep="");

  cat(as.character(Sys.time()),">> Reading file", inputFileName, " ...\n");
  t = readRDS(inputFileName);
  cat(as.character(Sys.time()),">> Done.\n");
  
  cat(as.character(Sys.time()),">> Writing file", outputFileName, " ...\n");
  write.csv(t, outputFileName, row.names = FALSE);
  cat(as.character(Sys.time()),">> Done.\n");
}