combineFeatures <-
  function(subFeatureFilePrefix, featureSchemes, balancing) {

    features = NULL;
    protection = NULL;
    for (fScheme in featureSchemes) {
      curFeatureFile = paste(subFeatureFilePrefix, fScheme, balancing, ".rds", sep = "");
      curFeatures = readRDS(curFeatureFile);
      curFeatures$ID <- seq.int(nrow(curFeatures))
      cat(as.character(Sys.time()),">> Read file: ", curFeatureFile, "\n");
      # protection is a common column in each sub feature file
      # We will merge by ID. The protection column should come from the
      # first file. Subsequently they will be ignored from other files
      if (is.null(protection)) {
        protection = curFeatures$protection
      }
      curFeatures$protection = NULL;
      if (is.null(features)) {
        features = curFeatures;
      } else {
        features = merge(features, curFeatures, by="ID");
      }
    }
    features$protection = protection;
    features$ID = NULL;
    cat(as.character(Sys.time()),">> Total features: ", length(features[1,]), "\n");
    return(features)
  }