features = readRDS("featurized_comb.rds")
pseAAC = readRDS("featurized_pseAAC.rds")

protection = features$protection;
features$protection = NULL;
features$ID  = seq.int(nrow(features))

pseAAC$ID <- seq.int(nrow(pseAAC))
pseAAC$protection = NULL;

features = merge(features, pseAAC, by="ID");
features$protection = protection;
features$ID = NULL;
