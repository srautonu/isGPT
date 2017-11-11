impindex = 3
rfmodel = readRDS(rfmodelFile);
rankedt = (rfmodel$importance[order(-rfmodel$importance[,impindex]),])
cname = rownames(rankedt)
pif = 0
psf = 0
gap = 0
pifv = c()
psfv = c()
gapv = c()
for(i in 1:3000){
  if(gregexpr(pattern = "C_", cname[i])[[1]][1]>0){
    pif = pif + rankedt[i,impindex]
    pifv = c(pifv,cname[i])
  } else if(gregexpr(pattern = "P_", cname[i])[[1]][1]>0){
    psf = psf + rankedt[i,impindex]
    psfv = c(psfv,cname[i])
  }  else{
    gap = gap + rankedt[i,impindex]
    gapv = c(gapv,cname[i])
  }
}
cat(pif,"\n")
cat(psf,"\n")
cat(gap,"\n")