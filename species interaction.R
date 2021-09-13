wd<- "D:/OneDrive/disease"
setwd(wd)

library(psych)

OTU=read.table("otu.txt", head=T, row.names=1)
# calculate the correlation with the p-value 
occor = corr.test(OTU,use="pairwise",method="spearman",adjust="fdr",alpha=0.05) 
occor.r = occor$r #correlation
occor.p = occor$p #p-value 
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0 #filtering
write.csv(occor.r,file="species interaction.csv") # save the species interaction
