wd<- "D:/OneDrive/disease"
setwd(wd)

#OTU table
otu <- read.delim('disease.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,na.strings="na")
otu[is.na(otu)] <- 0
otu <- data.frame(t(otu))

#group table
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

library(vegan)

#calculation of the Bray¨CCurtis dissimilarity
distance <- vegdist(otu, method = 'bray') 
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = TRUE)
write.csv(as.matrix(distance),'distance.csv')#output distance

#extract the first two coordinate
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)
sample_site <- data.frame({pcoa$point})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')

#add the information of group
sample_site <- merge(sample_site, group,by ='names', all.x = TRUE)

library(ggplot2)

pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = group)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = 0, color = 'gray', size = 0.3) + #add the line of pco1=0
  geom_hline(yintercept = 0, color = 'gray', size = 0.3) + #add the line of pco2=0
  geom_point(aes(color =group), size = 3, alpha = 0.8) + #set the size, color and transparency of points
  stat_ellipse(level = 0.90, show.legend = F,aes(color = group),linetype="longdash")+#add the ellipse
  scale_color_manual(values = c('red', 'skyblue',"green")) + 
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%')) +
  annotate('text', label = 'R2=0.3684,P=0.151', x = 0, y = 0, size = 5, colour = '#73D5FF')#results of PERMANOVA (calculate as below)
 
pcoa_plot

#PERMANOVA
adonis_result_otu <- adonis(otu~group, group, permutations = 999, distance = 'bray') 
adonis_result_otu