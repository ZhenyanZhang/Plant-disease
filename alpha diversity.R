wd<- "D:/OneDrive/disease"
setwd(wd)

otu <- read.delim('genus.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

library(vegan)

richness <- rowSums(otu > 0)

shannon_index <- diversity(otu, index = 'shannon', base = exp(1))

write.csv(richness, 'richness.csv', quote = FALSE)
write.csv(shannon_index, 'shannon_index.csv', quote = FALSE)


