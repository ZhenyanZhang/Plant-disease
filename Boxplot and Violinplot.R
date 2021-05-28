wd<- "D:/OneDrive"
setwd(wd)

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)


# create a dataset
data <- read.delim('argabundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE,na.strings="na")
#data <- data[apply(data,1,var)!=0,]
# Plot

ggplot(data, aes(x=value, y=type)) +
  geom_violin(width =0.35)+
  geom_boxplot(notch=F,notchwidth=0,width =0.6) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.9, alpha=0.9,width =0.02) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=9, color="red", fill="red") +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent'))+
  xlab("")