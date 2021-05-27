library(randomForest)
library(ggplot2)
library("caret")
library(ROCR)

wd<- "D:/OneDrive/disease/RF/RF2"
setwd(wd)
data.all <- read.table("genus.txt", head=T, row.names=1)
data.test <- read.table("pestcides.txt", head=T, row.names=1)

set.seed(10000)
folds <- createFolds(y=data.all$group,k=10)
col.num=ncol(data.all)
col.num2=col.num+1
max=0
num=0
for(i in 1:10){    
  fold_test <- data.all[folds[[i]],]   
  fold_train <- data.all[-folds[[i]],]      
  fold_pre <-randomForest(as.factor(group) ~ .,data= fold_train,na.action = na.omit,type='class', proximity=TRUE,importance=TRUE)
  fold_predict <- predict(fold_pre,type='class',newdata=fold_test) 
  fold_test$predict = fold_predict
  true.num=length(which(fold_test[,col.num]==fold_test[,col.num2]))
  fold_accuracy = true.num/nrow(fold_test)   
  print(fold_accuracy)
  if(fold_accuracy>max)    {    
    max=fold_accuracy      
    num=i   
  }  }
print(max)
print(num)

testi <- data.all[folds[[num]],]

traini <- data.all[-folds[[num]],]
#establish the final model
prei <- randomForest(as.factor(group) ~ .,data= traini,type='class',proximity=TRUE,importance=TRUE)
prei
predict <- predict(prei,type='class',newdata=testi)
testi$predict = predict

#ROC and AUC
prob <- predict(prei,type='prob',newdata=data.test)
data.test$prob = prob
roc<-roc(as.numeric(testi$group),prob[,2])
x<-1-roc$specificities
y<-roc$sensitivities
plotdata<-data.frame(x,y)
names(plotdata)<-c("x","y")
ggplot(plotdata)+
  geom_path(aes(x,y),color='red')+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  labs(x = paste('1-Specificity'), y = paste('Sensitivity'))+
  annotate('text', label = roc$auc, x = 0.5, y = 0.5, size = 5, colour = '#73D5FF')+
  geom_abline(slope=1,intercept=0)

#importance of factors
importance<-importance(x= prei)
write.csv(data.test,"data.test.csv")


##Cross Validation
result <- replicate(5, rfcv(data.all[-ncol(data.all)], data.all$group, cv.fold = 10,step = 1.5,scale = "log"), simplify = FALSE)
error.cv=sapply(result,"[[","error.cv")
matplot(result[[1]]$n.var,cbind(rowMeans(error.cv),error.cv),type = "l",lwd = c(2,rep(1,ncol(error.cv))),col = 1,lty = 1,log="x",xlab = "Number of genus",ylab="CV Error")


