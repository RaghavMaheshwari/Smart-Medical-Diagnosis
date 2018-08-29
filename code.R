library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(pbkrtest)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

heart <- read.csv("heart.csv", 
                 header = FALSE, 
                 sep = ",")
colnames(heart) <- c("Age","Gender","CP","TBps",
                    "Chol","Fbs","Recg","Thalach","Exang","Op",
                    "Slope","Ca","Thal","Heart")

#heart[heart == "?"] <- NA
str(heart)
heart$CP=as.numeric(as.character(heart$CP))
heart$TBps=as.numeric(as.character(heart$TBps))
heart$Chol=as.numeric(as.character(heart$Chol))
heart$Fbs=as.numeric(as.character(heart$Fbs))
heart$Recg=as.numeric(as.character(heart$Recg))
heart$Thalach=as.numeric(as.character(heart$Thalach))
heart$Exang=as.numeric(as.character(heart$Exang))
heart$Op=as.numeric(as.character(heart$Op))
heart$Slope=as.numeric(as.character(heart$Slope))
heart$Ca=as.numeric(as.character(heart$Ca))
heart$Thal=as.numeric(as.character(heart$Thal))
heart$Heart=as.numeric(as.character(heart$Heart))




str(heart)

heart$Age[which(is.na(heart$Age))]= mean(heart$Age, na.rm = TRUE)
heart$Gender[which(is.na(heart$Gender))]= mean(heart$Gender, na.rm = TRUE)
heart$CP[which(is.na(heart$CP))]= mean(heart$CP, na.rm = TRUE)
heart$TBps[which(is.na(heart$TBps))]= mean(heart$TBps, na.rm = TRUE)
heart$Chol[which(is.na(heart$Chol))]= mean(heart$Chol, na.rm = TRUE)
heart$Fbs[which(is.na(heart$Fbs))]= mean(heart$Fbs, na.rm = TRUE)
heart$Recg[which(is.na(heart$Recg))]= mean(heart$Recg, na.rm = TRUE)
heart$Thalach[which(is.na(heart$Thalach))]= mean(heart$Thalach, na.rm = TRUE)
heart$Exang[which(is.na(heart$Exang))]= mean(heart$Exang, na.rm = TRUE)
heart$Op[which(is.na(heart$Op))]= mean(heart$Op, na.rm = TRUE)
heart$Slope[which(is.na(heart$Slope))]= mean(heart$Slope, na.rm = TRUE)
heart$Ca[which(is.na(heart$Ca))]= mean(heart$Ca, na.rm = TRUE)
heart$Thal[which(is.na(heart$Thal))]= mean(heart$Thal, na.rm = TRUE)
heart$Heart[which(is.na(heart$Heart))]= mean(heart$Heart, na.rm = TRUE)
summary(heart)


str(heart)

heart$Heart[heart$Heart == "2"] <- "1"
heart$Heart[heart$Heart == "3"] <- "1"
heart$Heart[heart$Heart == "4"] <- "1"
heart$Heart=as.numeric(as.character(heart$Heart))
library(caret)

preprocessParamsh <- preProcess(heart, method=c("scale"))
print(preprocessParamsh)
transformedh <- predict(preprocessParamsh, heart)


preprocessParamsh <- preProcess(heart, method=c("center"))
print(preprocessParamsh)
transformedh <- predict(preprocessParamsh,heart)


preprocessParamsh <- preProcess(heart, method=c("center", "scale"))
print(preprocessParamsh)
transformedh <- predict(preprocessParamsh,heart)


preprocessParamsh <- preProcess(heart, method=c("range"))
print(preprocessParamsh)
transformedh <- predict(preprocessParamsh, heart)
summary(transformedh)
str(heart)
heart = transformedh
########################################################################
########################################################################

cp_meanh <- heart %>% group_by(Heart) %>% summarise(Plas = round(mean(CP),2))

#Relationship between Cp, heart
cp = ggplot(data=heart,aes(Heart,CP)) + 
  geom_boxplot(aes(fill=Heart)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Heart rates against CP Levels") + 
  xlab("Heart") + ylab("CP") + guides(fill=F) + 
  geom_text(data = cp_meanh, aes(x=Heart,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)

cp

heart$Heart[heart$Heart > 0] <- 1
fate =barplot(table(heart$Heart),
        main="Fate", col="black")

fate

ins_meanh <- heart %>% group_by(Heart) %>% summarise(Plas = round(mean(Chol),2))

#Relationship between Heart & Chol levels
Chol_ = ggplot(data=heart,aes(Heart,Chol)) + 
  geom_boxplot(aes(fill=Heart)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Heart rates against Chol Levels") + 
  xlab("Heart") + ylab("Chol") + guides(fill=F) + 
  geom_text(data = ins_meanh, aes(x=Heart,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)


Chol_
library(tidyr)
#ALL
gather(heart, x, y, Age:Thal) %>%
  ggplot(aes(x = y, color = Heart, fill = Heart)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

#Thalach and Op
t_op= ggplot(heart, aes(x = Thalach, y = Op, color = Heart)) +
  geom_point() +
  ylab("Thalach") +
  xlab("OP") +
  ggtitle("Relationship") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

t_op

#Relationship between Age and Heart
age_= ggplot(heart, aes(Age, fill = Heart)) +
  geom_density() + ylab("Heart Risk") + 
  ggtitle("Age vs. the threat of Heart")

age_


str(heart)
corrplot(cor(heart[,-9]),type = "lower", method = "number")
set.seed(15689)
indexh <- createDataPartition(heart$Heart,p = 0.7,list = F)
trainh <- heart[indexh,]
testh  <- heart[-indexh,]

########################################################################
########################################################################
#Logistic Regression


m1h <- glm(Heart ~ ., data = trainh, family = binomial(link = "logit"))
summary(m1h)

anova(m1h,testh = "Chisq")
mod_finh <- glm(Heart ~ Age + CP + TBps + Chol + Fbs + Recg + Thalach + Exang +Op + 
                 Slope + Ca + Thal,
               data = trainh, family = binomial(link = "logit"))
summary(mod_finh)

anova(m1h,testh = "Chisq")
mod_finh <- glm(Heart ~ Age + CP + TBps + Chol + Fbs + Recg + Thalach + Exang +Op + 
                 Slope + Ca + Thal,
               data = trainh, family = binomial(link = "logit"))
summary(mod_finh)
summary(residuals(mod_finh))
par(mfrow=c(2,2))
plot(mod_finh)

testh_pred <- predict(mod_finh,testh, type = "response")
pred_testhh <- as.data.frame(cbind(testh$Heart,testh_pred))
colnames(pred_testhh) <- c("Original","testh_pred")
pred_testhh$outcome <- ifelse(pred_testhh$testh_pred > 0.5, 1, 0)
error <- mean(pred_testhh$outcome != testh$Heart)
print(paste('testh Data Accuracy', round(1-error,2)*100,'%'))
confusionMatrix(testh$Heart,pred_testhh$outcome)

acc_lgh <- confusionMatrix(testh$Heart,pred_testhh$outcome)$overall['Accuracy']

par(mfrow=c(1,1))
plot.roc(testh$Heart,testh_pred,percent=TRUE,col="#1c61b6",print.auc=TRUE,
         main = "Area under the curve for Logistic Regression")


########################################################################
########################################################################

#Bayesian Logistic Regression
prior_disth <- student_t(df = 7, location = 0, scale = 2.5)
bayes_modh  <- stan_glm(Heart ~ ., data = trainh,
                       family = binomial(link = "logit"), 
                       prior = prior_disth, prior_intercept = prior_disth,
                       seed = 15689)


posterior_interval(bayes_modh, prob = 0.95)

summary(residuals(bayes_modh))

bayes_resh <- data.frame(residuals(bayes_modh))
bayes_resh$indexh <- seq.int(nrow(bayes_resh)) 


pred <- posterior_linpred(bayes_modh, newdata = testh, transform=TRUE)
fin_predh <- colMeans(pred)
testh_prediction <- as.integer(fin_predh >= 0.5)

confusionMatrix(testh$Heart,testh_prediction)

acc_bayesh <- confusionMatrix(testh$Heart,testh_prediction)$overall['Accuracy']

plot.roc(testh$Heart,fin_predh,percent=TRUE,col="#1c61b6", print.auc=TRUE,
         main = "Area under the curve for Bayesian Logistic Regression")


########################################################################
########################################################################
#Decision Tress



library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(Heart ~ .,
             data = trainh,
             method = "class",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)
pred_dt_testh <- predict(fit, testh, type = "class")
confusionMatrix(testh$Heart,pred_dt_testh)




set.seed(15689)
m_dth <- tree(Heart ~ ., data = trainh)
pred_dth <- predict(m_dth, trainh)
confusionMatrix(trainh$Heart,pred_dth)
plot(m_dth)
text(m_dth, pretty = 0)
pred_dt_testh <- predict(m_dth, testh, type = "class")
confusionMatrix(testh$Heart,pred_dt_testh)


acc_dth <- confusionMatrix(pred_dt_testh,testh$Heart)$overall['Accuracy']


########################################################################
########################################################################
#Random Forest
heart$Heart <- as.factor(heart$Heart)
trainh$Heart <- as.factor(trainh$Heart)
testh$Heart <- as.factor(testh$Heart)
str(heart)
str(trainh)

set.seed(42)
model_rfh <- caret::train(Heart ~ .,
                         data = trainh,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))
model_rfh$finalModel$confusion
imph <- model_rfh$finalModel$imphortance
imp[order(imph, decreasing = TRUE), ]
importanceh <- varImp(model_rfh, scale = TRUE)
plot(importanceh)
confusionMatrix(predict(model_rfh, testh), testh$Heart)
acc_rfh <- confusionMatrix(predict(model_rfh, testh), testh$Heart)$overall['Accuracy']


########################################################################
########################################################################
#Extreme gradient boosting


model_xgbh <- caret::train(Heart ~ .,
                          data = trainh,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 10, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))

imphortance <- varimph(model_xgbh, scale = TRUE)
plot(imphortance)
confusionMatrix(predict(model_xgbh, testh), testh$Heart)
acc_xbgh <- confusionMatrix(predict(model_xgbh, testh), testh$Heart)$overall['Accuracy']  ; acc_gnh= 0.83

df= data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,10)
colnames(df)=colnames(testh)
predict(model_xgbh, testh)
predict(model_xgbh,df)
########################################################################
########################################################################
#Genetic

set.seed(27)
model_gnh <- gafs(x = trainh[, -1], 
                 y = trainh$Heart,
                 iters = 10, 
                 popSize = 10, 
                 levels = c("NO", "Yes"),
                 gafsControl = gafsControl(functions = rfGA, 
                                           method = "cv",    
                                           genParallel = TRUE, 
                                           allowParallel = TRUE))

plot(model_gnh)
trainh_ga <- trainh[, c(1, which(colnames(trainh) %in% model_gnh$ga$final))]

confusionMatrix(predict(trainh_ga, testh), testh$Heart)
acc_gah <- confusionMatrix(predict(model_gnh, testh), testh$Heart)$overall['Accuracy'] 

a = predict(model_gnh, testh, type = 'class')
b = testh_data$Class
xtab<- table(a,b)
library(caret)
library(pbkrtest)
confusionMatrix(xtab)


########################################################################
########################################################################

acc_rfh

accuracyh <- data.frame(Model=c("Logistic","Bayesian Logistic","Decision Tree",
                               "Random Forest","Boosting","Genetics"),
                       Accuracy=c(acc_lgh,acc_bayesh,acc_dth,
                                  acc_rfh,acc_xbgh,acc_gnh))

acc_heart= ggplot(accuracyh,aes(x=Model,y=Accuracy))+geom_bar(stat='identity')+theme_bw()+
  ggtitle('Comparison of Model Accuracy')

acc_heart
########################################################################
########################################################################

#neural networks

str(heart)
heart$Heart=as.numeric(as.character(heart$Heart))
str(heart)
library(neuralnet)
mh= model.matrix(
  ~ Heart + Age + Gender + CP + TBps + Chol +
    Fbs + Recg + Thalach + Exang + Op + 
    Slope + Ca + Thal,
  data=heart
)
nnh <- neuralnet(Heart ~ Age + Gender + CP + TBps + Chol +
                 Thalach + Exang + Op + 
                  Slope + Ca + Thal,
                data=m, hidden=c(2,1), 
                linear.output=FALSE, threshold=0.01)
nnh$result.matrix
plot(nnh)


dfh= data.frame(2,2,22,22,23,24,2.4,55,5,7.88,99,12,1,2)
colnames(dfh)=colnames(testh)

p <- preProcess(dfh, method=c("scale"))
t <- predict(p, dfh)
p <- preProcess(dfh, method=c("center"))
t <- predict(p,dfh)
p <- preProcess(dfh, method=c("center", "scale"))
t <- predict(p,dfh)
p <- preProcess(dfh, method=c("range"))
t <- predict(p, dfh)
dfh=t


##########################################################################
##########################################################################

##########################################################################
##########################################################################


library(keras)

library(tensorflow)
model_keras <- keras_model_sequential()

model_keras %>% 
  
   layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_trainh)) %>% 
  
 
  layer_dropout(rate = 0.1) %>%
  
 
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  
  
  layer_dropout(rate = 0.1) %>%
  

  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  
  
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

keras_model







