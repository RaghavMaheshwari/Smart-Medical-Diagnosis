library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

pima <- read.csv("diabetes.csv", 
                     header = FALSE, 
                     sep = ",")
colnames(pima) <- c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin",
                    "Serum_Insulin","BMI","DPF","Age","Diabetes")
sapply(pima, function(x) sum(is.na(x)))
summary(pima)

pima$Diabetes <- as.factor(pima$Diabetes)

#Remove entry if Glucose,BP or BMI == 0
pima <- pima[apply(pima[,c(2,3,6)],1,function(x) !any(x==0)),]


########################################################################
########################################################################

gluc_mean <- pima %>% group_by(Diabetes) %>% summarise(Plas = round(mean(Plasma_Glucose),2))

#Relationship between Plasma Glucose & Diabetes
ggplot(data=pima,aes(Diabetes,Plasma_Glucose)) + 
  geom_boxplot(aes(fill=Diabetes)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Diabetes rates against Plasma Glucose Levels") + 
  xlab("Diabetes") + ylab("Plasma Glucose") + guides(fill=F) + 
  geom_text(data = gluc_mean, aes(x=Diabetes,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)


ins_mean <- pima %>% group_by(Diabetes) %>% summarise(Plas = round(mean(Serum_Insulin),2))

#Relationship between Diabetes & Serum Insulin levels
ggplot(data=pima,aes(Diabetes,Serum_Insulin)) + 
  geom_boxplot(aes(fill=Diabetes)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Diabetes rates against Serum Insulin Levels") + 
  xlab("Diabetes") + ylab("Serum Insulin") + guides(fill=F) + 
  geom_text(data = ins_mean, aes(x=Diabetes,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)


#Difference in Blood Pressure & BMI for Diabetics
ggplot(data = pima,aes(Dias_BP,BMI)) + geom_point(aes(colour=Diabetes),alpha=0.6) +
  xlab("Diastolic Blood Pressure") + ylab("Body Mass Index") + 
  ggtitle("Interaction between Blood Pressure & BMI for Diabetics") + 
  labs(colour="Diabetes Status") + 
  scale_colour_manual(values = c("#D55E00", "#009E73"))

#Relationship between pregnancy and diabetes
ggplot(pima, aes(Pregnant, fill = Diabetes)) +
  geom_density() + ylab("Distribution of Pregnancy") + 
  ggtitle("Pregnant Women vs. the threat of Diabetes")





corrplot(cor(pima[,-9]),type = "lower", method = "number")
set.seed(15689)
index <- createDataPartition(pima$Diabetes,p = 0.7,list = F)
train <- pima[index,]
test  <- pima[-index,]

########################################################################
########################################################################
#Logistic Regression

m1 <- glm(Diabetes ~ ., data = train, family = binomial(link = "logit"))
summary(m1)

anova(m1,test = "Chisq")
mod_fin <- glm(Diabetes ~ Pregnant + Plasma_Glucose + Triceps_Skin + BMI + DPF,
               data = train, family = binomial(link = "logit"))
summary(mod_fin)

anova(m1,test = "Chisq")
mod_fin <- glm(Diabetes ~ Pregnant + Plasma_Glucose + Triceps_Skin + BMI + DPF,
               data = train, family = binomial(link = "logit"))
summary(mod_fin)
summary(residuals(mod_fin))
par(mfrow=c(2,2))
plot(mod_fin)



#Apply the model to the testing sample
test_pred <- predict(mod_fin,test, type = "response")
pred_test <- as.data.frame(cbind(test$Diabetes,test_pred))
colnames(pred_test) <- c("Original","Test_pred")
pred_test$outcome <- ifelse(pred_test$Test_pred > 0.5, 1, 0)
error <- mean(pred_test$outcome != test$Diabetes)
print(paste('Test Data Accuracy', round(1-error,2)*100,'%'))
confusionMatrix(test$Diabetes,pred_test$outcome)

acc_lg <- confusionMatrix(test$Diabetes,pred_test$outcome)$overall['Accuracy']

# Get the ROC curve and the AUC
par(mfrow=c(1,1))
plot.roc(test$Diabetes,test_pred,percent=TRUE,col="#1c61b6",print.auc=TRUE,
         main = "Area under the curve for Logistic Regression")


########################################################################
########################################################################

#Bayesian Logistic Regression
prior_dist <- student_t(df = 7, location = 0, scale = 2.5)
bayes_mod  <- stan_glm(Diabetes ~ ., data = train,
                       family = binomial(link = "logit"), 
                       prior = prior_dist, prior_intercept = prior_dist,
                       seed = 15689)

#Confidence Intervals for the predictors
posterior_interval(bayes_mod, prob = 0.95)
#Residuals for the Bayesian Model
summary(residuals(bayes_mod))

bayes_res <- data.frame(residuals(bayes_mod))
bayes_res$index <- seq.int(nrow(bayes_res)) 
colnames(bayes_res) <- "Residuals"

#Plotting the residuals
ggplot(data = bayes_res,aes(index,Residuals)) + geom_point() + ggtitle("Representation of randomness amongst Residuals")

ggplot(data = bayes_res,aes(Residuals)) + geom_density(aes(fill=Residuals)) + 
  ylab("Density") + ggtitle("Distribution of Residuals")

#Predicting Probabilities for the test data
pred <- posterior_linpred(bayes_mod, newdata = test, transform=TRUE)
fin_pred <- colMeans(pred)
test_prediction <- as.integer(fin_pred >= 0.5)

confusionMatrix(test$Diabetes,test_prediction)

acc_bayes <- confusionMatrix(test$Diabetes,test_prediction)$overall['Accuracy']

plot.roc(test$Diabetes,fin_pred,percent=TRUE,col="#1c61b6", print.auc=TRUE,
         main = "Area under the curve for Bayesian Logistic Regression")


########################################################################
########################################################################
#Decision Tress






set.seed(15689)
m_dt <- tree(Diabetes ~ ., data = train)
pred_dt <- predict(m_dt, train, type = "class")
confusionMatrix(train$Diabetes,pred_dt)[2:3]
plot(m_dt)
text(m_dt, pretty = 0)
pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$Diabetes,pred_dt_test)


acc_dt <- confusionMatrix(pred_dt_test,test$Diabetes)$overall['Accuracy']


########################################################################
########################################################################
#Random Forest

#set.seed(15689)

#opt_mod <- tuneRF(train[-as.numeric(ncol(train))],train$Diabetes,ntreeTry = 150, 
#                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)

#mtry_fin <- opt_mod[as.numeric(which.min(opt_mod[,"OOBError"])),"mtry"]

#rf_fin <- randomForest(Diabetes~.,data=train, mtry=mtry_fin, ntree=101, 
#                       keep.forest=TRUE, proximity=TRUE, importance=TRUE,test=test)

#pred_test <- predict(rf_fin, newdata = test)
#confusionMatrix(test$Diabetes,pred_test)

#acc_rf <- confusionMatrix(test$Diabetes,pred_test)$overall['Accuracy']

#par(mfrow=c(1,2))
#varImpPlot(rf_fin,type = 2,main = "Variable Importance",col = 'black')
#plot(rf_fin,main = "Error vs no. of trees grown")




set.seed(42)
model_rf <- caret::train(Diabetes ~ .,
                         data = train,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 20, 
                                                  repeats = 20, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))
  
model_rf$finalModel$confusion
imp <- model_rf$finalModel$importance
imp[order(imp, decreasing = TRUE), ]
importance <- varImp(model_rf, scale = TRUE)
plot(importance)
confusionMatrix(predict(model_rf, test), test$Diabetes)
acc_rf <- confusionMatrix(predict(model_rf, test), test$Diabetes)$overall['Accuracy']


########################################################################
########################################################################
#Extreme gradient boosting


set.seed(42)
model_xgb <- caret::train(Diabetes ~ .,
                          data = train,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 20, 
                                                   repeats = 20, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE))

importance <- varImp(model_xgb, scale = TRUE)
plot(importance)
confusionMatrix(predict(model_xgb, test), test$Diabetes)
acc_xbg <- confusionMatrix(predict(model_xgb, test), test$Diabetes)$overall['Accuracy']


########################################################################
########################################################################
#Genetic

set.seed(27)
model_ga <- gafs(x = train[, -1], 
                 y = train$Diabetes,
                 iters = 10, # generations of algorithm
                 popSize = 10, # population size for each generation
                 levels = c("NO", "Yes"),
                 gafsControl = gafsControl(functions = rfGA, # Assess fitness with RF
                                           method = "cv",    # 10 fold cross validation
                                           genParallel = TRUE, # Use parallel programming
                                           allowParallel = TRUE))

plot(model_ga) # Plot mean fitness (AUC) by generation
train_ga <- train[, c(1, which(colnames(train) %in% model_ga$ga$final))]

confusionMatrix(predict(train_ga, test), test$Diabetes)
acc_ga <- confusionMatrix(predict(model_ga, test), test$Diabetes)$overall['Accuracy']

a = predict(model_ga, test, type = 'class')
b = test_data$Class
xtab<- table(a,b)
library(caret)
confusionMatrix(xtab)


########################################################################
########################################################################


accuracy <- data.frame(Model=c("Logistic","Bayesian Logistic","Decision Tree",
                               "Random Forest","Boosting"),
                       Accuracy=c(acc_lg,acc_bayes,acc_dt,acc_rf,acc_xbg))

ggplot(accuracy,aes(x=Model,y=Accuracy))+geom_bar(stat='identity')+theme_bw()+
  ggtitle('Comparison of Model Accuracy')


########################################################################
########################################################################











