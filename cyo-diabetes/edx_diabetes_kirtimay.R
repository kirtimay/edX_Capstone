## Kirtimay Pendse
## Capstone Project Submission
## HarvardX: PH125.9x Data Science: Capstone
## https://github.com/kirtimay/edX_Capstone/tree/master/cyo-diabetes

################################
# Preprocessing
################################

#Loading required packages
library(lubridate)
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(knitr)
library(ggplot2)
library(dslabs)
library(lubridate)
library(corrplot)
library(readr)

#Downloading the data
dl <- tempfile()
download.file("https://github.com/kirtimay/edX_Capstone/blob/master/cyo-diabetes/diabetes.csv", dl)
pima_diabetes <- read.csv("diabetes.csv",
                          col.names=c("pregnancies","glucose","bp","skin_thickness","insulin","bmi","dpf","age","outcome"))

#convert outcome to factor
pima_diabetes$outcome <- factor(pima_diabetes$outcome)

################################
#Description of Variables
################################

v_type <- lapply(pima_diabetes, class)
v_desc <- c("No. of Pregnancies", "Plasma Glucose Concentration (mg/dL)", "Diastolic BP (mm Hg)", "Triceps Skin Thickness (mm)", "2 Hour Serum Insulin (uU/mL)", "Body Mass Index", "Diabetes Pedigree Function", "Age in Years", "Presence of Diabetes")
v_name <- colnames(pima_diabetes)
desc_table <- as_data_frame(cbind(v_name, v_type, v_desc))
colnames(desc_table) <- c("Variable","Class","Description")
desc_table 

################################
#Create test and train sets
################################

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = pima_diabetes$outcome, times = 1, p = 0.2, list = FALSE)
train_set <- pima_diabetes[-test_index,]
test_set <- pima_diabetes[test_index,]

################################
#Exploratory Analysis
################################

head(pima_diabetes) 
summary(pima_diabetes)


# Missing Data 


sapply(pima_diabetes, function(x) sum(is.na(x))) #seems as if there is no missing data

#but lets take a deeper look
pima_miss <- pima_diabetes[,setdiff(names(pima_diabetes), c('outcome', 'pregnancies'))]
num_miss_features <- apply(pima_miss, 2, function(x) sum(x <= 0))
miss_features <- names(pima_miss)[ num_miss_features > 0]

missing_rows <- apply(pima_miss, 1, function(x) sum(x <= 0) >= 1) 
sum(missing_rows)

pima_miss[pima_miss <= 0] <- NA
pima_diabetes[, names(pima_miss)] <- pima_miss

data <- pima_diabetes
colSums(is.na(pima_diabetes)) #a lot of missing data

#using knnimputation
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
library(DMwR)
pima_diabetes[,c(-8,-9)] <- knnImputation(pima_diabetes[,c(-8,-9)], k = 5)

colSums(is.na(pima_diabetes)) #check if missing data has been accounted for


# Plots


p1 <- ggplot(pima_diabetes, aes(x = outcome, y = pregnancies, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in Pregnancies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("No. of Pregnancies") +
  theme(legend.position = "bottom") 

p2 <- ggplot(pima_diabetes,aes(x = pregnancies, fill=outcome)) + 
  geom_bar(position = "Dodge") + 
  scale_x_continuous(limits = c(0,17)) +
  labs(title = "No. of Pregnancies In Diabetics") +  
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("No. of Pregnancies") +
  ylab("Count") +
  theme(legend.position = "bottom") 

p3 <- ggplot(pima_diabetes, aes(x = outcome, y=glucose, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in Glucose Levels") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("Glucose Level") +
  theme(legend.position = "bottom") 

p4 <- ggplot(pima_diabetes, aes(x = glucose, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "Glucose Level", y = "Density", title = "Density Plot of Glucose Levels")

p5 <- ggplot(pima_diabetes, aes(x = outcome, y=bp, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in BP Level") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("Blood Pressure") +
  theme(legend.position = "bottom") 

p6 <- ggplot(pima_diabetes, aes(x = bp, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "Diastolic Blood Pressure", y = "Density", title = "Density Plot of BP Levels")

p7 <- ggplot(pima_diabetes, aes(x = outcome, y=skin_thickness, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in Skin Thickness") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("Skin Thickness") +
  theme(legend.position = "bottom") 

p8 <- ggplot(pima_diabetes, aes(x = skin_thickness, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "Skin Thickness", y = "Density", title = "Density Plot of Skin Thickness")

p9 <- ggplot(pima_diabetes, aes(x = outcome, y=insulin, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in Insulin Level") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("Insulin Level") +
  theme(legend.position = "bottom") 

p10 <- ggplot(pima_diabetes, aes(x = insulin, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "Insulin Level", y = "Density", title = "Density Plot of Insulin Level")

p11 <- ggplot(pima_diabetes, aes(x = outcome, y=bmi, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in BMI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("BMI") +
  theme(legend.position = "bottom") 

p12 <- ggplot(pima_diabetes, aes(x = bmi, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "BMI", y = "Density", title = "Density Plot of BMI")

p13 <- ggplot(pima_diabetes, aes(x = outcome, y=dpf, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in DPF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("DPF") +
  theme(legend.position = "bottom") 

p14 <- ggplot(pima_diabetes, aes(x = dpf, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "DPF", y = "Density", title = "Density Plot of DPF")

p15 <- ggplot(pima_diabetes, aes(x = outcome, y=age, color=outcome)) +
  geom_boxplot() +
  ggtitle("Difference in Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Presence of Diabetes") +
  ylab("Age") +
  theme(legend.position = "bottom") 

p16 <- ggplot(pima_diabetes, aes(x = age, color = outcome, fill = outcome)) +
  geom_density(alpha = 0.4) +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Density", title = "Density Plot of Age")


# Correlation Matrix


corrplot(cor(pima_diabetes[, -9]), type = "upper", method = "number")
pairs(pima_diabetes, panel = panel.smooth)


################################
# Modeling 
################################


## Guessing Randomly

set.seed(123, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$outcome)

## Logistic Regression

fit_glm <- glm(outcome~.,data=train_set,family = binomial)
summary(fit_glm)


fit_glm2 <- glm(formula = outcome ~ pregnancies + glucose + bmi + dpf, 
                family = binomial, 
                data = train_set)
summary(fit_glm2)


pred_glm <- predict(fit_glm2, type="response") #predict on train set
tapply(pred_glm, train_set$outcome, mean)

#making ROC

if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
library(ROCR)
roc_pred <- prediction(pred_glm, train_set$outcome)
roc_perf <- performance(roc_pred, "tpr", "fpr") 

plot(roc_perf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
abline(a=0, b=1)

train_auc <- round(as.numeric(performance(roc_pred,"auc")@y.values),2)
legend(0.8,0.2,train_auc, title="auc", cex=1)

#testing on test set

test_pred <- predict(fit_glm2, type="response", newdata = test_set)
test_table <- table(test_set$outcome, test_pred >0.5)
test_table 

#report accuracy
test_accuracy <- round(sum(diag(test_table))/sum(test_table),2)
print(test_accuracy)
      
      
## Random Forest
      

library(randomForest)
set.seed(123, sample.kind = "Rounding") 
fit_rf_tuning <- tuneRF(x = subset(train_set, select = -outcome),
                        y = train_set$outcome,
                        ntreeTry = 500,
                        plot = TRUE, 
                        tunecontrol = tune.control(cross = 5))


set.seed(123, sample.kind = "Rounding") 
fit_rf <- randomForest(outcome~., data = train_set, importance = TRUE, mtry = 2)
fit_rf


par(mfrow = c(1,2))
train_rf_pred <- predict(fit_rf, train_set)
rf_pred <- predict(fit_rf, train_set,type="prob")
confusionMatrix(train_set$outcome, train_rf_pred)


#make ROC plot

if(!require(verification)) install.packages("verification", repos = "http://cran.us.r-project.org")
library(verification)
roc.plot(train_set$outcome == "1", rf_pred[,2], ylab = "True Positives", xlab = "False Positives")$roc.vol

#test on test set

test_rf_pred <- predict(fit_rf, test_set)
rf_pred2 <- predict(fit_rf, test_set,type="prob")
confusionMatrix(test_set$outcome, test_rf_pred)

roc.plot(test_set$outcome== "1", rf_pred2[,2], ylab = "True Positives", xlab = "False Positives")$roc.vol



## Comparison

#2 in 1 ROC plot
rocplot <- roc.plot(x = (test_set$outcome == "1"), 
                    pred = cbind(test_pred, rf_pred2[,2]),
                    main = "ROC Comparison",
                    legend = T, leg.text = c("glm", "rf"))


################################
# Appendix 
################################

print("Operating System:")
version

