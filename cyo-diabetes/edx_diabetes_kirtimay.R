## Kirtimay Pendse
## Capstone Project Submission
## HarvardX: PH125.9x Data Science: Capstone
## https://github.com/kirtimay/edX_Capstone/

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
pima_diabetes <- read.csv("diabetes.csv", col.names=c("pregnancies","glucose","bp","skin_thickness","insulin","bmi","dpf","age","outcome"))

#convert outcome to factor
pima_diabetes$outcome <- factor(pima_diabetes$outcome)

v_type <- lapply(pima_diabetes, class)
v_desc <- c("No. of Pregnancies", "Plasma Glucose Concentration (mg/dL)", "Diastolic BP (mm Hg)", "Triceps Skin Thickness (mm)", "2 Hour Serum Insulin (uU/mL)", "Body Mass Index", "Diabetes Pedigree Function", "Age in Years", "Presence of Diabetes")
v_name <- colnames(pima_diabetes)
desc_table <- as_data_frame(cbind(v_name, v_type, v_desc))
colnames(desc_table) <- c("Variable","Class","Description")
desc_table 

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = pima_diabetes$outcome, times = 1, p = 0.2, list = FALSE)
train_set <- pima_diabetes[-test_index,]
test_set <- pima_diabetes[test_index,]

head(pima_diabetes) 

summary(pima_diabetes) 

sapply(pima_diabetes, function(x) sum(is.na(x))) 

pima_miss <- pima_diabetes[,setdiff(names(pima_diabetes), c('outcome', 'pregnancies'))]
num_miss_features <- apply(pima_miss, 2, function(x) sum(x <= 0))
miss_features <- names(pima_miss)[ num_miss_features > 0]

missing_rows <- apply(pima_miss, 1, function(x) sum(x <= 0) >= 1) 
sum(missing_rows)

pima_miss[pima_miss <= 0] <- NA
pima_diabetes[, names(pima_miss)] <- pima_miss

data <- pima_diabetes
colSums(is.na(pima_diabetes))

# KNN imputation
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
library(DMwR)
pima_diabetes[,c(-8,-9)] <- knnImputation(pima_diabetes[,c(-8,-9)], k = 5)

colSums(is.na(pima_diabetes))


## Plots

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

gridExtra::grid.arrange(p1, p2, ncol = 2)

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

gridExtra::grid.arrange(p3, p4, ncol = 2)

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

gridExtra::grid.arrange(p5, p6, ncol = 2)

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

gridExtra::grid.arrange(p7, p8, ncol = 2)

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

gridExtra::grid.arrange(p9, p10, ncol = 2)

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

gridExtra::grid.arrange(p11, p12, ncol = 2)

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

gridExtra::grid.arrange(p13, p14, ncol = 2)

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

gridExtra::grid.arrange(p15, p16, ncol = 2)

# Corr

corr_mat <- round(cor(pima_diabetes[1:8]),2)
corr_mat %>% knitr::kable()
#To better visually see the highest correlations
corrplot(cor(pima_diabetes[, -9]), type = "lower", method = "number")

pairs(pima_diabetes, panel = panel.smooth)


## Modeling


