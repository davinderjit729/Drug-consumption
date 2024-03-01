################################# environment setup #############################

# import libraries
# check if libraries are present. If not, install libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("rsample")) install.packages("rsample")
if (!require("modeldata")) install.packages("modeldata")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plyr")) install.packages("plyr")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("ada")) install.packages("ada")

# This package will not work as it is removed from CRAN. Install an older 
# version
if (!require("DMwR")) install.packages("DMwR")

library(tidyverse)
library(caret)
library(rsample)
library(modeldata)
library(dplyr)
library(plyr)
library(ada)
library(rpart.plot)



drug_consumption_dataset<- read.csv("C:/Users/user/Downloads/drug_consumption.data", header=FALSE, sep= ",")
head(drug_consumption_dataset)

# have a quick look at the data
head(drug_consumption_dataset)

# rename the columns
names(drug_consumption_dataset) <- c("ID" ,
                                     "Age" ,
                                     "Gender" ,
                                     "Education" ,
                                     "Country" ,
                                     "Ethnicity" ,
                                     "Nscore" ,
                                     "Escore" ,
                                     "Oscore" ,
                                     "Ascore" ,
                                     "Cscore" ,
                                     "Impulsive" ,
                                     "SS" ,
                                     "Alcohol" ,
                                     "Amphet" ,
                                     "Amyl" ,
                                     "Benzos" ,
                                     "Caff" ,
                                     "Cannabis" ,
                                     "Choc" ,
                                     "Coke" ,
                                     "Crack" ,
                                     "Ecstasy" ,
                                     "Heroin" ,
                                     "Ketamine" ,
                                     "Legalh" ,
                                     "LSD" ,
                                     "Meth" ,
                                     "Mushrooms" ,
                                     "Nicotine" ,
                                     "Semer" ,
                                     "VSA"
)

# check the dataset information
str(drug_consumption_dataset)

# check the summary of the dataset
summary(drug_consumption_dataset)

############################# data preprocessing and EDA #######################
# Now checking the dataset information by column-wise
# In general the data seems to be normalized/standardized when looking at the 
# values

# checking for any missing/null values
colSums(is.na(drug_consumption_dataset))
# there are no visible null values in the dataset

# plot the histograms to see the distribution of data

# Age
unique(drug_consumption_dataset[2])

ggplot(
  drug_consumption_dataset, 
  aes(x = Age)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 6
    ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[2]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[2]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Gender
unique(drug_consumption_dataset[3])

ggplot(
  drug_consumption_dataset, 
  aes(x = Gender)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 2
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[3]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[3]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Education
unique(drug_consumption_dataset[4])

ggplot(
  drug_consumption_dataset, 
  aes(x = Education)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 9
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[4]), 
      " in the drug dataset"),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[4]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Country
unique(drug_consumption_dataset[5])

ggplot(
  drug_consumption_dataset, 
  aes(x = Country)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 7
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[5]), 
      " in the drug dataset"),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[5]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Ethnicity
unique(drug_consumption_dataset[6])

ggplot(
  drug_consumption_dataset, 
  aes(x = Ethnicity)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 7
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[6]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[6]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# NScore
ggplot(
  drug_consumption_dataset, 
  aes(x = Nscore)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 20
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[7]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[7]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Escore
ggplot(
  drug_consumption_dataset, 
  aes(x = Escore)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 20
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[8]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[8]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Oscore
ggplot(
  drug_consumption_dataset, 
  aes(x = Oscore)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 20
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[9]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[9]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Ascore
ggplot(
  drug_consumption_dataset, 
  aes(x = Ascore)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 20
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[10]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[10]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Cscore
ggplot(
  drug_consumption_dataset, 
  aes(x = Cscore)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 20
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[11]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[11]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Impulsive
ggplot(
  drug_consumption_dataset, 
  aes(x = Impulsive)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 10
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[12]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[12]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# SS
ggplot(
  drug_consumption_dataset, 
  aes(x = SS)) +
  geom_histogram(
    color = "#a8a832", 
    fill = "#a8a832", 
    bins = 10
  ) +
  labs(
    title = paste0(
      "Histogram of ", 
      colnames(drug_consumption_dataset[13]), 
      " in the drug dataset"
      ),
    caption = "Source: archive.ics.uci.edu",
    x = colnames(drug_consumption_dataset[13]),
    y = "Count"
  ) + 
  theme(
    panel.background = element_rect(
      fill = "#000000", 
      colour = "#000000", 
      linetype = "solid"
      ),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# convert the dependent variable columns (temp) based on values into 1 or 0
drug_consumption_dataset$Alcohol <- ifelse(
  drug_consumption_dataset$Alcohol %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Amphet <- ifelse(
  drug_consumption_dataset$Amphet %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Amyl <- ifelse(
  drug_consumption_dataset$Amyl %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Benzos <- ifelse(
  drug_consumption_dataset$Benzos %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Caff <- ifelse(
  drug_consumption_dataset$Caff %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Cannabis <- ifelse(
  drug_consumption_dataset$Cannabis %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Choc <- ifelse(
  drug_consumption_dataset$Choc %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Coke <- ifelse(
  drug_consumption_dataset$Coke %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Crack <- ifelse(
  drug_consumption_dataset$Crack %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Ecstasy <- ifelse(
  drug_consumption_dataset$Ecstasy %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Heroin <- ifelse(
  drug_consumption_dataset$Heroin %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Ketamine <- ifelse(
  drug_consumption_dataset$Ketamine %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Legalh <- ifelse(
  drug_consumption_dataset$Legalh %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$LSD <- ifelse(
  drug_consumption_dataset$LSD %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Meth <- ifelse(
  drug_consumption_dataset$Meth %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Mushrooms <- ifelse(
  drug_consumption_dataset$Mushrooms %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Nicotine <- ifelse(
  drug_consumption_dataset$Nicotine %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$Semer <- ifelse(
  drug_consumption_dataset$Semer %in% c("CL0" , "CL1", "CL2"), 0, 1)
drug_consumption_dataset$VSA <- ifelse(
  drug_consumption_dataset$VSA %in% c("CL0" , "CL1", "CL2"), 0, 1)

# check if one is addicted to more than 
drug_consumption_dataset$total_drugs_addiction <- rowSums(
  drug_consumption_dataset[14:32], 
  na.rm = TRUE
  )
table(drug_consumption_dataset$total_drugs_addiction)

# create a dependent variable based on the drug usage frequency
drug_consumption_dataset$total_drugs_addiction <- as.factor(
  ifelse(
    drug_consumption_dataset$total_drugs_addiction %in% c(0,1,2,3), 
    "no.addiction", 
    "addiction"
    )
  )

table(drug_consumption_dataset$total_drugs_addiction)

# Only subset data that needs to be used
colnames(drug_consumption_dataset)

drug_consumption_dataset_subset <- 
  drug_consumption_dataset[,
                           c(
                             "ID",
                             "Age",
                             "Gender",
                             "Education",
                             "Country",
                             "Ethnicity",
                             "Nscore",
                             "Escore",
                             "Oscore",
                             "Ascore",
                             "Cscore",
                             "Impulsive",
                             "SS",
                            "total_drugs_addiction"
                             )]

# box plot to see distribution
# set the margins
par(mfrow = c(3,3))

for (i in colnames(drug_consumption_dataset_subset[c(7:13)])){
  boxplot(
    drug_consumption_dataset_subset[i], 
    horizontal = TRUE,
    xlab = colnames(drug_consumption_dataset_subset[i]), 
    ylab = "values", 
    main = paste0("Boxplot for ", colnames(drug_consumption_dataset_subset[i]))
  )
}

# Correct outliers with outlier detection 1.5 * IQR
# find the IQR

colnames(drug_consumption_dataset_subset)
column_name_IQR <- c(
  "Nscore",
  "Escore",
  "Oscore",
  "Ascore",
  "Cscore",
  "Impulsive",
  "SS"
)

iqr_and_replace <- function(column_name) {
  
  q1 <- unname(quantile(column_name, probs = c(.25), na.rm = TRUE))
  q3 <- unname(quantile(column_name, probs = c(.75), na.rm = TRUE))
  
  IQR <- q3 - q1
  
  outlier_value <- 1.5 * IQR
  
  lower_bound <- q1 - outlier_value
  upper_bound <- q3 + outlier_value
  
  return(c("lower_bound" = lower_bound, "upper_bound" = upper_bound))
}

for (i in column_name_IQR) {
  outlier_value <- iqr_and_replace(drug_consumption_dataset_subset[, eval(i)])
  lower_bound <- unname(outlier_value["lower_bound"])
  upper_bound <- unname(outlier_value["upper_bound"])
  
  drug_consumption_dataset_subset[,eval(i)][
    drug_consumption_dataset_subset[, eval(i)] < lower_bound] <- 
    lower_bound
  
  drug_consumption_dataset_subset[,eval(i)][
    drug_consumption_dataset_subset[, eval(i)] > upper_bound] <- 
    upper_bound
}

# check the results after the replacement
par(mfrow = c(3,3))
for (i in colnames(drug_consumption_dataset_subset[c(7:13)])){
  boxplot(
    drug_consumption_dataset_subset[i], 
    horizontal = TRUE,
    xlab = colnames(drug_consumption_dataset_subset[i]), 
    ylab = "values", 
    main = paste0("Boxplot for ", colnames(drug_consumption_dataset_subset[i]))
  )
}

################################## modelling ###################################
# split the data into train and test datasets
# follow the 80-20 rule
set.seed(666)  

drug_consumption_dataset_subset_smote <- subset(
  drug_consumption_dataset_subset, 
  select = -c(ID)
  )

# use smote to oversample and make sure the ratios of positive and negative
# classes are going to be the same
#drug_consumption_dataset_subset <- 
  #SMOTE(total_drugs_addiction~., data = drug_consumption_dataset_subset_smote,
    #    perc.over = 100, k=5)  

table(drug_consumption_dataset_subset$total_drugs_addiction)

library(caret)
training_index<-createDataPartition(
  y=drug_consumption_dataset_subset$total_drugs_addiction,
  p=0.8,
  list=FALSE
  )
training_set <- drug_consumption_dataset_subset[training_index,]
testing_set <- drug_consumption_dataset_subset[-training_index,]

# Create training set
training_set <- as.data.frame(drug_consumption_dataset_subset[training_index, ])

# Create testing set
testing_set <- as.data.frame(drug_consumption_dataset_subset[-training_index, ])
y_actual <- testing_set$total_drugs_addiction
testing_set <- subset(testing_set, select = -c(total_drugs_addiction) )

################################################################################
str(training_set)

################################################################################
#KNN Model
# model building
knn_model <- train(
  total_drugs_addiction ~ .,
  data = drug_consumption_dataset_subset,
  method = "knn",
  trControl = trainControl(
    method = "cv", number = 10)
)

# model summary
knn_model

# model predictions
knn_predictions <- predict(knn_model, newdata = testing_set)
knn_predictions

# confusion matrix
knn_confusion_matrix <- confusionMatrix(
  as.factor(knn_predictions), 
  as.factor(y_actual))
knn_confusion_matrix

# model accuracy
knn_accuracy <- mean(knn_predictions == y_actual)
knn_accuracy

################################################################################
#Decision tree model
# model building
decision_tree_model <- train(
  total_drugs_addiction ~ .,
  data = training_set,
  method = "rpart"
)

# model summary
decision_tree_model

# tree visualization
par(mfrow = c(1,1))
rpart.plot(decision_tree_model$finalModel)

# model prediction
decision_tree_predictions <- predict(decision_tree_model, newdata = testing_set)
decision_tree_predictions

# confusion matrix
decision_tree_confusionmatrix <- confusionMatrix(
  as.factor(decision_tree_predictions), 
  as.factor(y_actual)
  )
decision_tree_confusionmatrix

# model accuracy
decision_tree_accuracy <- mean(decision_tree_predictions == y_actual)
decision_tree_accuracy

################################################################################
#Random forest model
# model building
random_forest_model <- train(total_drugs_addiction ~ .,
                             data = training_set,
                             method = "ranger",
                             trControl = trainControl(method = "cv",
                             number = 5,
                             classProbs = TRUE
                             ),
                             num.trees = 200,
                             importance = "impurity"
                             )

# model summary
random_forest_model

# model visualization
plot(varImp(random_forest_model))

# model prediction
random_forest_predictions <- predict(random_forest_model, newdata = testing_set)
random_forest_predictions

# confusion matrix
random_forest_confusionmatrix <- confusionMatrix(
  as.factor(random_forest_predictions), 
  as.factor(y_actual)
  )
random_forest_confusionmatrix

# model accuracy
random_forest_accuracy <- mean(random_forest_predictions == y_actual)
random_forest_accuracy

################################################################################
#Boosting model
# model building
adaboost_model <- 
  train(
    total_drugs_addiction ~ .,
    data = training_set,
    method = "ada",
    trControl = trainControl(method = "cv",
    number = 5,
    classProbs = TRUE
    )
)

# model summary
adaboost_model

# model prediction
adaboost_model_predictions <- predict(adaboost_model, newdata = testing_set)
adaboost_model_predictions

# confusion matrix
adaboost_model_confusionmatrix <- confusionMatrix(
  as.factor(adaboost_model_predictions), 
  as.factor(y_actual)
  )
adaboost_model_confusionmatrix

# model accuracy
adaboost_model_accuracy <- mean(adaboost_model_predictions == y_actual)
adaboost_model_accuracy
################################################################################