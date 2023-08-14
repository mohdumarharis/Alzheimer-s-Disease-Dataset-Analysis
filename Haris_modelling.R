library(dplyr)
library(psych)
library(knitr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(tidyr)
library(reshape2)
library(corrplot)
library(factoextra)
library(cluster)
library(caret)
library(randomForest)
library(MASS)

Alzheimer_data <- read.csv("/Users/haris/Downloads/project data.csv")

dim(Alzheimer_data) # checking dimentions 

# Cleaning the data

#Checking if there is any N/A values in the data 
any(is.na(Alzheimer_data))

# count of N/A values
na_count <- sum(is.na(Alzheimer_data))

# removing N/A values
Alzheimer_data <-na.omit(Alzheimer_data)

summary(Alzheimer_data)
dim(Alzheimer_data) # Again checking dimentions 

# Changing column name from M.F to Gender
Alzheimer_data <- Alzheimer_data %>%
  rename(Gender = M.F)

# changing gender into numericals
Alzheimer_data <- Alzheimer_data %>% mutate(Gender = ifelse(Gender == "M", 1, 0))

# removing converted from group
Alzheimer_data <- subset(Alzheimer_data, Group != "Converted")

dim(Alzheimer_data) # Again checking dimentions 

# give each row a unique id 

Alzheimer_data <- Alzheimer_data %>% mutate(ID = row_number())
Alzheimer_data <- Alzheimer_data[, c(ncol(Alzheimer_data), 1:(ncol(Alzheimer_data)-1))]

# Creating a new column for Group as numeric
Alzheimer_data <- Alzheimer_data %>% mutate(Group_numeric=ifelse(Group=="Demented",1,0))

head(Alzheimer_data)

# Explaining the data

# Selecting the relevant variables for the table
variables <- c("Group_numeric", "Gender", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")

# Extracting the subset of data for selected variables
subset_data <- Alzheimer_data[, variables]

# Generating the table with numerical explanations
table_explanation <- describe(subset_data)

# Convert the table to a formatted table using kable()
formatted_table <- kable(round(table_explanation,2))

# Calculate the percentages
df_percent <- Alzheimer_data %>%
  group_by(Gender, Group) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Recode Gender values for labeling
df_percent$Gender <- factor(df_percent$Gender, labels = c("Female", "Male"))

# Create a bar graph for dementia (demented or non-demented) based on gender
ggplot(df_percent, aes(x = Gender, y = Percentage, fill = Group)) +
  geom_bar(position = "dodge",stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = 2, size = 3) +
  labs(title = "Dementia Distribution by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Group") +
  scale_fill_manual(values = c("red", "darkgreen")) +
  theme_minimal()

# Distribution of Age
ggplot(Alzheimer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Distribution of Age
ggplot(Alzheimer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  scale_x_continuous(breaks = seq(min(Alzheimer_data$Age), max(Alzheimer_data$Age), by = 5),
                     labels = seq(min(Alzheimer_data$Age), max(Alzheimer_data$Age), by = 5)) +
  theme_minimal()

# Correlation graph
correlation_matrix <- cor(Alzheimer_data[, c('Gender',"Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF","Group_numeric")])

# Round the values in the correlation matrix
rounded_correlation <- round(correlation_matrix, digits = 2)

# Create the correlation plot
corrplot(rounded_correlation, method = "circle", type = "full", tl.col = "black")


ggplot(Alzheimer_data, aes(Group, MMSE)) +
  geom_boxplot() +
  xlab("Group") +
  ylab("MMSE") +
  ggtitle("Distribution of MMSE by Group")


#-------------------------------------------------

# Histogram of Educational Years (EDUC)
ggplot(Alzheimer_data, aes(x = EDUC)) +
  geom_histogram(binwidth = 1, fill = "#56B4E9", color = "black") +
  labs(title = "Educational Years Distribution",
       x = "Educational Years",
       y = "Count") +
  theme_minimal()

#-----------------------------------------------



ggplot(Alzheimer_data, aes(x = EDUC, y = SES, color = factor(Group_numeric))) +
  geom_point() +
  labs(title = "EDUC vs SES by Dementia Status",
       x = "EDUC",
       y = "SES",
       color = "Dementia Status") +
  theme_minimal()


# Cross-tabulation table: Group by Gender
cross_table <- table(Alzheimer_data$Group, Alzheimer_data$Gender)
cross_table

# Chi-square test of independence between Group and Gender
chi_test <- chisq.test(cross_table)
chi_test
#---------------------------------------------------
#----------------------------------------------------
# Q2
set.seed(111)
# Selecting the relevant variables for clustering
clustering_data <- Alzheimer_data[, c("Gender","Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF","Group_numeric")]

# Scaling the variables for clustering
scaled_data <- scale(clustering_data)

# Performing k-means clustering
kmeans_results_3 <- kmeans(scaled_data, centers = 3)

# Extracting cluster assignments
cluster_assignments_3 <- kmeans_results_3$cluster

fviz_cluster(kmeans_results_3, data = scaled_data )

# Performing k-means clustering for 4
kmeans_results_4 <- kmeans(scaled_data, centers = 4)

# Extracting cluster assignments
cluster_assignments_4 <- kmeans_results_4$cluster

fviz_cluster(kmeans_results_4, data = scaled_data )


# Calculate the optimal number of clusters using the NbClust function
nb_clusters <- fviz_nbclust(scaled_data, kmeans, method = "wss")

optimal_k <-3 #from the graph
# Apply k-means clustering
kmeans_model <- kmeans(scaled_data, centers = optimal_k, nstart = 25)

# Add the cluster labels to the original dataframe
Alzheimer_data$cluster <- as.factor(kmeans_model$cluster)

## Another clustering method

# Calculate the dissimilarity matrix using Euclidean distance
dist_matrix <- dist(clustering_data, method = "euclidean")

# Perform hierarchical agglomerative clustering
hierarchical_single <- hclust(dist_matrix, method="single")
hierarchical_complete <- hclust(dist_matrix, method="complete")
hierarchical_average <- hclust(dist_matrix, method="average")
hierarchical_centroid <-hclust(dist_matrix, method="centroid")


# Cut the dendrogram to obtain a specified number of clusters
optimal_k <- 4  
hierchiel_clusters <- cutree(hclust_model, k = optimal_k)

# Add the cluster labels to the original dataframe
Alzheimer_data$hierchial_cluster <- as.factor(hierchiel_clusters)

table(hierchiel_clusters)

plot(hierarchical_complete)
rect.hclust(hierarchical_complete, k=6, border="red") 

hierarchical_ward <- hclust(dist_matrix, method = "ward.D2")
plot(hierarchical_ward)
rect.hclust(hierarchical_ward, k=6, border="red") 

#----------------------------------------------------------------
#feature selection

library(caret)
library(randomForest)
library(MASS)

# Selecting the relevant variables for feature selection
feature_data <- Alzheimer_data[, c("Gender","Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]

# Defining the target variable (Group)
target <- cbind(Alzheimer_data$Group_numeric)

# Performing Recursive Feature Elimination (RFE)
set.seed(10)
ctrl <- rfeControl(functions = lmFuncs, method = "repeatedcv",repeats = 10, number = 10)  # Using random forest as the algorithm
results <- rfe(feature_data, target, sizes = c(1:8), rfeControl = ctrl)

# Extracting the most important features
important_features <- results$optVariables
print(important_features)

# Visualizing the feature importance
plot(results, type = c("g", "o"))

#-------------------------------------------------

####### Logic regression ------------------

########### splitting before logic regression
# Selecting the relevant variables for the logistic regression model
feature_data <- Alzheimer_data[, c("Gender","Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]

# Defining the target variable (Group)
target <- Alzheimer_data$Group_numeric

# Setting the seed for reproducibility
set.seed(76)

# Splitting the data into training and testing sets
train_indices <- sample(1:nrow(data), round(0.7 * nrow(data)))  # 70% for training
train_data <- feature_data[train_indices, ]
train_target <- target[train_indices]

test_data <- feature_data[-train_indices, ]
test_target <- target[-train_indices]

# Fitting a logistic regression model on the training data
model <- glm(train_target ~ ., data = train_data, family = binomial)

# Predicting on the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Converting predicted probabilities to class labels (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculating accuracy
accuracy <- sum(predicted_classes == test_target) / length(test_target)
print(paste("Accuracy:", accuracy))

# Accuracy is "Accuracy: 0.978947368421053"

#### Performing on selected features from previous question

# Selecting the relevant variables for the logistic regression model
feature_data <- Alzheimer_data[, c("Gender", "CDR", "nWBV", "ASF")]

# Defining the target variable (Group)
target <- Alzheimer_data$Group_numeric

# Setting the seed for reproducibility
set.seed(75)

# Splitting the data into training and testing sets
train_indices <- sample(1:nrow(data), round(0.7 * nrow(data)))  # 70% for training
train_data <- feature_data[train_indices, ]
train_target <- target[train_indices]

test_data <- feature_data[-train_indices, ]
test_target <- target[-train_indices]

# Fitting a logistic regression model on the training data
model <- glm(train_target ~ ., data = train_data, family = binomial)

# Predicting on the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Converting predicted probabilities to class labels (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculating accuracy
accuracy <- sum(predicted_classes == test_target) / length(test_target)
print(paste("Accuracy:", accuracy))

# Accuracy is "Accuracy: 0.989473684210526"



