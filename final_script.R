#Loading libraries
require (jsonlite)
require(dplyr)
require(stringr)
require(tidyverse)
require(lubridate)
require(ggplot2)
require(magrittr)
require(randomForest)
require(rpart)
require(ipred)
require(caret)
require(tidyr)
require(viridis)
require(scales)

##NOTE: I chose to create new dataframes when any modification is made so many fairly similar dataframes are generated throughout this script
##Sentiment Analysis: This takes ~40 mins to obtain sentiment scores
## MODELS: The models I chose to run have a large number of trees and take ~1 hour to run, I have included the other models I tried in the script but this have not ran all the models in the same script as it would be very computationally expensive

#Clear
cat("\014")
rm(list=ls())

#Set directory
setwd("C:/Users/u2105029/OneDrive - University of Warwick/Data Science")

#Loading datasets
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
user_data <- stream_in(file("yelp_academic_dataset_user.json")) 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json"))  
load(file='yelp_review_small.rda')

#Merge datasets (small reviews with full user datasets)
userandreview <- merge (x= review_data_small, y= user_data, by= "user_id", all.x =TRUE)
#save (userandreview, file= "initialmerge")

#Too many observations and file was too large so I randomly selected 700000 observations but this was still too large so I cut to 500000
set.seed(1)
merged_data_small <- sample_n(userandreview, size = 700000)
set.seed(2)
merged_data_small1 <- sample_n (userandreview, size= 500000)

#Merging this dataset with the information on state, stars and review count of the business dataset
business_condensed = subset(business_data, select = c(business_id, state, stars, review_count))
business_review_user <- merge (x= merged_data_small1, y= business_condensed, by= "business_id", all.x =TRUE)

#Checking for NA values but only find 3 NA values so I have chosen to drop these
na_count <- colSums(is.na(business_review_user))
print (na_count)
merged_na <- na.omit(business_review_user)

#Converting variables so they are read correctly by R 
#Stars to categorical
merged_na$stars.x <- factor(merged_na$stars.x)
summary(merged_na$stars.x)

#Choose to use elite variable but would like to count the number of years member is elite so it becomes numerical
merged_na1 <- merged_na %>%
  mutate(elite = str_replace(elite, "20,20,2021", "2020,2021")) %>%
  mutate(elite = str_replace(elite, "20,20 ", "2020")) %>%
  mutate(elite = str_replace(elite, "20202021", "2020,2021")) %>%
  mutate(elite = na_if(elite, "")) %>%
  mutate(elite = ifelse(is.na(elite), 0, str_count(elite, ",") + 1)) %>%
  mutate(elite= as.numeric(elite))

#Choose to use friends variable and would also like this to be numerical
merged_na1 <- merged_na1 %>%
  mutate(friends = ifelse(friends == "None", 0, str_count(friends, ",") + 1))

#Make state categorical variable
merged_na1$state <- factor(merged_na1$state)
summary(merged_na1$state)

#Create days_difference variable calculating the days between the review being posted and when member first joined yelp
#First turn date columns to date format
merged_na2 <- merged_na1 %>%
  mutate(
    yelping_since = as.POSIXct(yelping_since, format = "%Y-%m-%d %H:%M:%S"),
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
    days_difference = as.numeric(difftime(date, yelping_since, units = "days"))
  )
merged_na3 = subset(merged_na2, select = -c(date, yelping_since,name))





#Text Processing ~ SENTIMENT ANALYSIS ~
require(sentimentr)
sentiment_1 <- merged_na3 %>%
  get_sentences() %$%
  sentiment_by(text, by = list(review_id))

#Adding the sentiment scores to the merged dataframe
merged_na5 <- merge (x= merged_na3, y=sentiment_1, by= c("review_id"), all.x= TRUE)



#Average sentiments for the categories (CREATE GRAPH?)
average_sentiments_table1 <- merged_na5 %>%
  group_by(stars.x) %>%
  summarise(average_sentiment = mean(ave_sentiment, na.rm = TRUE))

##Removing NA values generated through the preprocessing
#Remove sd value introduced by sentiment analysis
merged_na6 = subset(merged_na5, select = -c(sd))
#NA values found for days_difference removed
merged_na6 <- na.omit(merged_na6)
cleaned_data = subset(merged_na6, select = -c(review_id, user_id, business_id, user_id, word_count, text))

##Creating graphs
#Proportion of star rating amongst all reviews
percentage_data <- merged_na6 %>%
  group_by(stars.x) %>%
  summarise(percentage = n() / nrow(merged_na6))

ggplot(percentage_data, aes(x = factor(stars.x), y = percentage)) +
  geom_bar(stat = "identity", fill = "#0073C2FF", color = "white", size = 0.7) +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Percentage Distribution of Stars",
       x = "Stars",
       y = "Percentage",
       caption = "Data Source: Your Source") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "none",      
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 10, color = "gray"))

#To validate including useful, funny, cool votes in model - plotting how they are distributed between the star ratings
merged_na6 %>%
  group_by(stars.x) %>%
  summarise(useful_count = sum(useful.x),
            cool_count = sum(cool.x),
            funny_count = sum(funny.x)) %>%
  pivot_longer(cols = c("useful_count", "cool_count", "funny_count"), names_to = "category", values_to = "count") %>%
  ggplot(aes(x = factor(stars.x), y = count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Votes by Star Category",
       x = "Stars",
       y = "Count",
       fill = "Category") +
  theme_minimal()

#Stacked plot for all the compliments 
merged_na6 %>%
  group_by(stars.x) %>%
  summarise(across(starts_with("compliment_"), sum)) %>%
  pivot_longer(cols = starts_with("compliment_"), names_to = "category", values_to = "count") %>%
  ggplot(aes(x = factor(stars.x), y = count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Compliments by Star Category",
       x = "Stars",
       y = "Count",
       fill = "Compliment Category") +
  theme_minimal()

#Boxplot to show how star ratings relate to the average star ratings of the businesses and average star ratings the user gives
ggplot(merged_na6, aes(x=stars.x, y=stars.y)) + geom_boxplot()
ggplot(merged_na6, aes(x=stars.x, y=average_stars)) + geom_boxplot()

#How the sentiment score is distributed for each of the star categories

ggplot(merged_na6, aes(x = factor(stars.x), y = ave_sentiment, fill = factor(stars.x))) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(title = "Average Sentiment by Stars",
       x = "Stars",
       y = "Average Sentiment") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.box.background = element_rect(color = "black", fill = "white")
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrodyellow", "lightpink"))

#Sentiment Score Density plot
merged_na6 %>%
  ggplot() +
  geom_density(aes(x = ave_sentiment), fill = "#0073C2FF", color = "#1A1A1A", alpha = 0.7) +
  labs(title = "Density Plot of Average Sentiment",
       x = "Average Sentiment",
       y= "Density")+
  coord_cartesian(xlim = c(-1, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"  # Remove legend
  )
#Star distribution between states
percentage_data_state <- merged_na6 %>%
  group_by(state, stars.x) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(percentage = count / sum(count))  

ggplot(percentage_data_state, aes(x = stars.x, y = percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage of each category in stars.x by State",
       x = "Stars",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ state, scales = "free_y", ncol = 2) +  # Facet by state
  theme_minimal()

#Star distribution against elite status
merged_na6 %>%
  group_by(stars.x) %>%
  summarise(elite_count = sum(elite)) %>%
  ggplot(aes(x = factor(stars.x), y = elite_count)) +
  geom_bar(stat = "identity", fill = "#0073C2FF", color = "white", size = 0.7) +
  geom_text(aes(label = elite_count),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Elite Status Distribution",
       x = "Stars",
       y = "Elite Count",
       caption = "Data Source: Your Source") +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, 250000), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 10, color = "gray"))

#Star Distribution against friend count
merged_na6 %>%
  group_by(stars.x) %>%
  summarise(friends_count = sum(friends)) %>%
  ggplot(aes(x = factor(stars.x), y = friends_count, fill = factor(stars.x))) +
  geom_bar(stat = "identity", fill = "#0073C2FF",color = "white", size = 0.7) +
  geom_text(aes(label = friends_count),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(title = "Count of Friends by Star Category",
       x = "Stars",
       y= "Friend Count",
       caption = "Data Source: Your Source") +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 10, color = "gray"))

##MODELLING
#After attempting to run the mode, the process was too computationally expensive so I chose to reduce the observations further to 300000
set.seed(1)
cleaned_data_small<- sample_n(cleaned_data, size = 300000)

#Splitting into training and test data with 90% allocated to training
set.seed(1)
n <- nrow(cleaned_data_small)
train_indices <- sample(1:n, 0.9 * n, replace = FALSE) 
train_data <- cleaned_data_small[train_indices, ]
test_data <- cleaned_data_small[-train_indices, ]

#Main Model Used - RF with ntree=1000 and mtry=6 
set.seed(1)
model_RF1000_6 <- randomForest(stars.x ~ ., data = train_data, ntree = 1000, mtry=6, importance=TRUE, do.trace = TRUE)
pred_RF_test1000_6 = predict(model_RF1000_6, test_data, do.trace = TRUE)

#Accuracy on training data
accuracytrain1000_6 <- sum(diag(model_RF1000_6$confusion)) / sum(model_RF1000_6$confusion)
print(paste("Accuracy on Train Data:", round(accuracytrain1000_6, 4)))

#Accuracy on test data
conf_matrix1000_6 <- table(Actual = test_data$stars.x, Predicted = pred_RF_test1000_6)
accuracy1000_6 <- sum(diag(conf_matrix1000_6)) / sum(conf_matrix1000_6)
print(paste("Accuracy on Test Data:", round(accuracy1000_6, 4)))

model_RF1000_6

#Analysing the importance of the different variables in decreasing the mean accuracy
importance(model_RF1000_6)
varImpPlot(model_RF1000_6, type=1, main='Random Forest Variable Importance', cex.axis = 0.8)


#First attempt with 100 trees and default mtry
set.seed(1)
model_RF100_5 <- randomForest(stars.x ~ ., data = train_data, ntree = 100, do.trace = TRUE)
pred_RF_test100_5 = predict(model_RF100_5, test_data, do.trace = TRUE)

#Accuracy on training data
accuracytrain100_5 <- sum(diag(model_RF100_5$confusion)) / sum(model_RF100_5$confusion)
print(paste("Accuracy on Train Data:", round(accuracytrain100_5, 4)))

conf_matrix100_5 <- table(Actual = test_data$stars.x, Predicted = pred_RF_test100_5)
#Accuracy on test data
accuracy100_5 <- sum(diag(conf_matrix100_5)) / sum(conf_matrix100_5)

#Second attempt with 500 trees and default mtry
model_RF500_5 <- randomForest(stars.x ~ ., data = train_data, ntree = 500, do.trace = TRUE)
pred_RF_test500_5 = predict(model_RF500_5, test_data, do.trace = TRUE)

#Accuracy on training data
accuracytrain500_5 <- sum(diag(model_RF500_5$confusion)) / sum(model_RF500_5$confusion)
print(paste("Accuracy on Train Data:", round(accuracytrain500_5, 4)))

conf_matrix500_5 <- table(Actual = test_data$stars.x, Predicted = pred_RF_test500_5)
#Accuracy on test data
accuracy500_5 <- sum(diag(conf_matrix500_5)) / sum(conf_matrix500_5)

#Running with 1000 trees
model_RF1000_5 <- randomForest(stars.x ~ ., data = train_data, ntree = 1000, do.trace = TRUE)
pred_RF_test1000_5 = predict(model_RF1000_5, test_data, do.trace = TRUE)

#Accuracy on training data
accuracytrain1000_5 <- sum(diag(model_RF1000_5$confusion)) / sum(model_RF1000_5$confusion)
print(paste("Accuracy on Train Data:", round(accuracytrain1000_5, 4)))

conf_matrix1000_5 <- table(Actual = test_data$stars.x, Predicted = pred_RF_test1000_5)
#Accuracy on test data
accuracy1000_5 <- sum(diag(conf_matrix1000_5)) / sum(conf_matrix1000_5)



#Trying to find the best mtry to use by testing that OOB error for 3-7
oob.values <- rep(0, 5)

# Fit models and collect OOB error rates
for (i in 3:7) {
  temp.model <- randomForest(stars.x ~ ., data = train_data, mtry = i, ntree = 100)
  oob.values[i - 2] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}
save(oob.values,file="oob_values.Rdata")

plot(x = 3:7, y = oob.values, type = "b", 
     xlab = "Number of Variables Tried at Each Split", ylab = "Out-of-Bag Error Rate",
     main = "Optimal Number of Variables Tried At Each Split (mtry)",
     col = "steelblue", pch = 16, bty = "n", ylim = c(0.372, 0.378), lwd = 2, cex.lab = 1.2, cex.axis = 1.2)
grid()
text(x = 3:7, y = oob.values, labels = round(oob.values, 5), pos = 3, col = "darkred", cex = 0.8)
legend("topright", legend = "OOB Error", col = "steelblue", pch = 16, cex = 1.2)
png("optimal_mtry.png", width = 800, height = 600, units = "px", res = 100)



#Checking random forest with ntrees=500 and mtry=6
set.seed(1)
model_RF500_6 <- randomForest(stars.x ~ ., data = train_data, ntree = 500,mtry=6, do.trace = TRUE)
pred_RF_test500_6 = predict(model_RF500_6, test_data, do.trace = TRUE)
#Accuracy on training data
accuracytrain500_6 <- sum(diag(model_RF500_6$confusion)) / sum(model_RF500_6$confusion)
print(paste("Accuracy on Train Data:", round(accuracytrain500_6, 4)))

conf_matrix500_6 <- table(Actual = test_data$stars.x, Predicted = pred_RF_test500_6)
# Calculate accuracy on test data
accuracy500_6 <- sum(diag(conf_matrix500_6)) / sum(conf_matrix500_6)
print(paste("Accuracy on Test Data:", round(accuracy500_6, 4)))

##Checking bagging model performance specifying nbags=500
set.seed(1)
bag1<- bagging(stars.x~., data=train_data, nbagg=500, coob=TRUE, control= rpart.control(minsplit=2, cp=0.1), do.trace=TRUE) 
bag1
bag_predictions1 <- predict(bag1, newdata = test_data, type = "class")

#Creating confusion matrix
conf_matrix1 <- confusionMatrix(bag_predictions1, test_data$stars.x)$table

# Calculate and print accuracy
accuracy_bagging1 <- sum(diag(conf_matrix1)) / sum(conf_matrix1)
cat("Accuracy on Test Data:", accuracy_bagging1, "\n")








