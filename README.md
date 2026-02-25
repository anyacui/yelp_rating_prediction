# Yelp Star Rating Prediction

## Overview
Built a Random Forest classification model to predict Yelp star ratings using review, user, and business data.

## Data
300,000 reviews from the Yelp Open Dataset with 27 predictor variables across user behaviour, business metrics, and review features.

## Methods
- Sentence-level sentiment analysis using `sentimentr` to account for negation and valence shifters
- Random Forest classification (ntree=1000, mtry=6) selected via OOB error optimisation
- Hyperparameter tuning across tree count and mtry values

## Results
- **Test Accuracy: 63.7%** on 30,000 held-out reviews
- Sentiment score was the most important predictor by mean accuracy decrease

## Full Report
[View full analysis here](https://anyacui.github.io/yelp_review_project/)

**Tools:** R, randomForest, sentimentr, ggplot2
