library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(randomForest)
library(leaflet)
library(xgboost)
library(caret)
library(tidyverse)
library(pROC)

map_subdf_2 <- map_subdf %>%
  filter(!is.na(Grid_ID), Grid_ID != 0)

logit_model_test <- glm(Unsafe_Driver ~ Day_of_Week + 
                          Time_of_Day + Grid_ID + Worst_Injury_Num + 
                          Rural_Urban + Weather_Conditions + Road_Conditions,
                        data = map_subdf_2, family = binomial)

map_subdf_2$Unsafe_Driver_Likelihood <- predict(logit_model_test, newdata = map_subdf_2, type = "response")