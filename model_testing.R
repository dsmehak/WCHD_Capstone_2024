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

#set up a random forest model that takes the variables in map_subdf as inputs to predict 'Unsafe_Driver' from 0 to 1

#first do a logistic regression model that takes the variables in map_subdf as inputs to predict 'Unsafe_Driver' from 0 to 1 and plots it
#set up evaluation use RMSE and AUC for both and compare
#set up loop that can add a 'prediction' value form 0 to 1 in the column 'likelihood_unsafe_driver' in the map_subdf data frame
#use the example from the github and then use the shell of the pre-existing code to graph it


#set up baseline multi variable regression to check relationship between time of day, day of week, and output of unsafe_driver
#filter out all the NAs bc it's being a butt
    map_subdf_2 <- map_subdf %>%
    filter(!is.na(Grid_ID), Grid_ID != 0)
    
#-------------------------------------------------------
  #baseline regression, use friday as reference day?
    map_subdf_2$Day_of_Week <- relevel(map_subdf_2$Day_of_Week, ref = "Friday")
    logit_model <- glm(Unsafe_Driver ~ Time_of_Day + Day_of_Week, data = map_subdf_2, family = binomial)
    summary(logit_model)  

  #likelihood ratio test to compare logistic model with null model
    null_model <- glm(Unsafe_Driver ~ 1, data = map_subdf_2, family = binomial)
    anova(null_model, logit_model, test = "Chisq")

  #auc prediction and curve check
    predicted_probs <- predict(logit_model, type = "response")  # Get predicted probabilities
    predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)    # Convert to 0/1 using threshold = 0.5
    confusionMatrix(factor(predicted_classes), factor(map_subdf_2$Unsafe_Driver))
  #auc check
    roc_curve <- roc(map_subdf_2$Unsafe_Driver, predicted_probs)
    plot(roc_curve, col = "blue")
    auc(roc_curve)
    #very low auc, 0.5342
#----------------------------------
    #--------THIS IS THE ONE! TEST THIS AS RANDOM FOREST!
    logit_model_test <- glm(Unsafe_Driver ~ Day_of_Week + Time_of_Day + Grid_ID + Worst_Injury_Num + Rural_Urban + Weather_Conditions + Road_Conditions, data = map_subdf_2, family = binomial)
    summary(logit_model_test)
    
    #compare to previous model without interaction term
    anova(logit_model, logit_model_test, test = "Chisq")
    #very high p-value, not much difference rip
    
    #check AUC
    logit_probs_test <- predict(logit_model_test, type = "response")
    
    # Compute AUC
    logit_roc_test <- roc(map_subdf_2$Unsafe_Driver, logit_probs_test)
    plot(logit_roc_test, col = "red", main = "Logistic Regression with Pedestrian ROC Curve")
    auc(logit_roc_test)
    #omg its better!!!!! 0.6031!!!
#--------------------------------------
    map_subdf_2$Unsafe_Driver_Likelihood <- predict(logit_model_test, newdata = map_subdf_2, type = "response")
    summary(map_subdf_2$Unsafe_Driver_Likelihood)
    #yeah .. makes sense that max = 0.5353501
    
    map_obj <- leaflet(map_subdf_2) %>%
      addTiles() %>%  
      addCircleMarkers(
        ~Crash_Longitude, ~Crash_Latitude,  
        color = ~colorNumeric(palette = "YlOrRd", domain = map_subdf_2$Unsafe_Driver_Likelihood)(Unsafe_Driver_Likelihood),
        radius = 5,              # Set a fixed size for the circles (or adjust as needed)
        fillOpacity = 0.7,       # Adjust the transparency of the circles
        popup = ~paste("Likelihood: ", round(Unsafe_Driver_Likelihood, 2))  # Show likelihood in popup
      )
    
    map_obj
#-------------------------------------------------
  #now try it with an added interaction term also
    logit_model_interaction <- glm(Unsafe_Driver ~ Time_of_Day * Day_of_Week, data = map_subdf_2, family = binomial)
    summary(logit_model_interaction)

  #compare to previous model without interaction term
    anova(logit_model, logit_model_interaction, test = "Chisq")
  #very high p-value, not much difference rip

  #check AUC
  # Get predicted probabilities
  logit_probs_interaction <- predict(logit_model_interaction, type = "response")

  # Compute AUC
  logit_roc_interaction <- roc(map_subdf_2$Unsafe_Driver, logit_probs_interaction)
  plot(logit_roc_interaction, col = "red", main = "Logistic Regression with Interaction ROC Curve")
  auc(logit_roc_interaction)
  #almost same auc, 0.5312! not good
#----------------------------------------------
#ok, now try it with latitude and longitude included
  #now try it with latitude and longitude
  logit_model_location <- glm(Unsafe_Driver ~ Time_of_Day + Day_of_Week + Crash_Latitude + Crash_Longitude, data = map_subdf_2, family = binomial)
  summary(logit_model_interaction)
  
  #compare to previous model without interaction term
  anova(logit_model, logit_model_location, test = "Chisq")
  #very high p-value, not much difference rip
  
  #check AUC
  # Get predicted probabilities
  logit_probs_location <- predict(logit_model_location, type = "response")
  
  # Compute AUC
  logit_roc_location <- roc(map_subdf_2$Unsafe_Driver, logit_probs_location)
  plot(logit_roc_location, col = "red", main = "Logistic Regression with Interaction ROC Curve")
  auc(logit_roc_location)
  #bro its 0.5518 now thats nothingggg
#-------------------------------------------------
  #glm with rural_urban included
  logit_model_rural <- glm(Unsafe_Driver ~ Time_of_Day + Day_of_Week + Rural_Urban, data = map_subdf_2, family = binomial)
  summary(logit_model_rural)
  
  #compare to previous model without interaction term
  anova(logit_model, logit_model_rural, test = "Chisq")
  #very high p-value, not much difference rip
  
  #check AUC
  # Get predicted probabilities
  logit_probs_rural <- predict(logit_model_rural, type = "response")
  
  # Compute AUC
  logit_roc_rural <- roc(map_subdf_2$Unsafe_Driver, logit_probs_rural)
  plot(logit_roc_rural, col = "red", main = "Logistic Regression with Rural_Urban ROC Curve")
  auc(logit_roc_rural)
  #bro its 0.541 how is it lowerrrrr
#-----------------------------------------------
  #glm with 'Grid_ID' added 
  #try it with the grid thing found in that paper? do it in dataprocessing.R
  #give each row a 'grid ID' based on a 1/2 mile square grid of latitudes and longitudes?
  
  #glm with rural_urban included
  logit_model_grid <- glm(Unsafe_Driver ~ Time_of_Day + Day_of_Week + Rural_Urban + Grid_ID, data = map_subdf_2, family = binomial)
  summary(logit_model_grid)
  
  #compare to previous model without interaction term
  anova(logit_model, logit_model_grid, test = "Chisq")
  
  #check AUC
  # Get predicted probabilities
  logit_probs_grid <- predict(logit_model_grid, type = "response")
  
  # Compute AUC
  logit_roc_grid <- roc(map_subdf_2$Unsafe_Driver, logit_probs_grid)
  plot(logit_roc_grid, col = "red", main = "Logistic Regression with Grid ROC Curve")
  auc(logit_roc_grid)
  #its 0.5786 its getting better!!! just need to add more and make it even better? figure out what variables to add
  #wednesday significant , Urban significant, saturday significant, time of day significant!! yay!
#-------------------------------------------------
  #glm with 'Pedestrian' added 
  
  logit_model_pedestrian <- glm(Unsafe_Driver ~ Time_of_Day + Day_of_Week + Rural_Urban + Grid_ID + Pedestrian, data = map_subdf_2, family = binomial)
  summary(logit_model_pedestrian)
  
  #compare to previous model without interaction term
  anova(logit_model, logit_model_pedestrian, test = "Chisq")
  #very high p-value, not much difference rip
  
  #check AUC
  logit_probs_pedestrian <- predict(logit_model_pedestrian, type = "response")
  
  # Compute AUC
  logit_roc_pedestrian <- roc(map_subdf_2$Unsafe_Driver, logit_probs_pedestrian)
  plot(logit_roc_pedestrian, col = "red", main = "Logistic Regression with Pedestrian ROC Curve")
  auc(logit_roc_pedestrian)
 #bro did that make it lower??? its 0.5632 now fjsnfksdnfds
#-----------------------------------------
  #--------THIS IS THE ONE! TEST THIS AS RANDOM FOREST!
  logit_model_test <- glm(Unsafe_Driver ~ Day_of_Week + Time_of_Day + Grid_ID + Worst_Injury_Num + Rural_Urban + Weather_Conditions + Road_Conditions, data = map_subdf_2, family = binomial)
  summary(logit_model_test)
  
  #compare to previous model without interaction term
  anova(logit_model, logit_model_test, test = "Chisq")
  #very high p-value, not much difference rip
  
  #check AUC
  logit_probs_test <- predict(logit_model_test, type = "response")
  
  # Compute AUC
  logit_roc_test <- roc(map_subdf_2$Unsafe_Driver, logit_probs_test)
  plot(logit_roc_test, col = "red", main = "Logistic Regression with Pedestrian ROC Curve")
  auc(logit_roc_test)
  #omg its better!!!!! 0.6031!!!
#------------------------------------------
#ok, now try it with a random forest model?
  set.seed(123) 
  map_subdf_2$Unsafe_Driver <- as.factor(map_subdf_2$Unsafe_Driver) 

  rf_model <- randomForest(Unsafe_Driver ~ Time_of_Day + Day_of_Week, data = map_subdf_2, ntree = 500, mtry = 2,
                           importance = TRUE)
  summary(rf_model)

  # Get predicted probabilities
  rf_probs <- predict(rf_model, type = "prob")[,2] 

  # Compute AUC
  rf_roc <- roc(map_subdf_2$Unsafe_Driver, rf_probs)
  plot(rf_roc, col = "blue", main = "Random Forest ROC Curve")
  auc(rf_roc)
#this has an AUC of exactly 0.5, which is WORSE lmao, try: adding interaction term, adding more variables and test again? 
#will need to modify glm regression to compare
  
#---------------------------------------------------------
#random forest with added variables FROM FINAL TEST GLM
  
  set.seed(123) 
  map_subdf_2$Unsafe_Driver <- as.factor(map_subdf_2$Unsafe_Driver) 
  
  rf_model_test <- randomForest(Unsafe_Driver ~ Day_of_Week + Time_of_Day + Grid_ID + Worst_Injury_Num, data = map_subdf_2, ntree = 500, mtry = 2,
                           importance = TRUE)
  
  # Get predicted probabilities
  rf_probs_test <- predict(rf_model_test, type = "prob")[,2] 
  
  # Compute AUC
  rf_roc_test <- roc(map_subdf_2$Unsafe_Driver, rf_probs_test)
  plot(rf_roc_test, col = "blue", main = "Random Forest ROC Curve w/ added variables")
  importance(rf_model_test)
  
  auc(rf_roc_test)
  #its 0.5599, somehow lower? 
#day_of_week + time_of_Day + grid_ID + word_injury_num is 0.5628, higher than glm version!,
  #+Road_Conditions and Weather Conditions = 0.5627, lower
  #+pedestrian = 0.5604, also lower
#-------------------------------
  #trying with xgboost instead
  #do teh one-hot encoding

  map_subdf_3 <- cbind(map_subdf_2[, "Time_of_Day"], map_subdf_2[, "Day_of_Week"], map_subdf_2[, "Grid_ID"], map_subdf_2[, "Worst_Injury_Num"])
  
  Day_of_Week_dummies <- model.matrix(~ Day_of_Week - 1, data = map_subdf_3)
  Time_of_Day_dummies <- model.matrix(~ Time_of_Day - 1, data = map_subdf_3)
  Grid_ID_dummies <- model.matrix(~ Grid_ID - 1, data = map_subdf_3)
  
  map_subdf_3 <- cbind(map_subdf_2, Day_of_Week_dummies, Time_of_Day_dummies, Grid_ID_dummies)

  map_subdf_3 <- map_subdf_3[, !names(map_subdf_3) %in% c("Day_of_Week", "Time_of_Day", "Grid_ID")]
  

  train_matrix <- 
    as.matrix(map_subdf_3[, c("Worst_Injury_Num", names(Day_of_Week_dummies),
                              names(Time_of_Day_dummies), names(Grid_ID_dummies))])
  
  model_xgb <- xgboost(
    data = train_matrix,
    label = map_subdf_3$Unsafe_Driver,
    nrounds = 100,
    objective = "binary:logistic",
    eval_metric = "auc"
  )
  #auc = 0.557637
  
#-----------------
#maybe i should just do the leaflet plot with teh glm model honestly ...
  # Predict the probabilities
  map_subdf_2$Unsafe_Driver_Likelihood <- predict(logit_model_test, newdata = map_subdf_2, type = "response")
  summary(map_subdf_2$Unsafe_Driver_Likelihood)
  #yeah .. makes sense that max = 0.5353501
  
  map_obj <- leaflet(map_subdf_2) %>%
    addTiles() %>%  
    addCircleMarkers(
      ~Crash_Longitude, ~Crash_Latitude,  
      color = ~colorNumeric(palette = "YlOrRd", domain = map_subdf_2$Unsafe_Driver_Likelihood)(Unsafe_Driver_Likelihood),
      radius = 5,              # Set a fixed size for the circles (or adjust as needed)
      fillOpacity = 0.7,       # Adjust the transparency of the circles
      popup = ~paste("Likelihood: ", round(Unsafe_Driver_Likelihood, 2))  # Show likelihood in popup
    )
  
  map_obj
  
  
  
  
 