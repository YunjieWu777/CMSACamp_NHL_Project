# Random Forests Model

library(tidyverse)
load("shots2020.RData")
load("shots1019.RData")

evenstrength <- 
  shots1019 %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)


ongoal <- evenstrength %>% 
  filter(shotWasOnGoal==1,
         !is.na(shotType),
         shotType!="")

ongoal <-
  ongoal %>%
  mutate(
    Outcome = case_when(shotGoalieFroze == 1 ~ "GoalieFroze",
                        goal == 1 ~ "Goal",
                        shotGeneratedRebound == 1 ~ "GeneratesRebound",
                        shotPlayContinuedInZone == 1 ~ "PlayInZone",
                        shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                        shotPlayStopped == 1 ~ "PlayStopped"))

ongoal <-
  ongoal %>%
  mutate(Outcome = as.factor(Outcome))

ongoal$Outcome2 <- 
  relevel(ongoal$Outcome, ref = "Goal")

model_data <- ongoal %>%
  dplyr::select(Outcome2, season,
              shotAngleAdjusted, shotDistance,
                shotType, shotRebound, shotRush)

model_data2 <- ongoal %>%
  dplyr::select(Outcome2, season,
                shotAngleAdjusted, shotDistance,
                shotType, shotRebound, shotRush, shooterName)

model_data <- model_data %>%
  mutate(Outcome2 = as.factor(Outcome2))

library(ranger)
init_shot_rf <- ranger(Outcome2 ~., data = model_data, num.trees = 50, importance = "impurity")

library(vip)
vip(init_shot_rf, geom = "point") + theme_bw()


# LOSO Preds --------------------------------------------------------------

loso_cv_preds <- 
  map_dfr(unique(model_data$season),
          function(x) {
            
            test_data <- model_data %>%
              filter(season == x)
            train_data <- model_data %>%
              filter(season != x)
            
            forest_model <-
              ranger(Outcome2 ~., 
                       data = train_data, num.trees = 200, # importance = "impurity",
                     probability = TRUE)
            
          
            predict(forest_model, data = test_data, type = "response")$predictions %>%
              as_tibble() %>%
              mutate(Outcome2 = test_data$Outcome2,
                     season = x,
                     xcord = )
          })

rf_oreds2 <- 
  map_dfr(unique(model_data$season),
          function(x) {
            
            test_data <- model_data %>%
              filter(season == x)
            train_data <- model_data %>%
              filter(season != x)
            
            forest_model <-
              ranger(Outcome2 ~., 
                     data = train_data, num.trees = 200, # importance = "impurity",
                     probability = TRUE)
            
            
            predict(forest_model, data = test_data, type = "response")$predictions %>%
              as_tibble() %>%
              mutate(Outcome2 = test_data$Outcome2,
                     season = x,
                     xcord = )
          })

save(loso_cv_preds, file = "randomforestloso.RData")


ep_cv_loso_calibration_results <- loso_cv_preds %>%
  pivot_longer(Goal:PlayStopped,
               names_to = "outcome_type",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.03) * 0.03) %>%
  group_by(outcome_type, bin_pred_prob) %>%
  dplyr::summarize(n_plays = n(),
                   n_scoring_event = length(which(Outcome2 == outcome_type)),
                   bin_actual_prob = n_scoring_event / n_plays,
                   bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_plays)) %>% 
  ungroup() %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0))



ep_cv_loso_calibration_results %>%
  
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(se = FALSE) +
  geom_point(aes(size = n_plays)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + 
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(legend.position = c(1, .05),
        legend.justification = c(1, 0),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ outcome_type, ncol = 3)+
  theme_reach() +
  labs(x = "Predicted Probabilities",
       y = "Actual Probabilities", 
       size = "Number of Plays")


# Set up to make heatmaps

loso_cv_preds$row_num <- seq.int(nrow(loso_cv_preds))
ongoal$row_num <- seq.int(nrow(ongoal))


rf_preds <- left_join(loso_cv_preds, ongoal, by = "row_num")

save(rf_preds, file = "rf_preds.RData")




# KNN ---------------------------------------------------------------------

library(caret)
set.seed(2000)
folds <- groupKFold(model_data$game_id, k = length(unique(model_data$game_id))) 


train_i <- trainControl(method = "knn",
                        index = folds,
                        p = 0.75) 
train_i <- as.numeric(unlist(train_i))

train_data <- model_data[train_i,]
test_data <- model_data[-train_i,]





# Fitting model -----------------------------------------------------------

library(ranger)

init_shot_rf <- ranger(Outcome ~., data = model_data, num.trees = 50,
                       importance = "impurity")

init_shot_rf



# Variable importance -----------------------------------------------------

library(vip)
vip(init_shot_rf, geom = "point") + theme_bw()


# Tuning random forests ---------------------------------------------------

rf_tune_grid <- 
  expand.grid(mtry = seq(3, 18, by = 3), 
              splitrule = "variance",
              min.node.size = 5)
set.seed(1917)
caret_shot_rf <- 
  train(Outcome ~ ., data = model_data,
        method = "ranger", num.trees = 50,
        trControl = trainControl(method = "cv", number = 5),
        tuneGrid = rf_tune_grid)




