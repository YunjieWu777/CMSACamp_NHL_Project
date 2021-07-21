
# Load Packages and data --------------------------------------------------
library(tidyverse)
load("recent_season.RData")


# Create ongoal_loso_cv_preds ---------------------------------------------
ongoal<- recent_season %>% 
  filter(shotWasOnGoal==1,
         !is.na(shotType),
         shotType!="",
         homeSkatersOnIce==5 & awaySkatersOnIce==5)%>% 
  mutate(outcome=case_when(shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"),
         outcome=relevel(as.factor(outcome),ref="Goal"),
         shotType=as.factor(shotType))


ongoal_loso_cv_preds <-
  map_dfr(unique(ongoal$season),
          function(x) {
            test_data <- ongoal %>%
              filter(season == x)
            train_data <- ongoal %>%
              filter(season != x)
            
            ep_model <-
              multinom(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                       data = train_data, maxit = 300)
            
            predict(ep_model, newdata = test_data, type = "probs") %>%
              as_tibble() %>%
              mutate(outcome = test_data$outcome,
                     season = x,
                     xcord = test_data$xCordAdjusted,
                     ycord = test_data$yCordAdjusted)
          })

save(ongoal_loso_cv_preds, file = "ongoal_loso_cv_preds.RData")

# Create all_loso_cv_preds ------------------------------------------------
all<- recent_season %>% 
  filter(!is.na(shotType),
         shotType!="",
         homeSkatersOnIce==5 & awaySkatersOnIce==5)%>% 
  mutate(outcome=case_when(shotWasOnGoal==0 ~"Miss",
                           shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"),
         outcome=relevel(as.factor(outcome),ref="Goal"),
         shotType=as.factor(shotType))


all_loso_cv_preds <-
  map_dfr(unique(all$season),
          function(x) {
            test_data <- all %>%
              filter(season == x)
            train_data <- all %>%
              filter(season != x)
            
            ep_model <-
              multinom(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                       data = train_data, maxit = 300)
            
            predict(ep_model, newdata = test_data, type = "probs") %>%
              as_tibble() %>%
              mutate(outcome = test_data$outcome,
                     season = x,
                     xcord = test_data$xCordAdjusted,
                     ycord = test_data$yCordAdjusted)
          })

save(all_loso_cv_preds, file = "all_loso_cv_preds.RData")
