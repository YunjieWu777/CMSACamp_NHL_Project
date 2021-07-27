# Load Packages--------------------------------------------------
library(tidyverse)
library(nnet)

# How to make recent_season (can ignore) ---------------------------------------------------------------
load("data/shots2020.RData")
load("data/shots0719.RData")
shots1019 <-shots0719 %>% 
  filter(season>2009)
variable<- c("xGoal","arenaAdjustedShotDistance","shotWasOnGoal",
             "shotPlayStopped","shotAngle","shotGoalieFroze",
             "shotPlayContinuedInZone","shotPlayContinuedOutsideZone",
             "awaySkatersOnIce","homeSkatersOnIce","season",
             "shotAngleAdjusted","goal",
             "shotGeneratedRebound", "xCordAdjusted", "yCordAdjusted",
             "shotType","shotRush","shotRebound","game_id",
             "goalieNameForShot","goalieIdForShot","shooterName","shooterPlayerId")

recent_season <- rbind(dplyr::select(shots1019,all_of(variable)),dplyr::select(shots2020,all_of(variable)))

save(recent_season, file = "data/recent_season.RData")

# Load data ---------------------------------------------------------------
load("data/recent_season.RData")

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

save(ongoal_loso_cv_preds, file = "data/ongoal_loso_cv_preds.RData")


# Create missnet_loso_cv_preds --------------------------------------------

missnet<- recent_season %>% 
  filter(shotWasOnGoal==0,
         !is.na(shotType),
         shotType!="",
         homeSkatersOnIce==5 & awaySkatersOnIce==5)%>% 
  mutate(outcome=case_when(shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"),
         outcome=as.factor(outcome),
         shotType=as.factor(shotType))

missnet_loso_cv_preds <-
  map_dfr(unique(missnet$season),
          function(x) {
            test_data <- missnet %>%
              filter(season == x)
            train_data <- missnet %>%
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

save(missnet_loso_cv_preds, file = "data/missnet_loso_cv_preds.RData")


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

save(all_loso_cv_preds, file = "data/all_loso_cv_preds.RData")



# another version of all shots --------------------------------------------


all2 <- recent_season %>% 
  filter(!is.na(shotType),
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


all_loso_cv_preds2 <-
  map_dfr(unique(all2$season),
          function(x) {
            test_data <- all2 %>%
              filter(season == x)
            train_data <- all2 %>%
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



save(all_loso_cv_preds2, file = "data/all_loso_cv_preds2.RData")
