
library(tidyverse)
library(lme4)
library(merTools)

load("data/recent_season.RData")

recent_season <-recent_season %>% 
  filter(!is.na(shooterPlayerId), 
         !is.na(goalieNameForShot),
         !is.na(shotType),
         shotType!="",
         homeSkatersOnIce==5 & awaySkatersOnIce==5) %>% 
  mutate(shooter_name_id = paste0(shooterName, "-", shooterPlayerId),
         goalie_name_id = paste0(goalieNameForShot, "-", goalieIdForShot),
         outcome_type=case_when(shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"))

recent_season<-recent_season %>% 
  mutate(outcome_type = relevel(as.factor(outcome_type),ref="Goal"),
         shotType=as.factor(shotType),
         shotRush=as.factor(shotRush),
         shotRebound=as.factor(shotRebound),
         goalie_name_id=as.factor(goalie_name_id),
         shooter_name_id=as.factor(shooter_name_id),
         outcome = as.integer(outcome_type) -1)


recent_season<- recent_season %>% sample_frac(0.1)

# On Goal -----------------------------------------------------------------

ongoal<- recent_season %>% 
  filter(shotWasOnGoal==1)

# Rebound

test<- ongoal %>% 
  filter(outcome %in% c(0,1))

rebound_ongoal<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
        shotType+shotRush+shotRebound+
        (1|shooter_name_id) + (1|goalie_name_id),
      data = test, family = "binomial")

player_effects <- REsim(rebound_ongoal)

player_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  coord_flip() +
  theme_bw()

# Froze

test<- ongoal %>% 
  filter(outcome %in% c(0,2)) %>% 
  mutate(outcome = case_when(outcome==2 ~1,
                             TRUE ~0))

froze_ongoal<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                        shotType+shotRush+shotRebound+
                        (1|shooter_name_id) + (1|goalie_name_id),
                      data = test, family = "binomial")

# In Zone

test<- ongoal %>% 
  filter(outcome %in% c(0,3)) %>% 
  mutate(outcome = case_when(outcome==3 ~1,
                             TRUE ~0))

inZone_ongoal<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                      shotType+shotRush+shotRebound+
                      (1|shooter_name_id) + (1|goalie_name_id),
                    data = test, family = "binomial")

# Out Zone

test<- ongoal %>% 
  filter(outcome %in% c(0,4)) %>% 
  mutate(outcome = case_when(outcome==4 ~1,
                             TRUE ~0))

outZone_ongoal<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                      shotType+shotRush+shotRebound+
                      (1|shooter_name_id) + (1|goalie_name_id),
                    data = test, family = "binomial")


# Stopped

test<- ongoal %>% 
  filter(outcome %in% c(0,5)) %>% 
  mutate(outcome = case_when(outcome==5 ~1,
                             TRUE ~0))

stop_ongoal<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                      shotType+shotRush+shotRebound+
                      (1|shooter_name_id) + (1|goalie_name_id),
                    data = test, family = "binomial")


# Miss Net ----------------------------------------------------------------

miss<- recent_season %>% 
  filter(shotWasOnGoal==0) %>% 
  mutate(outcome=outcome-1)

# Froze

test<- miss %>% 
  filter(outcome %in% c(0,1))

froze_miss<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                        shotType+shotRush+shotRebound+
                        (1|shooter_name_id) + (1|goalie_name_id),
                      data = test, family = "binomial")

player_effects <- REsim(froze_miss)

player_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  coord_flip() +
  theme_bw()

# In Zone

test<- miss %>% 
  filter(outcome %in% c(0,2)) %>% 
  mutate(outcome = case_when(outcome==2 ~1,
                             TRUE ~0))

inZone_miss<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                      shotType+shotRush+shotRebound+
                      (1|shooter_name_id) + (1|goalie_name_id),
                    data = test, family = "binomial")

# Out Zone

test<- miss %>% 
  filter(outcome %in% c(0,3)) %>% 
  mutate(outcome = case_when(outcome==3 ~1,
                             TRUE ~0))

outZone_miss<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                       shotType+shotRush+shotRebound+
                       (1|shooter_name_id) + (1|goalie_name_id),
                     data = test, family = "binomial")

# Stopped

test<- miss %>% 
  filter(outcome %in% c(0,4)) %>% 
  mutate(outcome = case_when(outcome==4 ~1,
                             TRUE ~0))

stop_miss<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                        shotType+shotRush+shotRebound+
                        (1|shooter_name_id) + (1|goalie_name_id),
                      data = test, family = "binomial")

# All ----------------------------------------------------------------------

# Rebound

test<- recent_season %>% 
  filter(outcome %in% c(0,1))

rebound_all<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                        shotType+shotRush+shotRebound+
                        (1|shooter_name_id) + (1|goalie_name_id),
                      data = test, family = "binomial")

player_effects <- REsim(rebound_all)

player_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  coord_flip() +
  theme_bw()

# Froze

test<- recent_season %>% 
  filter(outcome %in% c(0,2)) %>% 
  mutate(outcome = case_when(outcome==2 ~1,
                             TRUE ~0))

froze_all<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                      shotType+shotRush+shotRebound+
                      (1|shooter_name_id) + (1|goalie_name_id),
                    data = test, family = "binomial")

# In Zone

test<- recent_season %>% 
  filter(outcome %in% c(0,3)) %>% 
  mutate(outcome = case_when(outcome==3 ~1,
                             TRUE ~0))

inZone_all<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                       shotType+shotRush+shotRebound+
                       (1|shooter_name_id) + (1|goalie_name_id),
                     data = test, family = "binomial")

# Out Zone

test<- recent_season %>% 
  filter(outcome %in% c(0,4)) %>% 
  mutate(outcome = case_when(outcome==4 ~1,
                             TRUE ~0))

outZone_all<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                        shotType+shotRush+shotRebound+
                        (1|shooter_name_id) + (1|goalie_name_id),
                      data = test, family = "binomial")


# Stopped

test<- recent_season %>% 
  filter(outcome %in% c(0,5)) %>% 
  mutate(outcome = case_when(outcome==5 ~1,
                             TRUE ~0))

stop_all<-glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                     shotType+shotRush+shotRebound+
                     (1|shooter_name_id) + (1|goalie_name_id),
                   data = test, family = "binomial")

