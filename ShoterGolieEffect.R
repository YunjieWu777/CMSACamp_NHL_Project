
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
  mutate(outcome_type = relevel(as.factor(outcome_type),ref="PlayOutsideZone"),
         shotType=as.factor(shotType),
         shotRush=as.factor(shotRush),
         shotRebound=as.factor(shotRebound),
         goalie_name_id=as.factor(goalie_name_id),
         shooter_name_id=as.factor(shooter_name_id),
         outcome = as.integer(outcome_type) -1,
         arenaAdjustedShotDistance=scale(arenaAdjustedShotDistance),
         shotAngleAdjusted=scale(shotAngleAdjusted))

set.seed(777)
recent_season<- recent_season %>% sample_frac(0.1)


gl <- function(){
  glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
          shotType+shotRush+shotRebound+
          (1|shooter_name_id) + (1|goalie_name_id),
        data = test, family = "binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  }

# On Goal -----------------------------------------------------------------

ongoal<- recent_season %>% 
  filter(shotWasOnGoal==1)

# Rebound

test<- ongoal %>% 
  filter(outcome %in% c(0,1))

rebound_ongoal<- gl

?isSingular

player_effects <- REsim(rebound_ongoal)

summary(rebound_ongoal)

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


# Building the model

a<- tibble(rebound_ongoal = exp(predict(rebound_ongoal,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
           froze_ongoal = exp(predict(froze_ongoal,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
           inZone_ongoal = exp(predict(inZone_ongoal,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
           outZone_ongoal = exp(predict(outZone_ongoal,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
           stop_ongoal = exp(predict(stop_ongoal,newdata=recent_season, type = "link",allow.new.levels = TRUE))) 

a<- a %>% 
  mutate(goal=1/(1+rebound_ongoal+froze_ongoal+inZone_ongoal+outZone_ongoal+stop_ongoal))


exp(predict(inZone_ongoal,newdata=head(recent_season), type = "link",allow.new.levels = TRUE))
predict(inZone_ongoal,newdata=head(recent_season), type = "link",allow.new.levels = TRUE)

VarCorr(rebound_ongoal) %>% as_tibble() %>% mutate(icc = vcov / sum(vcov)) %>% dplyr::select(grp, icc)

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

rebound_all<-gl()

#
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

# Goal

test<- recent_season %>% 
  filter(outcome %in% c(0,2)) %>% 
  mutate(outcome = case_when(outcome==2 ~1,
                             TRUE ~0))

goal_all<-gl()

# Froze

test<- recent_season %>% 
  filter(outcome %in% c(0,3)) %>% 
  mutate(outcome = case_when(outcome==3 ~1,
                             TRUE ~0))

froze_all<-gl()



# In Zone

test<- recent_season %>% 
  filter(outcome %in% c(0,4)) %>% 
  mutate(outcome = case_when(outcome==4 ~1,
                             TRUE ~0))

inZone_all<-gl()

# Stopped

test<- recent_season %>% 
  filter(outcome %in% c(0,5)) %>% 
  mutate(outcome = case_when(outcome==5 ~1,
                             TRUE ~0))

stop_all<-gl()


## Create Table

all_gl<- tibble(rebound_all = exp(predict(rebound_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                goal_all = exp(predict(goal_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                froze_all = exp(predict(froze_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                inZone_all = exp(predict(inZone_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                stop_all = exp(predict(stop_all,newdata=recent_season, type = "link",allow.new.levels = TRUE))) 

all_gl<- all_gl %>% 
  mutate(outZone=1/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         rebound=rebound_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         goal= goal_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         froze=froze_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         inZone=inZone_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         stop=stop_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all))

all_gl<-cbind(all_gl,)
save(all_gl,file="data/all_gl.RData")


all_gl_var = data.frame(outcome=c("goal","rebound","froze","inZone","stop"),
                     shooter_var = c(summary(goal_all)$varcor$shooter_name_id[1],
                             summary(rebound_all)$varcor$shooter_name_id[1],
                             summary(froze_all)$varcor$shooter_name_id[1],
                             summary(inZone_all)$varcor$shooter_name_id[1],
                             summary(stop_all)$varcor$shooter_name_id[1]),
                     goalie_var= c(summary(goal_all)$varcor$goalie_name_id[1],
                                   summary(rebound_all)$varcor$goalie_name_id[1],
                                   summary(froze_all)$varcor$goalie_name_id[1],
                                   summary(inZone_all)$varcor$goalie_name_id[1],
                                   summary(stop_all)$varcor$goalie_name_id[1]))

save(all_gl_var,file="data/all_gl_var.RData")

VarCorr(rebound_all) %>% as_tibble() %>% mutate(icc = vcov / sum(vcov)) %>% dplyr::select(grp, icc)



# Heat map & fake data frame ----------------------------------------------


df = expand.grid(xcord=-25:25, ycord=23:90)
df<-df %>% 
  mutate(shotAngleAdjusted=atan(xcord/abs(89-ycord))/0.0174532925,
         arenaAdjustedShotDistance= sqrt(xcord^2+(ycord-89)^2),
         shotType=NA,
         shotRush=0,
         shotRebound=0,
         shooter_name_id=NA,
         goalie_name_id=NA)

source('rink.r')  
g <- rink

g + 
  stat_summary_hex(data = all_gl,
                   aes(x = ycord,
                       y = xcord,
                       z = goal,
                       color = goal),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "Goal",
       fill = "Probability") + 
  theme_bw()


