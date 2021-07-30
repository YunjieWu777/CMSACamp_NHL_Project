
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
         angle_org = shotAngleAdjusted,
         distance_org = arenaAdjustedShotDistance,
         arenaAdjustedShotDistance=scale(arenaAdjustedShotDistance),
         shotAngleAdjusted=scale(shotAngleAdjusted))

set.seed(777)
recent_season<- recent_season %>% sample_frac(0.1) %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42))


gl <- function(){
  glmer(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+
          shotType+shotRush+shotRebound+
          (1|shooter_name_id) + (1|goalie_name_id),
        data = test, family = "binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
}


pe<- function(x){
player_effects <- REsim(x)

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

pe()

# Rebound

test<- recent_season %>% 
  filter(outcome %in% c(0,1))

rebound_all<-gl()

write_rds(rebound_all, "model/rebound_all.rds")

pe(rebound_all)

# Goal

test<- recent_season %>% 
  filter(outcome %in% c(0,2)) %>% 
  mutate(outcome = case_when(outcome==2 ~1,
                             TRUE ~0))

goal_all<-gl()
write_rds(goal_all, "model/goal_all.rds")

# Froze

test<- recent_season %>% 
  filter(outcome %in% c(0,3)) %>% 
  mutate(outcome = case_when(outcome==3 ~1,
                             TRUE ~0))

froze_all<-gl()

write_rds(froze_all, "model/froze_all.rds")

# In Zone

test<- recent_season %>% 
  filter(outcome %in% c(0,4)) %>% 
  mutate(outcome = case_when(outcome==4 ~1,
                             TRUE ~0))

inZone_all<-gl()
write_rds(inZone_all, "model/inZone_all.rds")

# Stopped

test<- recent_season %>% 
  filter(outcome %in% c(0,5)) %>% 
  mutate(outcome = case_when(outcome==5 ~1,
                             TRUE ~0))

stop_all<-gl()
write_rds(stop_all, "model/stop_all.rds")

## Create Table

all_gl<- tibble(rebound_all = exp(predict(rebound_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                goal_all = exp(predict(goal_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                froze_all = exp(predict(froze_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                inZone_all = exp(predict(inZone_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                stop_all = exp(predict(stop_all,newdata=recent_season, type = "link",allow.new.levels = TRUE))) 

all_gl<- all_gl %>% 
  mutate(PlayOutsideZone=1/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Rebound=rebound_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Goal= goal_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         GoalieFroze=froze_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         PlayInZone=inZone_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Stop=stop_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all))

all_gl<-cbind(all_gl,recent_season[,15:16],recent_season[,27])
colnames(all_gl)[14]<-"outcome_type"

ep_cv_loso_calibration_results <- all_gl %>%
  pivot_longer(PlayOutsideZone:Stop,
               names_to = "outcome",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.05) * 0.05) %>% 
  group_by(outcome, bin_pred_prob) %>%
  summarize(n_plays = n(),
            n_scoring_event = length(which(outcome_type == outcome)),
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
  facet_wrap(~ outcome, ncol = 3)

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

df <- expand.grid(ycord=-42:42, xcord=23:90)
df<-df %>% 
  mutate(shotAngleAdjusted=((atan(ycord/89-xcord)/0.0174532925)-mean(recent_season$angle_org))/sd(recent_season$angle_org),
         arenaAdjustedShotDistance= (sqrt(ycord^2+(xcord-89)^2)-mean(recent_season$distance_org))/sd(recent_season$distance_org),
         shotType=as.factor("BACK"),
         shotRush=as.factor(0),
         shotRebound=as.factor(0),
         shooter_name_id=as.factor("Oscar Klefbom-8476472"),
         goalie_name_id=as.factor("Pekka Rinne-8471469"),
         angle_org=atan(ycord/abs(89-xcord))/0.0174532925,
         distance_org=sqrt(ycord^2+(xcord-89)^2))

df<-df %>% 
  mutate(rebound_all = exp(predict(rebound_all,newdata=df, type = "link",allow.new.levels = TRUE)),
         goal_all = exp(predict(goal_all,newdata=df, type = "link",allow.new.levels = TRUE)),
         froze_all = exp(predict(froze_all,newdata=df, type = "link",allow.new.levels = TRUE)),
         inZone_all = exp(predict(inZone_all,newdata=df, type = "link",allow.new.levels = TRUE)),
         stop_all = exp(predict(stop_all,newdata=df, type = "link",allow.new.levels = TRUE)),
         outZone=1/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         rebound=rebound_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         goal= goal_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         froze=froze_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         inZone=inZone_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         stop=stop_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all))
         

g+ 
  stat_summary_hex(data = df,
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

source('rink.r')  
g <- rink

plot1<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
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

plot2<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = rebound,
                       color = rebound),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "Rebound",
       fill = "Probability") + 
  theme_bw()

plot3<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = froze,
                       color = froze),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "Froze",
       fill = "Probability") + 
  theme_bw()

plot4<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = inZone,
                       color = inZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "In Zone",
       fill = "Probability") + 
  theme_bw()

plot5<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = outZone,
                       color = outZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "Out Zone",
       fill = "Probability") + 
  theme_bw()

plot6<-g + 
  stat_summary_hex(data = all_gl,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = stop,
                       color = stop),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  
  ylim(25, 100) +
  labs(title = "Stop",
       fill = "Probability") + 
  theme_bw()

gridExtra::grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6, nrow = 2)





ep_cv_loso_calibration_results <- all_gl %>%
  pivot_longer(outZone:stop,
               names_to = "outcome",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.03) * 0.03) %>%
  group_by(outcome_type, bin_pred_prob) %>%
  dplyr::summarize(n_plays = n(),
                   n_scoring_event = length(which(outcome == outcome_type)),
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
  facet_wrap(~ outcome_type, ncol = 3)


pe(stop_all)


g+ theme_bw()

load("data/all_loso_cv_preds2.RData")

mean(((all_loso_cv_preds2$outcome=="Goal")-all_loso_cv_preds2$Goal)^2)+
  mean(((all_loso_cv_preds2$outcome=="GeneratesRebound")-all_loso_cv_preds2$GeneratesRebound)^2)+
  mean(((all_loso_cv_preds2$outcome=="GoalieFroze")-all_loso_cv_preds2$GoalieFroze)^2)+
  mean(((all_loso_cv_preds2$outcome=="PlayInZone")-all_loso_cv_preds2$PlayInZone)^2)+
  mean(((all_loso_cv_preds2$outcome=="PlayStopped")-all_loso_cv_preds2$PlayStopped)^2)+
  mean(((all_loso_cv_preds2$outcome=="PlayOutsideZone")-all_loso_cv_preds2$PlayOutsideZone)^2)

library(tidyverse)
load("data/shots2020.RData")
load("data/all_gl.RData")
shots2020<-shots2020 %>% 
  filter(shotWasOnGoal==1)

a<- all_gl$PlayOutsideZone+all_gl$Rebound+all_gl$Goal+all_gl$GoalieFroze+all_gl$PlayInZone+all_gl$Stop
max(a)


a<-shots2020$xGoal+shots2020$xFroze+shots2020$xPlayContinuedInZone+shots2020$xPlayStopped+shots2020$xPlayContinuedOutsideZone+shots2020$xRebound

mean(a)

hist(a)

shots2020<-shots2020 %>% 
  filter(xGoal+xFroze+xPlayContinuedInZone+xPlayStopped+xPlayContinuedOutsideZone+xRebound > 0.9)




which.min(a)

a<-shots2020[1035,]

which.max(a)


min(a)
max(a)

a<-mean((shots2020$goal-shots2020$xGoal)^2)+
  mean((shots2020$shotGeneratedRebound-shots2020$xRebound)^2)+
  mean((shots2020$shotGoalieFroze-shots2020$xFroze)^2)+
  mean((shots2020$shotPlayContinuedInZone -shots2020$xPlayContinuedInZone)^2)+
  mean((shots2020$shotPlayContinuedOutsideZone-shots2020$xPlayContinuedOutsideZone)^2)+
    mean((shots2020$shotPlayStopped-shots2020$xPlayStopped)^2)





# Test for test data

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
         angle_org = shotAngleAdjusted,
         distance_org = arenaAdjustedShotDistance,
         arenaAdjustedShotDistance=scale(arenaAdjustedShotDistance),
         shotAngleAdjusted=scale(shotAngleAdjusted))

recent_season<- recent_season %>% sample_frac(0.1) %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42))

rebound_all<-read_rds("model/rebound_all.rds")
goal_all<-read_rds("model/goal_all.rds")
froze_all<-read_rds("model/froze_all.rds")
inZone_all<-read_rds("model/inZone_all.rds")
stop_all<-read_rds("model/stop_all.rds")


all_gl<- tibble(rebound_all = exp(predict(rebound_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                goal_all = exp(predict(goal_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                froze_all = exp(predict(froze_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                inZone_all = exp(predict(inZone_all,newdata=recent_season, type = "link",allow.new.levels = TRUE)),
                stop_all = exp(predict(stop_all,newdata=recent_season, type = "link",allow.new.levels = TRUE))) 

all_gl<- all_gl %>% 
  mutate(PlayOutsideZone=1/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Rebound=rebound_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Goal= goal_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         GoalieFroze=froze_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         PlayInZone=inZone_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all),
         Stop=stop_all/(1+rebound_all+goal_all+froze_all+inZone_all+stop_all))

all_gl<-cbind(all_gl,recent_season[,15:16],recent_season[,27])
colnames(all_gl)[14]<-"outcome_type"


a<-mean(((all_gl$outcome_type=="Goal")-all_gl$Goal)^2)+
  mean(((all_gl$outcome_type=="Rebound")-all_gl$Rebound)^2)+
  mean(((all_gl$outcome_type=="GoalieFroze")-all_gl$GoalieFroze)^2)+
  mean(((all_gl$outcome_type=="PlayInZone")-all_gl$PlayInZone)^2)+
  mean(((all_gl$outcome_type=="Stop")-all_gl$Stop)^2)+
  mean(((all_gl$outcome_type=="PlayOutsideZone")-all_gl$PlayOutsideZone)^2)
