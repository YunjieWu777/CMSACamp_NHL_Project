# Load packages and data --------------------------------------------------
library(tidyverse)
library(ggthemes)
library(haven)
library(nnet)
library(sjPlot)
shots2020 <- read_csv("data/shots_2020.csv")
shots0719 <- read.csv("data/shots_2007-2019.csv")

# Filter data -------------------------------------------------------------
shots1019 <- shots0719 %>% 
  filter(season>2009)
variable<- c("xGoal","arenaAdjustedShotDistance","shotWasOnGoal","shotPlayStopped","shotAngle","shotGoalieFroze","shotPlayContinuedInZone","shotPlayContinuedOutsideZone","awaySkatersOnIce","homeSkatersOnIce","season","shotAngleAdjusted","arenaAdjustedShotDistance","goal","shotGeneratedRebound", "xCordAdjusted", "yCordAdjusted","shotType","shotRush","shotRebound")

recent_season <- rbind(select(shots1019,all_of(variable)),select(shots2020,all_of(variable)))

evenstrength <- 
  recent_season %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

powerplay <-
  recent_season %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter((homeSkatersOnIce == 4 & awaySkatersOnIce == 5)|
           (homeSkatersOnIce == 5 & awaySkatersOnIce == 4))

# 2020 Sample
evenstrength <- 
  shots2020 %>% 
  select(all_of(variable))%>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

unique(shots2020$shotType)

a<-shots2020 %>% 
  filter(is.na(shotType))



shots2020$goal

powerplay <-
  shots2020 %>% 
  select(all_of(variable))%>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter((homeSkatersOnIce == 4 & awaySkatersOnIce == 5)|
           (homeSkatersOnIce == 5 & awaySkatersOnIce == 4))
#

# Goal --------------------------------------------------------------------
xG_logit_even <- glm(goal ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                     data = evenstrength,
                     family = binomial("logit"))

tab_model(xG_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")

# Rebound  ----------------------------------------------------------------
reb_logit_even <- glm(shotGeneratedRebound ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                      data = evenstrength,
                      family = binomial("logit"))
tab_model(reb_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")



# Freeze ------------------------------------------------------------------
froze_logit_even <- glm(shotGoalieFroze ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                        data = evenstrength,
                        family = binomial("logit"))
tab_model(froze_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")


# InZone ------------------------------------------------------------------
inZone_logit_even <- glm(shotPlayContinuedInZone ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                         data = evenstrength,
                         family = binomial("logit"))
tab_model(inZone_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")


# OutofZone ---------------------------------------------------------------
outZone_logit_even <- glm(shotPlayContinuedOutsideZone ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                          data = evenstrength,
                          family = binomial("logit"))
tab_model(outZone_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")


# Stopped -----------------------------------------------------------------
stop_logit_even <- glm(shotPlayStopped ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                       data = evenstrength,
                       family = binomial("logit"))
tab_model(stop_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")



# ShotOnGoal% -------------------------------------------------------------
shotOnGoal_logit_even <- glm(shotWasOnGoal ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                             data = evenstrength,
                             family = binomial("logit"))

tab_model(shotOnGoal_logit_even,transform = NULL,
          show.se = TRUE, show.stat = TRUE, show.ci = FALSE, show.aic = TRUE,
          digits.p = 2, digits = 4, df.method = "wald")


# Heat Map ----------------------------------------------------------------





# Multi -------------------------------------------------------------------

recent_season <- rbind(select(shots1019,all_of(variable)),select(shots2020,all_of(variable)))

multi <- recent_season %>% 
  filter(shotWasOnGoal==1,
         !is.na(shotType),
         shotType!="")
         
         
multi<-multi %>% 
  mutate(outcome=case_when(shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"))

# 21 observation that is both goal and froze, 5 NA shotType and 164 "" shotType
# for 2 of 164 "", all 6 outcomes are 0.



multi$outcome <- relevel(as.factor(multi$outcome),ref="Goal")
multi$shotType <- as.factor(multi$shotType)
levels(multi$outcome)

multi_mo <- multinom(outcome ~ shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound, 
                     data = multi,
                     model=TRUE)
summary(multi_mo)

head(multi_mo$fitted.values,5)

chisq.test(multi$outcome,predict(multi_mo))

library(summarytools)
# Build a classification table by using the ctable function
ctable <- table(multi$outcome,predict(multi_mo))
ctable


# test --------------------------------------------------------------------
library(gt)
knitr::kable(head(multi_mo$fitted.values,5))

hometeam <- subset(recent_season, team == "HOME")


awayteam <- subset(recent_season, team = "AWAY")


evenstrength <-  recent_season %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
powerplay <-  recent_season %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 4,
         isHomeTeam == 1)
penaltykill <-  recent_season %>%
  filter(homeSkatersOnIce == 4,
         awaySkatersOnIce == 5,
         isHomeTeam == 1)


backhand <- recent_season %>%
  filter(shotType == "BACK")
deflection <- recent_season %>%
  filter(shotType == "DEFL")
slapshot <- recent_season %>%
  filter(shotType == "SLAP")
snapshot <- recent_season %>%
  filter(shotType == "SNAP")
tip <- recent_season %>%
  filter(shotType == "TIP")
wristshot <- recent_season %>%
  filter(shotType == "WRIST")
wrap <- recent_season %>%
  filter(shotType == "WRAP")


forward <- recent_season %>%
  filter(Position == "F")
defense <- recent_season %>%
  filter(Position == "D")
lefthand <- subset(recent_season, shooterLeftRight == "L")
righthand <- subset(shots, shooterLeftRight == "R")

SJ <- 
  shots2020 %>%
  filter(homeTeamCode == "S.J")
CHI <- 
  shots %>%
  filter(homeTeamCode == "CHI")
ANA <-
  shots %>%
  filter(homeTeamCode == "ANA")
DET <-
  shots %>%
  filter(homeTeamCode == "DET")
VGK <-
  shots %>%
  filter(homeTeamCode == "VGK")

BOS <-
  shots %>%
  filter(homeTeamCode == "BOS")
PIT <-
  shots %>%
  filter(homeTeamCode == "PIT")


theme_reach <- function() {
  theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16)
    )
}

recent_season %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_reach()+ 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts")


wristshot %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Wristshot")

slapshot %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Slapshot")

backhand %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Backhand")

snapshot %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Snapshot")

tip %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Tip")

wrap %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Wraparound")

deflection %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4)+
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       plot.title = element_text(size = 20,face = "bold"),
       axis.title.x = element_text(size=16),
       axis.title.y = element_text(size=16),
       title="Deflection")

powerplay %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.05, y = -0.05),
            size = 4) +
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") +
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Powerplay")


evenstrength %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_text(aes(label = n_attempts),
            position = position_nudge(x = 0.1, y = -0.01),
            size = 4) +
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") +
  labs(x = "Predicted Probability",
       y = "Actual Probability",
       size = "Number of Attempts",
       title="Even Strength")
