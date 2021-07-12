# Load packages and data --------------------------------------------------
library(tidyverse)
library(sjPlot)
shots2020 <- read_csv("data/shots_2020.csv")
shots0719 <- read.csv("data/shots_2007-2019.csv")

# Filter data -------------------------------------------------------------
shots1019 <- shots0719 %>% 
  filter(season>2009)
variable<- c("arenaAdjustedShotDistance","shotWasOnGoal","shotPlayStopped","shotAngle","shotGoalieFroze","shotPlayContinuedInZone","shotPlayContinuedOutsideZone","awaySkatersOnIce","homeSkatersOnIce","season","shotAngleAdjusted","arenaAdjustedShotDistance","goal","shotGeneratedRebound", "xCordAdjusted", "yCordAdjusted","shotType","shotRush","shotRebound")

recent_season <- rbind(select(shots1019,all_of(variable)),select(shots2020,all_of(variable)))
evenstrength <- 
  recent_season %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

# 2020 Sample
evenstrength <- 
  shots2020 %>% 
  select(all_of(variable))%>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

powerplay <-
  shots2020 %>% 
  select(all_of(variable))%>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter((homeSkatersOnIce == 4 | awaySkatersOnIce == 4),
         (homeSkatersOnIce == 5 | awaySkatersOnIce == 5),
         (homeSkatersOnIce > 3),
         (homeSkatersOnIce < 6),
         (awaySkatersOnIce > 3),
         (awaySkatersOnIce < 6),
  )
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








