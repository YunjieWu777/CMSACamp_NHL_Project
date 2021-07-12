
# I only use shots2020 as the sample for quick computation

# Load packages and data --------------------------------------------------
library(tidyverse)
shots2020 <- read_csv("data/shots_2020.csv")


# Filter data -------------------------------------------------------------
variable<- c("arenaAdjustedShotDistance","shotWasOnGoal","shotPlayStopped","shotAngle","shotGoalieFroze","shotPlayContinuedInZone","shotPlayContinuedOutsideZone","awaySkatersOnIce","homeSkatersOnIce","season","shotAngleAdjusted","arenaAdjustedShotDistance","goal","shotGeneratedRebound", "xCordAdjusted", "yCordAdjusted","shotType","shotRush","shotRebound")

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


# Goal --------------------------------------------------------------------
xG_logit_even <- glm(goal ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                     data = evenstrength,
                     family = binomial("logit"))

summary(xG_logit_even)

# Rebound  ----------------------------------------------------------------
reb_logit_even <- glm(shotGeneratedRebound ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                      data = evenstrength,
                      family = binomial("logit"))
summary(reb_logit_even)



# Freeze ------------------------------------------------------------------
froze_logit_even <- glm(shotGoalieFroze ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                        data = evenstrength,
                        family = binomial("logit"))
summary(froze_logit_even)


# InZone ------------------------------------------------------------------
inZone_logit_even <- glm(shotPlayContinuedInZone ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                         data = evenstrength,
                         family = binomial("logit"))
summary(inZone_logit_even)


# OutofZone ---------------------------------------------------------------
outZone_logit_even <- glm(shotPlayContinuedOutsideZone ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                          data = evenstrength,
                          family = binomial("logit"))
summary(outZone_logit_even)


# Stopped -----------------------------------------------------------------
stop_logit_even <- glm(shotPlayStopped ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                       data = evenstrength,
                       family = binomial("logit"))
summary(stop_logit_even)



# ShotOnGoal% -------------------------------------------------------------
shotOnGoal_logit_even <- glm(shotWasOnGoal ~                        shotAngleAdjusted+arenaAdjustedShotDistance+shotType+shotRush+shotRebound,
                             data = evenstrength,
                             family = binomial("logit"))
summary(shotOnGoal_logit_even)


# Heat Map ----------------------------------------------------------------

