# Load packages and data --------------------------------------------------
library(tidyverse)
<<<<<<< HEAD
=======
library(haven)
library(nnet)
>>>>>>> 17b7523ffe91a21e006852d5c224bfd49b523bd3
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

unique(shots2020$shotType)

a<-shots2020 %>% 
  filter(is.na(shotType))



shots2020$goal

powerplay <-
  shots2020 %>% 
  select(all_of(variable))%>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
<<<<<<< HEAD
  filter((homeSkatersOnIce == 4 | awaySkatersOnIce == 4),
         (homeSkatersOnIce == 5 | awaySkatersOnIce == 5),
         (homeSkatersOnIce > 3),
         (homeSkatersOnIce < 6),
         (awaySkatersOnIce > 3),
         (awaySkatersOnIce < 6),
  )
=======
  filter((homeSkatersOnIce == 4 & awaySkatersOnIce == 5)|
           (homeSkatersOnIce == 5 & awaySkatersOnIce == 4))
>>>>>>> 17b7523ffe91a21e006852d5c224bfd49b523bd3
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





<<<<<<< HEAD
=======
# Multi -------------------------------------------------------------------

recent_season <- rbind(select(shots1019,all_of(variable)),select(shots2020,all_of(variable)))

multi <- recent_season %>% 
  filter(shotWasOnGoal==1,
         !is.na(shotType),
         shotType!="")

# 21 observation that is both goal and froze, 5 NA shotType and 164 "" shotType
# for 2 of 164 "", all 6 outcomes are 0.
multi<-multi %>% 
  mutate(outcome=case_when(shotGoalieFroze==1 ~ "GoalieFroze",
                           goal==1 ~"Goal",
                           shotGeneratedRebound == 1 ~ "GeneratesRebound",
                           shotPlayContinuedInZone == 1 ~ "PlayInZone",
                           shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                           shotPlayStopped == 1 ~ "PlayStopped"))


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
>>>>>>> 17b7523ffe91a21e006852d5c224bfd49b523bd3



