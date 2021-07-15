# PURPOSE: Tasks for Week 7

library(tidyverse)
library(foreign)
library(reshape2)


# Reading in the data -----------------------------------------------------


shots2020 <- read_csv("data/shots_2020.csv")

evenstrength <- 
  shots2020 %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

ongoal <-
  evenstrength %>%
  filter(shotWasOnGoal == 1)


ongoal <-
  evenstrength %>%
  mutate(
    Outcome = case_when(shotGoalieFroze == 1 ~ "GoalieFroze",
                        goal == 1 ~ "Goal",
                        shotGeneratedRebound == 1 ~ "GeneratesRebound",
                        shotPlayContinuedInZone == 1 ~ "PlayInZone",
                        shotPlayContinuedOutsideZone == 1 ~ "PlayOutsideZone",
                        shotPlayStopped == 1 ~ "PlayStopped"))
  
ongoal$Outcome

library(nnet)

ongoal <-
  ongoal %>%
  mutate(Outcome = as.factor(Outcome))

ongoal <-
  ongoal %>%
  filter(!is.na(shotType),
         !(shotType == ""))

ongoal$Outcome2 <- 
         relevel(ongoal$Outcome, ref = "Goal")

test <- multinom(Outcome2 ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                   shotType+shotRush+shotRebound,
                 data = ongoal,
                 Hess = TRUE)

summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

head(pp <- fitted(test))

library(stargazer)

stargazer(test, type = "html", out = "test.htm")
stargazer((summary(test)$coefficients/summary(test)$standard.errors), type = "html", 
          out = "ztest.htm")

# Relative Risk Ratios ----------------------------------------------------

test_rrr <- exp(coef(test))
stargazer(test, type = "html", coef = list(test_rrr), p.auto = FALSE,
          out = "testrrr.htm")



library(summarytools)
# Build a classification table by using the ctable function


# ordinal logit model: predicted probabilities ----------------------------

allmean <-
  data.frame(shotAngleAdjusted = rep(mean(ongoal$shotAngleAdjusted), 7),
             arenaAdjustedDistance = rep(mean(ongoal$arenaAdjustedShotDistance), 7),
             shotRush = rep(mean(ongoal$shotRush), 7),
             shotRebound = rep(mean(ongoal$shotRebound), 7),
             shotType = c("BACK", "DEFL", "SLAP",
                          "SNAP", "TIP", "WRAP",
                          "WRIST"))

allmean[, c("pred.prob")] <- predict(test, newdata = allmean, type = "probs")

# prog2 ~ ses + write, data = ml



# Predicted probabilities for shot type -----------------------------------


dshot <- data.frame(shotType = c("BACK", "DEFL", "SLAP",
                                 "SNAP", "TIP", "WRAP",
                                 "WRIST"), 
                    shotAngleAdjusted = rep(mean(ongoal$shotAngleAdjusted)),
                    arenaAdjustedShotDistance = rep(mean(ongoal$arenaAdjustedShotDistance)),
                    shotRush = rep(mean(ongoal$shotRush)),
                    shotRebound = rep(mean(ongoal$shotRebound))
                    )

predict(test, newdata = dshot, "probs")

stargazer(predict(test, newdata = dshot, "probs"), type = "html", out = "probs.htm")



dangle <- data.frame(shotType = rep(c("BACK", "DEFL", "SLAP",
                                  "SNAP", "TIP", "WRAP",
                                  "WRIST"), each = ), 
          shotAngleAdjusted = rep(c(0:80), 7),
          arenaAdjustedShotDistance = rep(c(0:50), 7),
          shotRush = rep(c(0:1), 7),
          shotRebound = rep(c(0:1), 7))

ppangle <- cbind(dangle, predict(test, newdata = dangle, type = "probs"))

by(ppangle[, 2:5], ppangle$shotType, colMeans)

library(effects)

plot(Effect("shotType", test))
plot(Effect("shotType", test), multiline = T)
plot(Effect("shotType", test), style = "stacked")


library(sjPlot)
library(sjmisc)


test2 <- multinom(Outcome2 ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                   shotType+shotRush+shotRebound+shotType*shotAngleAdjusted,
                 data = ongoal,
                 Hess = TRUE)
plot_model(test, type = "pred", terms = c("arenaAdjustedShotDistance", "shotType")) +
  theme_bw()

plot_model(test2, type = "pred", terms = c("shotAngleAdjusted [all]", "shotType")) +
  theme_bw()



