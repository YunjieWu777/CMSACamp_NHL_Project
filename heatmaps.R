# PURPOSE: Tasks for Week 7

library(tidyverse)
library(foreign)
library(reshape2)


# Reading in the data -----------------------------------------------------


shots2020 <- read_csv("data/shots_2020.csv")

load("shots1019.RData")

evenstrength <- 
  shots1019 %>% 
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42)) %>% 
  filter(homeSkatersOnIce==5 & awaySkatersOnIce==5)

ongoal <-
  evenstrength %>%
  filter(shotWasOnGoal == 1)


ongoal <-
  ongoal %>%
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
                 data = ongoal)

# LOSO Preds --------------------------------------------------------------



init_loso_cv_preds <-
  map_dfr(unique(ongoal$season),
          function(x) {
            test_data <- ongoal %>%
              filter(season == x)
            train_data <- ongoal %>%
              filter(season != x)
            
            exp_model <-
              multinom(Outcome2 ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                         shotType+shotRush+shotRebound,
                       data = train_data)
            predict(exp_model, newdata = test_data, type = "probs") %>%
              as_tibble() %>%
              mutate(Outcome2 = test_data$Outcome2,
                     season = x)
          })


# LOSO Preds with X and Y Coordinates

second_loso_cv_preds <-
  map_dfr(unique(ongoal$season),
          function(x) {
            test_data <- ongoal %>%
              filter(season == x)
            train_data <- ongoal %>%
              filter(season != x)
            
            exp_model <-
              multinom(Outcome2 ~ shotAngleAdjusted+arenaAdjustedShotDistance+
                         shotType+shotRush+shotRebound,
                       data = train_data)
            predict(exp_model, newdata = test_data, type = "probs") %>%
              as_tibble() %>%
              mutate(Outcome2 = test_data$Outcome2,
                     season = x,
                     xcord = test_data$xCordAdjusted,
                     ycord = test_data$yCordAdjusted)
          })

# Plots

cv_loso_calibration_results <- init_loso_cv_preds %>%
  pivot_longer(Goal:PlayStopped,
               names_to = "next_score_type",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  group_by(next_score_type, bin_pred_prob) %>%
  summarize(n_plays = n(),
            n_scoring_event = length(which(Outcome2 == next_score_type)),
            bin_actual_prob = n_scoring_event / n_plays,
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_plays)) %>%
  ungroup() %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0))

cv_loso_calibration_results %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(se = FALSE) + 
  geom_point(aes(size = n_plays)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + #coord_equal() +   
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays", x = "Estimated next score probability", 
       y = "Observed next score probability") + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        legend.position = c(1, .05), legend.justification = c(1, 0)) +
  facet_wrap(~ next_score_type, ncol = 4)





cv_loso_calibration_results <- second_loso_cv_preds %>%
  pivot_longer(Goal:PlayStopped,
               names_to = "next_score_type",
               values_to = "pred_prob") %>%
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  group_by(next_score_type, bin_pred_prob) %>%
  summarize(n_plays = n(),
            n_scoring_event = length(which(Outcome2 == next_score_type)),
            bin_actual_prob = n_scoring_event / n_plays,
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_plays)) %>%
  ungroup() %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0))

cv_loso_calibration_results %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(se = FALSE) + 
  geom_point(aes(size = n_plays)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + #coord_equal() +   
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays", x = "Estimated next score probability", 
       y = "Observed next score probability") + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        legend.position = c(1, .05), legend.justification = c(1, 0)) +
  facet_wrap(~ next_score_type, ncol = 4)


# Heat maps ---------------------------------------------------------------

source('rink.r')  
g <- rink 

# Goal

plot1 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = Goal,
                       color = Goal),
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

# Generates Rebound

plot2 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = GeneratesRebound,
                       color = GeneratesRebound),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                      high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Generates Rebound",
       fill = "Probability") +
  theme_bw()

# Goalie Froze

plot3 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = GoalieFroze,
                       color = GoalieFroze),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(title = "Goalie Froze Puck",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play in Zone

plot4 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayInZone,
                       color = PlayInZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                      high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play in Zone",
       fill = "Probability") +
  theme_bw()

# Play Outside Zone

plot5 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayOutsideZone,
                       color = PlayOutsideZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(title = "Play Outside Zone",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play Stopped

plot6 <- g + 
  stat_summary_hex(data = second_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayStopped,
                       color = PlayStopped),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                      high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play Stopped",
       fill = "Probability") +
  theme_bw()

library(gridExtra)
heatmaps <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
                        nrow = 2)


# With missed net

load("all_loso_cv_preds.RData")

all_loso_cv_preds <-
  all_loso_cv_preds %>%
  filter(xcord <= 90)

# Goal

plot1 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = Goal,
                       color = Goal),
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

# Generates Rebound

plot2 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = GeneratesRebound,
                       color = GeneratesRebound),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Generates Rebound",
       fill = "Probability") +
  theme_bw()

# Goalie Froze

plot3 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = GoalieFroze,
                       color = GoalieFroze),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Goalie Froze Puck",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play in Zone

plot4 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayInZone,
                       color = PlayInZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play in Zone",
       fill = "Probability") +
  theme_bw()

# Play Outside Zone

plot5 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayOutsideZone,
                       color = PlayOutsideZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Play Outside Zone",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play Stopped

plot6 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayStopped,
                       color = PlayStopped),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play Stopped",
       fill = "Probability") +
  theme_bw()

plot7 <- g + 
  stat_summary_hex(data = all_loso_cv_preds,
                   aes(x = ycord,
                       y = xcord,
                       z = Miss,
                       color = Miss),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Missed Net",
       fill = "Probability") +
  theme_bw()

library(gridExtra)
heatmaps <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                                    nrow = 2)
# All shots, missed net not an outcome

load("all_loso_cv_preds2.RData")

all_loso_cv_preds2 <-
  all_loso_cv_preds2 %>%
  filter(xcord <= 90)

# Goal

plot1 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = Goal,
                       color = Goal),
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

# Generates Rebound

plot2 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = GeneratesRebound,
                       color = GeneratesRebound),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Generates Rebound",
       fill = "Probability") +
  theme_bw()

# Goalie Froze

plot3 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = GoalieFroze,
                       color = GoalieFroze),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Goalie Froze Puck",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play in Zone

plot4 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayInZone,
                       color = PlayInZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play in Zone",
       fill = "Probability") +
  theme_bw()

# Play Outside Zone

plot5 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayOutsideZone,
                       color = PlayOutsideZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Play Outside Zone",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play Stopped

plot6 <- g + 
  stat_summary_hex(data = all_loso_cv_preds2,
                   aes(x = ycord,
                       y = xcord,
                       z = PlayStopped,
                       color = PlayStopped),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play Stopped",
       fill = "Probability") +
  theme_bw()


library(gridExtra)
heatmaps <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
                                    nrow = 2)


load("rf_preds.RData")


# Goal

plot1 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = Goal,
                       color = Goal),
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

# Generates Rebound

plot2 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = GeneratesRebound,
                       color = GeneratesRebound),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Generates Rebound",
       fill = "Probability") +
  theme_bw()

# Goalie Froze

plot3 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = GoalieFroze,
                       color = GoalieFroze),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Goalie Froze Puck",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play in Zone

plot4 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = PlayInZone,
                       color = PlayInZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play in Zone",
       fill = "Probability") +
  theme_bw()

# Play Outside Zone

plot5 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = PlayOutsideZone,
                       color = PlayOutsideZone),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  labs(title = "Play Outside Zone",
       fill = "Probability") +
  ylim(25, 100) +
  theme_bw()

# Play Stopped

plot6 <- g + 
  stat_summary_hex(data = rf_preds,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       z = PlayStopped,
                       color = PlayStopped),
                   binwidth = c(3,3),
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  scale_color_gradient(low = "darkblue",
                       high = "darkorange") +
  ylim(25, 100) +
  labs(title = "Play Stopped",
       fill = "Probability") +
  theme_bw()


library(gridExtra)
heatmaps <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
                                    nrow = 2)


# Testing different tables ------------------------------------------------


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



