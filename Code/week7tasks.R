# PURPOSE: Tasks for Week 7

library(tidyverse)


# Reading in the data -----------------------------------------------------


shots2020 <- read_csv("data/shots_2020.csv")
shots0719 <- read.csv("data/shots_2007-2019.csv")

# Filter data
shots1019 <- shots0719 %>% 
  filter(season>2009)

recent_season <- rbind(select(shots1019,shotGoalieFroze,
                              shotPlayContinuedInZone,
                              shotPlayContinuedOutsideZone,season,
                              shotAngleAdjusted,arenaAdjustedShotDistance,
                              goal,shotGeneratedRebound, xCordAdjusted, 
                              yCordAdjusted, shotAngle,
                              shotWasOnGoal, homeSkatersOnIce,
                              awaySkatersOnIce),
                       select(shots2020,shotGoalieFroze,
                              shotPlayContinuedInZone,
                              shotPlayContinuedOutsideZone,season,
                              shotAngleAdjusted,arenaAdjustedShotDistance,
                              goal,shotGeneratedRebound, xCordAdjusted, 
                              yCordAdjusted, shotAngle,
                              shotWasOnGoal, homeSkatersOnIce,
                              awaySkatersOnIce))

dat_list = split(recent_season, recent_season$season)

offense_shots <-
  recent_season %>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42))

recent_season <-
  recent_season %>%
  filter(xCordAdjusted %in% c(25:89),
         yCordAdjusted %in% c(-42:42))


# Splitting data into 5 on 5, 5 on 4 subsets ------------------------------

evenstrength <-
  recent_season %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)
  
powerplay <-
  recent_season %>%
  filter((homeSkatersOnIce == 4 | awaySkatersOnIce == 4),
         (homeSkatersOnIce == 5 | awaySkatersOnIce == 5),
          (homeSkatersOnIce > 3),
          (homeSkatersOnIce < 6),
          (awaySkatersOnIce > 3),
          (awaySkatersOnIce < 6),
          )


# Logistic Regression Models ----------------------------------------------

# Goal %

xg_logit_even <- glm(goal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                                 data = evenstrength,
                                 family = binomial("logit")) 

summary(xg_logit_even)

xg_logit_odd <- glm(goal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                     data = powerplay,
                     family = binomial("logit")) 

summary(xg_logit_odd)

# Rebound

rebound_logit_even <- glm(shotGeneratedRebound ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                     data = evenstrength,
                     family = binomial("logit")) 

summary(rebound_logit_even)

rebound_logit_odd <- glm(shotGeneratedRebound ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                    data = powerplay,
                    family = binomial("logit")) 

summary(rebound_logit_odd)

# Goalie Freeze

freeze_logit_even <- glm(shotGoalieFroze ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                     data = evenstrength,
                     family = binomial("logit")) 

summary(freeze_logit_even)

freeze_logit_odd <- glm(shotGoalieFroze ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                    data = powerplay,
                    family = binomial("logit")) 

summary(freeze_logit_odd)


# Play in Zone

inzone_logit_even <- glm(shotPlayContinuedInZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                         data = evenstrength,
                         family = binomial("logit")) 

summary(inzone_logit_even)

inzone_logit_odd <- glm(shotPlayContinuedInZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                        data = powerplay,
                        family = binomial("logit")) 

summary(inzone_logit_odd)

# Play Out Zone

outzone_logit_even <- glm(shotPlayContinuedOutsideZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                         data = evenstrength,
                         family = binomial("logit")) 

summary(outzone_logit_even)

outzone_logit_odd <- glm(shotPlayContinuedOutsideZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                        data = powerplay,
                        family = binomial("logit")) 

summary(outzone_logit_odd)

# Shot On Goal

ongoal_logit_even <- glm(shotWasOnGoal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                         data = evenstrength,
                         family = binomial("logit")) 

summary(ongoal_logit_even)

ongoal_logit_odd <- glm(shotWasOnGoal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                        data = powerplay,
                        family = binomial("logit")) 

summary(ongoal_logit_odd)



# Heat Maps ---------------------------------------------------------------

source('rink.r')  
g <- rink 
plot1 <- g + 
  stat_summary_hex(data = evenstrength,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       # fill = after_stat(level)),
                       z = xg_logit_even$fitted.values),
                   binwidth = c(3,3),
                   color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw()

plot2 <- g + 
  stat_summary_hex(data = evenstrength,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       # fill = after_stat(level)),
                       z = rebound_logit_even$fitted.values),
                   binwidth = c(3,3),
                   color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw()

plot3 <- g + 
  stat_summary_hex(data = evenstrength,
                   aes(x = yCordAdjusted,
                       y = xCordAdjusted,
                       # fill = after_stat(level)),
                       z = freeze_logit_even$fitted.values),
                   binwidth = c(3,3),
                   color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw()




