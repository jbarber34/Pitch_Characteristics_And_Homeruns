library(ggplot2)
library(RColorBrewer)

# Read in all fastballs from 2017
Y17_All_Pitches <- read.csv('/Users/James/OneDrive - oaklandathletics/R Studio/Pitcher Type Project/2017 All Pitches Thrown Clean.csv',na.strings = 'null')

# Remove NAs in pfx_x
Y17_All_Pitches<-Y17_All_Pitches[complete.cases(Y17_All_Pitches[,c(15)]),]

# Filter out negative pitch speeds
Y17_All_Pitches <- Y17_All_Pitches[Y17_All_Pitches$release_speed > 1, ]

# Create HR or No HR Column
Y17_All_Pitches$Result <- as.numeric(Y17_All_Pitches$events == "home_run")
Y17_All_Pitches$in_play <- !is.na(Y17_All_Pitches$events) & 
  !is.na(Y17_All_Pitches$launch_speed)
Balls_in_play_17 <- Y17_All_Pitches[Y17_All_Pitches$in_play, ]

#Transformation of Break Variable by Batter Stand
Y17_All_Pitches$pfx_x_batter <- Y17_All_Pitches$pfx_x

Y17_All_Pitches$pfx_x_batter[Y17_All_Pitches$stand == 'L'] <-
  -1 * Y17_All_Pitches$pfx_x[Y17_All_Pitches$stand == 'L']

#Fit better logistic regression with quadratic transformation of launch_angle
Hit_Quality_Model_17 <- 
  glm(Result ~ launch_speed + poly(launch_angle, degree = 2, raw = TRUE),
      data = Balls_in_play_17, family = binomial(link = "logit"))

# Create a vector of all NA's
Y17_All_Pitches$Hit_Quality_17 <- NA
# For all balls hit into play, get their quality of contact from the model
Y17_All_Pitches$Hit_Quality_17[Y17_All_Pitches$in_play] <- Hit_Quality_Model_17$fitted.values

# Subset into just Fastballs
Y17_Fastballs <- subset(Y17_All_Pitches, pitch_type == "FT" | pitch_type == "FF" 
                        | pitch_type == "SI" | pitch_type ==  "FS" | pitch_type ==  "FC")

#Logistic Regression Model of Fastball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_17 <- 
  lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y17_Fastballs)

# Get Pitch Quality for all fastballs thrown
Y17_Fastballs$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17,
                                          newdata = Y17_Fastballs)

# Put All Fastballs into a new data frame
F_plot_data_17 <- Y17_Fastballs

# Control the vertical movement variable and spin rate for Fastball by finding mean
F_plot_data_17$pfx_z <- median(F_plot_data_17$pfx_z, na.rm = TRUE)
F_plot_data_17$release_spin_rate <- median(F_plot_data_17$release_spin_rate, na.rm = TRUE)

# Find new pitch quaility with median of pfx_z
F_plot_data_17$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17, newdata = F_plot_data_17)

# Control Fastball most frequent zone which isolates release_speed and pfx_x_batter
F_plot_data_17 <- F_plot_data_17[F_plot_data_17$zone == 14, ]

# Plot Fastball results, faster pitches in on the hands are toughest to hit for home runs
# Slower pitches moving away are easier to hit.
ggplot(F_plot_data_17, aes(pfx_x_batter, release_speed, col = Pitch_Quality_17)) +
  stat_summary_hex(aes(z = Pitch_Quality_17), bins = 100)+
  ggtitle("2017 Fastball Pitch Quality Model")

# Plot Fastball hit quality
ggplot(Y17_Fastballs, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
  ggtitle("2017 Fastball Hit Quality Model")

#Fastball Pitch Quality by zone
#Both <85 and >100 had high variability in quality, 85-100mph most consistent pitch quality
ggplot(Y17_Fastballs, aes(release_speed, Pitch_Quality_17, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_speed", limits = c(62, 106),
                     breaks = seq(60, 106, 5)) +
  labs(title="2017 Fastball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between pitch speed and pitch quality in each zone",
       x = "Pitch Speed",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Breaking Balls
Y17_BreakingBall <- subset(Y17_All_Pitches, pitch_type == "SL" | pitch_type == "CU" 
                           | pitch_type == "KC")

#Logistic Regression Model of breaking ball home run probability from spin rate, location, and movement
Pitch_Quality_Model_17 <- 
  lm(Hit_Quality_17 ~ release_spin_rate + release_speed + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y17_BreakingBall)

# Get Pitch Quality for all breaking ball thrown
Y17_BreakingBall$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17,
                                             newdata = Y17_BreakingBall)

# Put All Breaking Balls into a new data frame
BB_plot_data_17 <- Y17_BreakingBall

# Control release speed variable
BB_plot_data_17$release_speed <- median(BB_plot_data_17$release_speed, na.rm = TRUE)

# Find new pitch quaility with median of variable
BB_plot_data_17$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17, newdata = BB_plot_data_17)

# Control Breaking Ball most frequent zone which isolates variables
BB_plot_data_17 <- BB_plot_data_17[BB_plot_data_17$zone == 14, ]

# Plot Pitch Quality results with isolated spin rate, vertical and horizontal movement variables
#Pitches with high spin rate, lower in the zone are not hit as well as 
# pitches with lower spin rate w/less downward vert. movement.
ggplot(BB_plot_data_17, aes(release_spin_rate, pfx_z, col = Pitch_Quality_17)) +
  stat_summary_hex(aes(z = Pitch_Quality_17), bins = 100)+
  ggtitle("2017 Breaking Ball Pitch Quality Model")

# Plot Breaking Ball hit quality
ggplot(Y17_BreakingBall, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
  ggtitle("2017 Breaking Ball Hit Quality Model")

# Breaking Ball pitch quality by zone
# Lower variabilty 2050-2650 rotations per minute. Highest quality in zone 2, 5
ggplot(Y17_BreakingBall, aes(release_spin_rate, Pitch_Quality_17, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_spin_rate", limits = c(700, 3500),
                     breaks = seq(650, 3500, 100)) +
  labs(title="2017 Breaking Ball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between spin rate and pitch quality in each zone",
       x = "Spin Rate",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Off Speed
Y17_OffSpeed <- subset(Y17_All_Pitches, pitch_type == "CH")

#Logistic Regression Model of breaking ball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_17 <- 
  lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y17_OffSpeed)

# Get Pitch Quality for all breaking ball thrown
Y17_OffSpeed$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17,
                                         newdata = Y17_OffSpeed)

# Put All Off Speed into a new data frame
OS_plot_data_17 <- Y17_OffSpeed

# Control hor. movement variable
OS_plot_data_17$pfx_x_batter <- median(OS_plot_data_17$pfx_x_batter, na.rm = TRUE)

# Find new pitch quaility with median of variable
OS_plot_data_17$Pitch_Quality_17 <- predict(Pitch_Quality_Model_17, newdata = OS_plot_data_17)

# Control Breaking Ball most frequent zone which isolates variables
OS_plot_data_17 <- OS_plot_data_17[OS_plot_data_17$zone == 14, ]

# Plot Pitch Quality results with isolated release speed, spin rate, and hor. movement variables
# Pitches higher w/ less vert. movement and slower are more likely to be high for 
#power than more downward vert. movement and closer to 90mph
ggplot(OS_plot_data_17, aes(release_speed, pfx_z, col = Pitch_Quality_17)) +
  stat_summary_hex(aes(z = Pitch_Quality_17), bins = 100)+
  ggtitle("2017 Breaking Ball Pitch Quality Model")

# Plot Off Speed hit quality
ggplot(Y17_OffSpeed, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
  ggtitle("2017 Off Speed Hit Quality Model")

# Off Speed pitch quality by zone
# Low variability between 80-90mph, highest quality in zones 2,5
ggplot(Y17_OffSpeed, aes(release_speed, Pitch_Quality_17, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_speed", limits = c(62, 106),
                     breaks = seq(60, 106, 5)) +
  labs(title="2017 Off Speed Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between pitch speed and pitch quality in each zone",
       x = "Pitch Speed",
       y = "Pitch Quality",
       fill = "Pitch Zone")


#############################2016 Data########################################

# Read in all fastballs from 2016
Y16_All_Pitches <- read.csv('/Users/James/OneDrive - oaklandathletics/R Studio/Pitcher Type Project/2016 All Pitches Thrown Clean.csv',na.strings = 'null')

# Filter out negative pitch speeds
Y16_All_Pitches <- Y16_All_Pitches[Y16_All_Pitches$release_speed > 1, ]

# Create HR or No HR Column
Y16_All_Pitches$Result <- as.numeric(Y16_All_Pitches$events == "home_run")
Y16_All_Pitches$in_play <- !is.na(Y16_All_Pitches$events) & 
  !is.na(Y16_All_Pitches$launch_speed)
Fastballs_in_play_16 <- Y16_All_Pitches[Y16_All_Pitches$in_play, ]

#Transformation of Break Variable by Batter Stand
Y16_All_Pitches$pfx_x_batter <- Y16_All_Pitches$pfx_x
Y16_All_Pitches$pfx_x_batter[Y16_All_Pitches$stand == 'L'] <-
  -1 * Y16_All_Pitches$pfx_x[Y16_All_Pitches$stand == 'L']

#Fit better logistic regression with quadratic transformation of launch_angle
Hit_Quality_Model_16 <- 
  glm(Result ~ launch_speed + poly(launch_angle, degree = 2, raw = TRUE),
      data = Fastballs_in_play_16, family = binomial(link = "logit"))

# Create a vector of all NA's
Fastballs_in_play_16$Hit_Quality_16 <- NA
# For all balls hit into play, get their quality of contact from the model
Y16_All_Pitches$Hit_Quality_16[Y16_All_Pitches$in_play] <- Hit_Quality_Model_16$fitted.values

# Subset into just Fastballs
Y16_Fastballs <- subset(Y16_All_Pitches, pitch_type == "FT" | pitch_type == "FF" 
                        | pitch_type == "SI" | pitch_type ==  "FS" | pitch_type ==  "FC")

#Logistic Regression Model of Fastball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_16 <- 
  lm(Hit_Quality_16 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y16_Fastballs)

# Get Pitch Quality for all pitches thrown
Y16_Fastballs$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16,
                                          newdata = Y16_Fastballs)
# Put All Pitches into a new data frame
F_plot_data_16 <- Y16_Fastballs

# Control the vertical movement variable by finding mean
F_plot_data_16$pfx_z <- median(F_plot_data_16$pfx_z, na.rm = TRUE)

# Find new pitch quaility with mean of pfx_z
F_plot_data_16$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16, newdata = F_plot_data_16)

#Control most frequent zone which isolates release_speed and pfx_x_batter
F_plot_data_16 <- F_plot_data_16[F_plot_data_16$zone == 14, ]

# Plot Fastball results, faster pitches in on the hands are toughest to hit for home runs
# Slower pitches moving away are easier to hit.
ggplot(F_plot_data_16, aes(pfx_x_batter, release_speed, col = Pitch_Quality_16)) +
  stat_summary_hex(aes(z = Pitch_Quality_16), bins = 100)+
  ggtitle("2016 Fastball Pitch Quality Model")

# Plot Fastball hit quality
ggplot(Y16_Fastballs, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Fastball Hit Quality Model")

#Fastball Pitch Quality by zone
#Both <85 and >100 had high variability in quality, 85-100mph most consistent pitch quality
ggplot(Y16_Fastballs, aes(release_speed, Pitch_Quality_16, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_speed", limits = c(62, 106),
                     breaks = seq(60, 106, 5)) +
  labs(title="2016 Fastball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between pitch speed and pitch quality in each zone",
       x = "Pitch Speed",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Breaking Balls
Y16_BreakingBall <- subset(Y16_All_Pitches, pitch_type == "SL" | pitch_type == "CU" 
                           | pitch_type == "KC")

#Logistic Regression Model of breaking ball home run probability from spin rate, location, and movement
Pitch_Quality_Model_16 <- 
  lm(Hit_Quality_16 ~ release_spin_rate + release_speed + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y16_BreakingBall)

# Get Pitch Quality for all breaking ball thrown
Y16_BreakingBall$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16,
                                             newdata = Y16_BreakingBall)

# Put All Breaking Balls into a new data frame
BB_plot_data_16 <- Y16_BreakingBall

# Control release speed variable
BB_plot_data_16$release_speed <- median(BB_plot_data_16$release_speed, na.rm = TRUE)

# Find new pitch quaility with median of variable
BB_plot_data_16$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16, newdata = BB_plot_data_16)

# Control Breaking Ball most frequent zone which isolates variables
BB_plot_data_16 <- BB_plot_data_16[BB_plot_data_16$zone == 14, ]

# Plot Pitch Quality results with isolated spin rate, vertical and horizontal movement variables
#Pitches higher downward vert. movement are not hit as well as pitches w/less 
#downward vert. movement. Regardless of spin rate
ggplot(BB_plot_data_16, aes(release_spin_rate, pfx_z, col = Pitch_Quality_16)) +
  stat_summary_hex(aes(z = Pitch_Quality_16), bins = 100)+
  ggtitle("2016 Breaking Ball Pitch Quality Model")

# Plot Breaking Ball hit quality
ggplot(Y16_BreakingBall, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Breaking Ball Hit Quality Model")

# Breaking Ball pitch quality by zone
# Lower variabilty 2050-2650 rotations per minute. Highest quality in zone 2, 5
ggplot(Y16_BreakingBall, aes(release_spin_rate, Pitch_Quality_16, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_spin_rate", limits = c(700, 3500),
                     breaks = seq(650, 3500, 100)) +
  labs(title="2016 Breaking Ball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between spin rate and pitch quality in each zone",
       x = "Spin Rate",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Off Speed
Y16_OffSpeed <- subset(Y16_All_Pitches, pitch_type == "CH")

#Logistic Regression Model of breaking ball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_16 <- 
  lm(Hit_Quality_16 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
     data = Y16_OffSpeed)

# Get Pitch Quality for all breaking ball thrown
Y16_OffSpeed$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16,
                                         newdata = Y16_OffSpeed)

# Put All Off Speed into a new data frame
OS_plot_data_16 <- Y16_OffSpeed

# Control hor. movement variable
OS_plot_data_16$pfx_x_batter <- median(OS_plot_data_16$pfx_x_batter, na.rm = TRUE)

# Find new pitch quaility with median of variable
OS_plot_data_16$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16, newdata = OS_plot_data_16)

# Control Breaking Ball most frequent zone which isolates variables
OS_plot_data_16 <- OS_plot_data_16[OS_plot_data_16$zone == 14, ]

# Plot Pitch Quality results with isolated release speed, spin rate, and vert. movement variables
# Pitches higher w/ less vert. movement and slower are more likely to be hit for 
# power than more downward vert. movement and closer to 80 - 90mph
ggplot(OS_plot_data_16, aes(release_speed, pfx_z, col = Pitch_Quality_16)) +
  stat_summary_hex(aes(z = Pitch_Quality_16), bins = 100)+
  ggtitle("2016 Off Speed Pitch Quality Model")

# Plot Off Speed hit quality
ggplot(Y16_OffSpeed, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Off Speed Hit Quality Model")

# Off Speed pitch quality by zone
# Low variability between 80-90mph, highest quality in zones 2,5
ggplot(Y16_OffSpeed, aes(release_speed, Pitch_Quality_16, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_speed", limits = c(62, 106),
                     breaks = seq(60, 106, 5)) +
  labs(title="2016 Off Speed Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between pitch speed and pitch quality in each zone",
       x = "Pitch Speed",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# HIT QUALITY TRENDS: Compare hit quality trends 2016/2017
# FASTBALLS: Use the 2016 model to get hit quality for the 2017 data 
Y17_Fastballs$Hit_Quality_16 <-
  predict(Hit_Quality_Model_16, newdata = Y17_Fastballs, type = 'response')

# Plot both years to check for coorelation, similar but not identical
with(Y17_Fastballs, plot(Hit_Quality_16, Hit_Quality_17))

# Plot difference between hit qualities, the biggest difference is on the borderline batted
# balls, where the home run probability is now as much as 8% more than in 2016
ggplot(Y17_Fastballs, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17 - Hit_Quality_16), bins = 100) +
  ggtitle("Fastball Quality of Contact Trend from 2017 - 2016")

# BREAKING BALL: Use the 2016 model to get hit quality for the 2017 data 
Y17_BreakingBall$Hit_Quality_16 <-
  predict(Hit_Quality_Model_16, newdata = Y17_BreakingBall, type = 'response')

# Plot both years to check for coorelation, similar but not identical
with(Y17_BreakingBall, plot(Hit_Quality_16, Hit_Quality_17))

# Plot difference between hit qualities, the biggest difference is on the borderline and mid range batted
# balls, where the home run probability is now as much as 8% more than in 2016
ggplot(Y17_BreakingBall, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17 - Hit_Quality_16), bins = 100) +
  ggtitle("Breaking Ball Quality of Contact Trend from 2017 - 2016")

# OFF SPEED: Use the 2016 model to get hit quality for the 2017 data 
Y17_OffSpeed$Hit_Quality_16 <-
  predict(Hit_Quality_Model_16, newdata = Y17_OffSpeed, type = 'response')

# Plot both years to check for coorelation, similar but not identical
with(Y17_OffSpeed, plot(Hit_Quality_16, Hit_Quality_17))

# Plot difference between hit qualities, the biggest difference is on the borderline and mid range batted
# balls, where the home run probability is now as much as 8% more than in 2016
ggplot(Y17_OffSpeed, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17 - Hit_Quality_16), bins = 100) +
  ggtitle("Off Speed Quality of Contact Trend from 2017 - 2016")

# PITCH QUALITY TRENDS: Compare pitch quality trends from 2017/2016
# FASTBALLS: Use the 2016 model to get pitch quality for the 2017 data 
F_plot_data_17$Pitch_Quality_16 <-
  predict(Pitch_Quality_Model_16, newdata = F_plot_data_17, type = 'response')

# Plot both years to check for coorelation, similar but not identical for each ind.
#pitch type
with(F_plot_data_17, plot(Pitch_Quality_16, Pitch_Quality_17))

# Plot difference between pitch qualities, slight difference on slower pitches
# moving away, where the home run probability is now as much as 0.5% more than in 2016
ggplot(F_plot_data_17, aes(pfx_x_batter, release_speed)) +
  stat_summary_hex(aes(z = Pitch_Quality_17 - Pitch_Quality_16), bins = 100) +
  labs(title="Fastball Pitch Quality Trend from 2017 - 2016",
       subtitle = "Relationship Between Pitch Speed and Horizontal Movement",
       x = "Horizontal Movement",
       y = "Release Speed")

# BREAKING BALL: Use the 2016 model to get pitch quality for the 2017 data 
BB_plot_data_17$Pitch_Quality_16 <-
  predict(Pitch_Quality_Model_16, newdata = BB_plot_data_17, type = 'response')

# Plot both years to check for coorelation, similar but not identical
with(BB_plot_data_17, plot(Pitch_Quality_16, Pitch_Quality_17))

# Plot difference between pitch qualities, very small difference in pitches with downward
# vert. movement and high spin rate, where the home run probability is now 0.1% higher than in 2016
ggplot(BB_plot_data_17, aes(release_spin_rate, pfx_z)) +
  stat_summary_hex(aes(z = Pitch_Quality_17 - Pitch_Quality_16), bins = 100) +
  ggtitle("Breaking Ball Pitch Quality Trend from 2017 - 2016")

# OFF SPEED: Use the 2016 model to get pitch quality for the 2017 data 
OS_plot_data_17$Pitch_Quality_16 <-
  predict(Pitch_Quality_Model_16, newdata = OS_plot_data_17, type = 'response')

# Plot both years to check for coorelation, moderately similar distribution 
with(OS_plot_data_17, plot(Pitch_Quality_16, Pitch_Quality_17))

# Plot difference between pitch qualities, very small difference in pitches with low
# speed and higher downward vert. movement, where the home run probability is now only 0.2% more than in 2016
ggplot(OS_plot_data_17, aes(release_speed, pfx_z)) +
  stat_summary_hex(aes(z = Pitch_Quality_17 - Pitch_Quality_16), bins = 100) +
  ggtitle("Off Speed Pitch Quality Trend from 2017 - 2016")

# Subset Y16_Fastballs to equal same rows as Y17_Fastballs
Y16_Fastballs1 <- Y16_Fastballs[c(1:26, 36:38)]
Y17_Fastballs1 <- Y17_Fastballs[1:29]

# Rbind both 2016 and 2017 Pitch Quality Data for Comparison
Combine_16_17 <- rbind(Y17_Fastballs1, Y16_Fastballs1)

# Predict 16/17 data using 16 Model
Combine_16_17$Pitch_Quality_16_Model <- predict(Pitch_Quality_Model_16,
                                                newdata = Combine_16_17)
# Predict 16/17 data using 17 Model
Combine_16_17$Pitch_Quality_17_Model <- predict(Pitch_Quality_Model_17,
                                                newdata = Combine_16_17)
Combine_16_17 <- Combine_16_17[complete.cases(Combine_16_17),]

mean(Combine_16_17$Pitch_Quality_16_Model)
mean(Combine_16_17$Pitch_Quality_17_Model)

