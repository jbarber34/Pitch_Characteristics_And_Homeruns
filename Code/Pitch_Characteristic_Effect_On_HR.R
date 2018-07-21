library(ggplot2)
library(RColorBrewer)
library(gtable)

# Read in all fastballs from 2017
Y17_All_Pitches <- read.csv('/Users/James/OneDrive - oaklandathletics/R Studio/Pitcher Type Project/All Pitches Clean/2017 All Pitches Thrown Clean.csv',na.strings = 'null')

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
                        | pitch_type == "SI" | pitch_type ==  "FC")

#Logistic Regression Model of Fastball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_17FB <- 
  lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter, 
     data = Y17_Fastballs)

# Get Pitch Quality for all fastballs thrown
Y17_Fastballs$Pitch_Quality_17FB <- predict(Pitch_Quality_Model_17FB,
                                          newdata = Y17_Fastballs)

# Put All Fastballs into a new data frame
F_plot_data_17 <- Y17_Fastballs

# Control the vertical movement variable and spin rate for Fastball by finding median
F_plot_data_17$pfx_z <- median(F_plot_data_17$pfx_z, na.rm = TRUE)
F_plot_data_17$release_spin_rate <- median(F_plot_data_17$release_spin_rate, na.rm = TRUE)

# Find new pitch quaility with median of pfx_z
F_plot_data_17$Pitch_Quality_17FB <- predict(Pitch_Quality_Model_17FB, newdata = F_plot_data_17)

# Control Fastball most frequent zone which isolates release_speed and pfx_x_batter
F_plot_data_17 <- F_plot_data_17[F_plot_data_17$zone == 14, ]

# Plot Fastball results, faster pitches in on the hands are toughest to hit for home runs
# Slower pitches moving away are easier to hit.
ggplot(F_plot_data_17, aes(pfx_x_batter, release_speed, col = Pitch_Quality_17FB)) +
  stat_summary_hex(aes(z = Pitch_Quality_17FB), bins = 100)+
  labs(title="2017 Fastball Pitch Quality Model",
       subtitle = "As pitches increase in velocity with inward movement, they become more difficult to hit",
       x = "Horizontal Movement",
       y = "Release Speed")+
  theme(text = element_text(size=15))

# Plot Fastball hit quality
ggplot(Y17_Fastballs, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
labs(title="2017 Hit Quality Model",
     x = "Launch Speed",
     y = "Launch Angle")+
  theme(text = element_text(size=15))

#Fastball Pitch Quality by zone
#Both <85 and >100 had high variability in quality, 85-100mph most consistent pitch quality
ggplot(Y17_Fastballs, aes(release_speed, Pitch_Quality_17FB, col = as.factor(zone))) +
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
Pitch_Quality_Model_17BB <- 
  lm(Hit_Quality_17 ~ release_spin_rate + release_speed + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:release_spin_rate, 
     data = Y17_BreakingBall)

# Get Pitch Quality for all breaking ball thrown
Y17_BreakingBall$Pitch_Quality_17BB <- predict(Pitch_Quality_Model_17BB,
                                             newdata = Y17_BreakingBall)

# Put All Breaking Balls into a new data frame
BB_plot_data_17 <- Y17_BreakingBall

# Control release speed variable
BB_plot_data_17$release_speed <- median(BB_plot_data_17$release_speed, na.rm = TRUE)

# Find new pitch quaility with median of variable
BB_plot_data_17$Pitch_Quality_17BB <- predict(Pitch_Quality_Model_17BB, newdata = BB_plot_data_17)

# Control Breaking Ball most frequent zone which isolates variables
BB_plot_data_17 <- BB_plot_data_17[BB_plot_data_17$zone == 14, ]

# Plot Pitch Quality results with isolated spin rate, vertical and horizontal movement variables
#Pitches with high spin rate, lower in the zone are not hit as well as 
# pitches with lower spin rate w/less downward vert. movement.
ggplot(BB_plot_data_17, aes(release_spin_rate, pfx_z, col = Pitch_Quality_17BB)) +
  stat_summary_hex(aes(z = Pitch_Quality_17BB), bins = 100)+
  ggtitle("2017 Breaking Ball Pitch Quality Model")

# Plot Breaking Ball hit quality
ggplot(Y17_BreakingBall, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
  ggtitle("2017 Breaking Ball Hit Quality Model")

# Breaking Ball pitch quality by zone
# Lower variabilty 2050-2650 rotations per minute. Highest quality in zone 2, 5
ggplot(Y17_BreakingBall, aes(release_spin_rate, Pitch_Quality_17BB, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_spin_rate", limits = c(700, 3500),
                     breaks = seq(650, 3500, 100)) +
  labs(title="2017 Breaking Ball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between spin rate and pitch quality in each zone",
       x = "Spin Rate",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Off Speed
Y17_OffSpeed <- subset(Y17_All_Pitches, pitch_type == "CH" | pitch_type ==  "FS" )

#Logistic Regression Model of breaking ball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_17OS <- 
  lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter, 
     data = Y17_OffSpeed)

# Get Pitch Quality for all breaking ball thrown
Y17_OffSpeed$Pitch_Quality_17OS <- predict(Pitch_Quality_Model_17OS,
                                         newdata = Y17_OffSpeed)

# Put All Off Speed into a new data frame
OS_plot_data_17 <- Y17_OffSpeed

# Control hor. movement variable
OS_plot_data_17$pfx_x_batter <- median(OS_plot_data_17$pfx_x_batter, na.rm = TRUE)

# Find new pitch quaility with median of variable
OS_plot_data_17$Pitch_Quality_17OS <- predict(Pitch_Quality_Model_17OS, newdata = OS_plot_data_17)

# Control Breaking Ball most frequent zone which isolates variables
OS_plot_data_17 <- OS_plot_data_17[OS_plot_data_17$zone == 14, ]

# Plot Pitch Quality results with isolated release speed, spin rate, and hor. movement variables
# Pitches higher w/ less vert. movement and slower are more likely to be high for 
#power than more downward vert. movement and closer to 90mph
ggplot(OS_plot_data_17, aes(release_speed, pfx_z, col = Pitch_Quality_17OS)) +
  stat_summary_hex(aes(z = Pitch_Quality_17OS), bins = 100)+
  ggtitle("2017 Breaking Ball Pitch Quality Model")

# Plot Off Speed hit quality
ggplot(Y17_OffSpeed, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_17), bins = 100) +
  ggtitle("2017 Off Speed Hit Quality Model")

# Off Speed pitch quality by zone
# Low variability between 80-90mph, highest quality in zones 2,5
ggplot(Y17_OffSpeed, aes(release_speed, Pitch_Quality_17OS, col = as.factor(zone))) +
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
Y16_All_Pitches <- read.csv('/Users/James/OneDrive - oaklandathletics/R Studio/Pitcher Type Project/All Pitches Clean/2016 All Pitches Thrown Clean.csv',na.strings = 'null')

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
                        | pitch_type == "SI" | pitch_type ==  "FC")

#Logistic Regression Model of Fastball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_16FB <- 
  lm(Hit_Quality_16 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter, 
     data = Y16_Fastballs)

# Get Pitch Quality for all pitches thrown
Y16_Fastballs$Pitch_Quality_16FB <- predict(Pitch_Quality_Model_16FB,
                                          newdata = Y16_Fastballs)
# Put All Pitches into a new data frame
F_plot_data_16 <- Y16_Fastballs

# Control the vertical movement variable by finding mean
F_plot_data_16$pfx_z <- median(F_plot_data_16$pfx_z, na.rm = TRUE)

# Find new pitch quaility with mean of pfx_z
F_plot_data_16$Pitch_Quality_16FB <- predict(Pitch_Quality_Model_16FB, newdata = F_plot_data_16)

#Control most frequent zone which isolates release_speed and pfx_x_batter
F_plot_data_16 <- F_plot_data_16[F_plot_data_16$zone == 14, ]

# Plot Fastball results, faster pitches in on the hands are toughest to hit for home runs
# Slower pitches moving away are easier to hit.
ggplot(F_plot_data_16, aes(pfx_x_batter, release_speed, col = Pitch_Quality_16FB)) +
  stat_summary_hex(aes(z = Pitch_Quality_16FB), bins = 100)+
  ggtitle("2016 Fastball Pitch Quality Model")

# Plot Fastball hit quality
ggplot(Y16_Fastballs, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Fastball Hit Quality Model")

#Fastball Pitch Quality by zone
#Both <85 and >100 had high variability in quality, 85-100mph most consistent pitch quality
ggplot(Y16_Fastballs, aes(release_speed, Pitch_Quality_16FB, col = as.factor(zone))) +
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
Pitch_Quality_Model_16BB <- 
  lm(Hit_Quality_16 ~ release_spin_rate + release_speed + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:release_spin_rate, 
     data = Y16_BreakingBall)

# Get Pitch Quality for all breaking ball thrown
Y16_BreakingBall$Pitch_Quality_16BB <- predict(Pitch_Quality_Model_16BB,
                                             newdata = Y16_BreakingBall)

# Put All Breaking Balls into a new data frame
BB_plot_data_16 <- Y16_BreakingBall

# Control release speed variable
BB_plot_data_16$release_speed <- median(BB_plot_data_16$release_speed, na.rm = TRUE)

# Find new pitch quaility with median of variable
BB_plot_data_16$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16BB, newdata = BB_plot_data_16)

# Control Breaking Ball most frequent zone which isolates variables
BB_plot_data_16 <- BB_plot_data_16[BB_plot_data_16$zone == 14, ]

# Plot Pitch Quality results with isolated spin rate, vertical and horizontal movement variables
#Pitches higher downward vert. movement are not hit as well as pitches w/less 
#downward vert. movement. Regardless of spin rate
ggplot(BB_plot_data_16, aes(release_spin_rate, pfx_z, col = Pitch_Quality_16BB)) +
  stat_summary_hex(aes(z = Pitch_Quality_16BB), bins = 100)+
  ggtitle("2016 Breaking Ball Pitch Quality Model")

# Plot Breaking Ball hit quality
ggplot(Y16_BreakingBall, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Breaking Ball Hit Quality Model")

# Breaking Ball pitch quality by zone
# Lower variabilty 2050-2650 rotations per minute. Highest quality in zone 2, 5
ggplot(Y16_BreakingBall, aes(release_spin_rate, Pitch_Quality_16BB, col = as.factor(zone))) +
  stat_summary(fun.y="mean", geom="line")+
  scale_x_continuous("release_spin_rate", limits = c(700, 3500),
                     breaks = seq(650, 3500, 100)) +
  labs(title="2016 Breaking Ball Pitch Quality Per Pitching Zone",
       subtitle = "Relationship between spin rate and pitch quality in each zone",
       x = "Spin Rate",
       y = "Pitch Quality",
       fill = "Pitch Zone")

# Subset into Off Speed
Y16_OffSpeed <- subset(Y16_All_Pitches, pitch_type == "CH"| pitch_type ==  "FS")

#Logistic Regression Model of breaking ball home run probability from pitch speed, location, and movement
Pitch_Quality_Model_16OS <- 
  lm(Hit_Quality_16 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter, 
     data = Y16_OffSpeed)

# Get Pitch Quality for all breaking ball thrown
Y16_OffSpeed$Pitch_Quality_16OS <- predict(Pitch_Quality_Model_16OS,
                                         newdata = Y16_OffSpeed)

# Put All Off Speed into a new data frame
OS_plot_data_16 <- Y16_OffSpeed

# Control hor. movement variable
OS_plot_data_16$pfx_x_batter <- median(OS_plot_data_16$pfx_x_batter, na.rm = TRUE)

# Find new pitch quaility with median of variable
OS_plot_data_16$Pitch_Quality_16 <- predict(Pitch_Quality_Model_16OS, newdata = OS_plot_data_16)

# Control Breaking Ball most frequent zone which isolates variables
OS_plot_data_16 <- OS_plot_data_16[OS_plot_data_16$zone == 14, ]

# Plot Pitch Quality results with isolated release speed, spin rate, and vert. movement variables
# Pitches higher w/ less vert. movement and slower are more likely to be hit for 
# power than more downward vert. movement and closer to 80 - 90mph
ggplot(OS_plot_data_16, aes(release_speed, pfx_z, col = Pitch_Quality_16OS)) +
  stat_summary_hex(aes(z = Pitch_Quality_16OS), bins = 100)+
  ggtitle("2016 Off Speed Pitch Quality Model")

# Plot Off Speed hit quality
ggplot(Y16_OffSpeed, aes(launch_speed, launch_angle)) +
  stat_summary_hex(aes(z = Hit_Quality_16), bins = 100) +
  ggtitle("2016 Off Speed Hit Quality Model")

# Off Speed pitch quality by zone
# Low variability between 80-90mph, highest quality in zones 2,5
ggplot(Y16_OffSpeed, aes(release_speed, Pitch_Quality_16OS, col = as.factor(zone))) +
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
labs(title="Fastball Quality of Contact Trend from 2017 - 2016",
     subtitle = "Borderline batted balls are more likely to be hit for a home run in 2017",
     x = "Launch Speed",
     y = "Launch Angle")+
  theme(text = element_text(size=15))

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
F_plot_data_17$Pitch_Quality_16FB <-
  predict(Pitch_Quality_Model_16FB, newdata = F_plot_data_17, type = 'response')

# Plot both years to check for coorelation, similar but not identical for each ind.
#pitch type
with(F_plot_data_17, plot(Pitch_Quality_16FB, Pitch_Quality_17FB))

# Plot difference between pitch qualities, slight difference on slower pitches
# moving away, where the home run probability is now as much as 0.5% more than in 2016
ggplot(F_plot_data_17, aes(pfx_x_batter, release_speed)) +
  stat_summary_hex(aes(z = Pitch_Quality_17FB - Pitch_Quality_16FB), bins = 100) +
  labs(title="Fastball Pitch Quality Trend from 2017 - 2016",
       subtitle = "Relationship Between Pitch Speed and Horizontal Movement",
       x = "Horizontal Movement",
       y = "Release Speed")+
  theme(text = element_text(size=20))

# BREAKING BALL: Use the 2016 model to get pitch quality for the 2017 data 
BB_plot_data_17$Pitch_Quality_16BB <-
  predict(Pitch_Quality_Model_16BB, newdata = BB_plot_data_17, type = 'response')

# Plot both years to check for coorelation, similar but not identical
with(BB_plot_data_17, plot(Pitch_Quality_16BB, Pitch_Quality_17BB))

# Plot difference between pitch qualities, very small difference in pitches with downward
# vert. movement and high spin rate, where the home run probability is now 0.1% higher than in 2016
ggplot(BB_plot_data_17, aes(release_spin_rate, pfx_z)) +
  stat_summary_hex(aes(z = Pitch_Quality_17BB - Pitch_Quality_16BB), bins = 100) +
  labs(title="Breaking Ball Pitch Quality Trend from 2017 - 2016",
       subtitle = "Relationship Between Vertical Movement and Spin Rate",
       x = "Spin Rate",
       y = "Vertical Movement")+
  theme(text = element_text(size=20))

# OFF SPEED: Use the 2016 model to get pitch quality for the 2017 data 
OS_plot_data_17$Pitch_Quality_16OS <-
  predict(Pitch_Quality_Model_16OS, newdata = OS_plot_data_17, type = 'response')

# Plot both years to check for coorelation, moderately similar distribution 
with(OS_plot_data_17, plot(Pitch_Quality_16OS, Pitch_Quality_17OS))

# Plot difference between pitch qualities, very small difference in pitches with low
# speed and higher downward vert. movement, where the home run probability is now only 0.2% more than in 2016
ggplot(OS_plot_data_17, aes(release_speed, pfx_z)) +
  stat_summary_hex(aes(z = Pitch_Quality_17OS - Pitch_Quality_16OS), bins = 100) +
  labs(title="Off-Speed Pitch Quality Trend from 2017 - 2016",
       subtitle = "Relationship Between Vertical Movement and Release Speed",
       x = "Release Speed",
       y = "Vertical Movement")+
  theme(text = element_text(size=15))


# Subset Y16_Fastballs to equal same rows as Y17_Fastballs
Y16_Fastballs1 <- Y16_Fastballs[c(1:26, 36:38)]
Y17_Fastballs1 <- Y17_Fastballs[1:29]

# Predict 16 data using 16 Model
Y16_Fastballs1$Pitch_Quality_16_ModelFB <- predict(Pitch_Quality_Model_16FB,
                                                newdata = Y16_Fastballs1)

# Predict 17 data using 16 Model
Y17_Fastballs1$Pitch_Quality_16_ModelFB <- predict(Pitch_Quality_Model_16FB,
                                                newdata = Y17_Fastballs1)

# Only use complete cases
Y16_Fastballs1 <- Y16_Fastballs1[complete.cases(Y16_Fastballs1),]
Y17_Fastballs1 <- Y17_Fastballs1[complete.cases(Y17_Fastballs1),]

# Subset Y16_BreakingBall to equal same rows as Y17_BreakingBall
Y16_BreakingBall1 <- Y16_BreakingBall[c(1:26, 36:38)]
Y17_BreakingBall1 <- Y17_BreakingBall[1:29]

# Predict 16 data using 16 Model
Y16_BreakingBall1$Pitch_Quality_16_ModelBB <- predict(Pitch_Quality_Model_16BB,
                                                 newdata = Y16_BreakingBall1)
# Predict 17 data using 16 Model
Y17_BreakingBall1$Pitch_Quality_16_ModelBB <- predict(Pitch_Quality_Model_16BB,
                                                 newdata = Y17_BreakingBall1)
# Only use complete cases
Y16_BreakingBall1 <- Y16_BreakingBall1[complete.cases(Y16_BreakingBall1),]
Y17_BreakingBall1 <- Y17_BreakingBall1[complete.cases(Y17_BreakingBall1),]

# Subset Y16_OffSpeed to equal same rows as Y17_OffSpeed
Y16_OffSpeed1 <- Y16_OffSpeed[c(1:26, 36:38)]
Y17_OffSpeed1 <- Y17_OffSpeed[1:29]

# Predict 16/17 data using 16 Model
Y16_OffSpeed1$Pitch_Quality_16_ModelOS <- predict(Pitch_Quality_Model_16OS,
                                                 newdata = Y16_OffSpeed1)
# Predict 16/17 data using 17 Model
Y17_OffSpeed1$Pitch_Quality_16_ModelOS <- predict(Pitch_Quality_Model_16OS,
                                                 newdata = Y17_OffSpeed1)
# Only use complete cases
Y16_OffSpeed1 <- Y16_OffSpeed1[complete.cases(Y16_OffSpeed1),]
Y17_OffSpeed1 <- Y17_OffSpeed1[complete.cases(Y17_OffSpeed1),]

Mean_16 <- c(mean(Y16_Fastballs1$Pitch_Quality_16_ModelFB)) 
Mean_17 <- c(mean(Y17_Fastballs1$Pitch_Quality_16_ModelFB)) 
BMean_16 <- c(mean(Y16_BreakingBall1$Pitch_Quality_16_ModelBB)) 
BMean_17 <- c(mean(Y17_BreakingBall1$Pitch_Quality_16_ModelBB)) 
OMean_16 <- c(mean(Y16_OffSpeed1$Pitch_Quality_16_ModelOS)) 
OMean_17 <- c(mean(Y17_OffSpeed1$Pitch_Quality_16_ModelOS)) 
Quality <- c(Mean_16, Mean_17, BMean_16, BMean_17, OMean_16, OMean_17)
Pitch_Type <- c("Fastballs 2016", "Fastballs 2017", "Breakingballs 2016", "Breakingballs 2017", "Off-Speed 2016", "Off-Speed 2017")
Year <- c("2016 Pitch Data", "2017 Pitch Data") 
qdata <- data.frame(Pitch_Type, Quality, Year)
Q <- ggplot(qdata, aes(Pitch_Type, Quality))
Q <- Q + labs(x = "Pitch Type")
Q + geom_bar(stat = "identity", aes(fill = Year))+
  labs(title = "2016 - 2017 Pitch Quality Comparison",
       subtitle = "Home Run Probability from 2016 Pitch Quality Model",
       xlab = "Pitch Quality Model")+
  scale_y_continuous("Home Run Probability", limits = c(0, .06),
                     breaks = seq(0, .06, .02))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(text = element_text(size=15))

Combine_Mean <- c(mean(Y16_Fastballs1$Pitch_Quality_16_ModelFB), mean(Y17_Fastballs1$Pitch_Quality_16_ModelFB))
names(Combine_Mean) <- c("2016 Data Fastball Pitch Quality", "2017 Data Fastball Pitch Quality")

BCombine_Mean <- c(mean(Y16_BreakingBall1$Pitch_Quality_16_ModelBB), mean(Y17_BreakingBall1$Pitch_Quality_16_ModelBB))
names(BCombine_Mean) <- c("2016 Data Breakingball Pitch Quality", "2017 Data Breakingball Pitch Quality")

OCombine_Mean <- c(mean(Y16_OffSpeed1$Pitch_Quality_16_ModelOS), mean(Y17_OffSpeed1$Pitch_Quality_16_ModelOS))
names(OCombine_Mean) <- c("2016 Data Off-Speed Pitch Quality", "2017 Data Off-Speed Pitch Quality")

Combine_Mean
BCombine_Mean
OCombine_Mean

## PLAY WITH THIS NEED BETTER NAMES TO EXPLAIN WHAT QUALITY or PROBABILITY MEANS AS WELL AS TURN TO % 

Model <- c("Fastball Model", "Breakingball Model", "Off-Speed Model")
Year1 <- c(Mean_16, BMean_16, OMean_16)
Year2 <- c(Mean_17, BMean_17, OMean_17)
Difference <- c((Mean_16 - Mean_17), (BMean_16 - BMean_17), (OMean_16 - OMean_17))
Quality <- data.frame(Model, Year1, Year2, Difference)
names(Quality) <- c("Model", "2016 HR Probability", "2017 HR Probability", "Difference")
Quality$`2016 HR Probability` <- as.numeric(Quality$`2016 HR Probability`)
Quality$`2016 HR Probability` <- round(Quality$`2016 HR Probability`, digits = 4) 
Quality$`2016 HR Probability` <- percent(Quality$`2016 HR Probability`)
Quality$`2017 HR Probability` <- as.numeric(Quality$`2017 HR Probability`)
Quality$`2017 HR Probability` <- round(Quality$`2017 HR Probability`, digits = 4) 
Quality$`2017 HR Probability` <- percent(Quality$`2017 HR Probability`)
Quality$`Difference` <- as.numeric(Quality$`Difference`)
Quality$`Difference` <- round(Quality$`Difference`, digits = 4) 
Quality$`Difference` <- percent(Quality$`Difference`)
Quality <- tableGrob(Quality, rows = NULL)
Quality<- gtable_add_grob(Quality,
                        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                        t = 2, b = nrow(Quality), l = 1, r = ncol(table))
Quality <- gtable_add_grob(Quality,
                         grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                         t = 1, l = 1, r = ncol(Quality))

grid.draw(Quality)







## TESTING MODEL INTERACTIONS ##

# no interaction
set.seed(42)
train <- sample.int(0.75 * nrow(Y17_Fastballs))

fit <- lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE), 
          data = Y17_Fastballs[train, ])
pred <- predict(fit, newdata=Y17_Fastballs[-train, ])

test <- data.frame(actual=Y17_Fastballs$Pitch_Quality_17FB[-train], pred)
test$error <- with(test, pred-actual)
test <- test[complete.cases(test),]
mean(test$error^2)


# w/speed:pfx_x_batter interaction
set.seed(42)
train2 <- sample.int(0.75 * nrow(Y17_Fastballs))

fit2 <- lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter, 
             data = Y17_Fastballs[train2, ])
pred2 <- predict(fit2, newdata=Y17_Fastballs[-train2, ])

test2 <- data.frame(actual=Y17_Fastballs$Pitch_Quality_17FB[-train2], pred2)
test2$error <- with(test2, pred2-actual)
test2 <- test2[complete.cases(test2),]
mean(test2$error^2)

# w/speed:as.factor(zone) interaction
set.seed(42)
train3 <- sample.int(0.75 * nrow(Y17_Fastballs))

fit3 <- lm(Hit_Quality_17 ~ release_speed + release_spin_rate + as.factor(zone) + pfx_z + poly(pfx_x_batter, degree = 2, raw = TRUE) + release_speed:pfx_x_batter + release_speed:as.factor(zone), 
           data = Y17_Fastballs[train3, ])
pred3 <- predict(fit3, newdata=Y17_Fastballs[-train3, ])

test3 <- data.frame(actual=Y17_Fastballs$Pitch_Quality_17FB[-train3], pred3)
test3$error <- with(test3, pred3-actual)
test3 <- test3[complete.cases(test3),]
mean(test3$error^2)

