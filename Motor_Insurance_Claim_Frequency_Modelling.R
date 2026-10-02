library(dplyr)
library(ggplot2)
install.packages(c("zoo", "xts"))

install.packages(
  "CASdatasets",
  repos = "https://dutangc.perso.math.cnrs.fr/RRepository/pub/",
  type = "source"
)

# STEP 1: LOAD AND INSPECT THE DATA

# Load the CASdatasets package
library(CASdatasets)

# Load the French motor third-party liability frequency dataset
data("freMTPL2freq")

# Rename the dataset
motor <- freMTPL2freq

# Check the dimensions: number of rows and columns
dim(motor)

# View the variable names
names(motor)

# View the first six observations
head(motor)

# Check the structure and data types
str(motor)

# View basic summary statistics
summary(motor)

# STEP 2: DATA QUALITY CHECKS

# Check the number of missing values in each variable
colSums(is.na(motor))

# Check whether any policy IDs are duplicated
sum(duplicated(motor$IDpol))

# Look at the distribution of claim counts
table(motor$ClaimNb)
table(motor$ClaimNb[motor$ClaimNb > 4])
motor[motor$ClaimNb > 4, ]
motor$ClaimNb_clean <- pmin(motor$ClaimNb, 4)
summary(motor$ClaimNb_clean)

# Count policies with more than 1 year of exposure
sum(motor$Exposure > 1)
summary(motor$Exposure[motor$Exposure > 1])
sort(motor$Exposure, decreasing = TRUE)[1:20]

# Look at the exposure values above 1
summary(motor$Exposure[motor$Exposure > 1])

# Create a new exposure variable capped at a maximum of 1 year
motor$Exposure_clean <- pmin(motor$Exposure, 1)
head(motor$Exposure_clean)

# Count very old vehicles
sum(motor$VehAge > 50)

# Look at the vehicle ages above 50
table(motor$VehAge[motor$VehAge > 50])

# Look at the largest Bonus-Malus values
sort(unique(motor$BonusMalus), decreasing = TRUE)[1:20]
sum(motor$BonusMalus > 150)

# STEP 3: PORTFOLIO SUMMARY

# Count the number of policy records in the dataset
nrow(motor)

# Calculate the total exposure across the portfolio
total_exposure <- sum(motor$Exposure_clean)
total_exposure

# Calculate the total number of claims
total_claims <- sum(motor$ClaimNb_clean)
total_claims

# Calculate the overall annual claim frequency
portfolio_frequency <- total_claims / total_exposure
portfolio_frequency

# Create a table containing the main portfolio statistics
portfolio_summary <- data.frame(
  Number_of_Policies = nrow(motor),
  Total_Exposure = total_exposure,
  Total_Claims = total_claims,
  Claim_Frequency = portfolio_frequency
)
portfolio_summary

# STEP 4: EXPLORATORY DATA ANALYSIS (EDA)

# STEP 4A: DRIVER AGE

# Group policies by driver age and calculate:
# - number of policies
# - total exposure
# - total claims
# - observed claim frequency
age_summary <- motor %>%
  group_by(DrivAge) %>%
  summarise(
    Policies = n(),
    Exposure = sum(Exposure_clean),
    Claims = sum(ClaimNb_clean),
    Frequency = Claims / Exposure,
    .groups = "drop"
  )
head(age_summary, 20)

# Plot observed claim frequency by driver age
ggplot(age_summary, aes(DrivAge, Frequency)) + geom_point() + geom_line() +
  labs(title = "Observed Claim Frequency by Driver Age", x = "Driver Age", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by driver age
ggplot(age_summary, aes(DrivAge, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Driver Age", x = "Driver Age", y = "Exposure") +
  theme_minimal()

# STEP 4B: VEHICLE AGE

# Calculate policies, exposure, claims and frequency for each vehicle age
vehage_summary <- motor %>% group_by(VehAge) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
head(vehage_summary, 20)

# Plot observed claim frequency by vehicle age
ggplot(vehage_summary, aes(VehAge, Frequency)) + geom_point() + geom_line() +
  labs(title = "Observed Claim Frequency by Vehicle Age", x = "Vehicle Age", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by vehicle age
ggplot(vehage_summary, aes(VehAge, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Vehicle Age", x = "Vehicle Age", y = "Exposure") +
  theme_minimal()

# STEP 4C: BONUS-MALUS

# Calculate policies, exposure, claims and frequency for each BonusMalus level
bonus_summary <- motor %>% group_by(BonusMalus) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
head(bonus_summary, 20)

# Plot observed claim frequency by BonusMalus
ggplot(bonus_summary, aes(BonusMalus, Frequency)) + geom_point() + geom_line() +
  labs(title = "Observed Claim Frequency by Bonus-Malus", x = "Bonus-Malus", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by BonusMalus
ggplot(bonus_summary, aes(BonusMalus, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Bonus-Malus", x = "Bonus-Malus", y = "Exposure") +
  theme_minimal()

# STEP 4D: VEHICLE POWER

# Calculate policies, exposure, claims and frequency for each vehicle power level
power_summary <- motor %>% group_by(VehPower) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
power_summary

# Plot observed claim frequency by vehicle power
ggplot(power_summary, aes(VehPower, Frequency)) + geom_point() + geom_line() +
  labs(title = "Observed Claim Frequency by Vehicle Power", x = "Vehicle Power", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by vehicle power
ggplot(power_summary, aes(VehPower, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Vehicle Power", x = "Vehicle Power", y = "Exposure") +
  theme_minimal()

# STEP 4E: VEHICLE FUEL TYPE

# Calculate policies, exposure, claims and frequency for each fuel type
gas_summary <- motor %>% group_by(VehGas) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
gas_summary

# Plot observed claim frequency by fuel type
ggplot(gas_summary, aes(VehGas, Frequency)) + geom_col() +
  labs(title = "Observed Claim Frequency by Fuel Type", x = "Fuel Type", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by fuel type
ggplot(gas_summary, aes(VehGas, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Fuel Type", x = "Fuel Type", y = "Exposure") +
  theme_minimal()

# STEP 4F: AREA

# Calculate policies, exposure, claims and frequency for each area
area_summary <- motor %>% group_by(Area) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
area_summary

# Plot observed claim frequency by area
ggplot(area_summary, aes(Area, Frequency)) + geom_col() +
  labs(title = "Observed Claim Frequency by Area", x = "Area", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by area
ggplot(area_summary, aes(Area, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Area", x = "Area", y = "Exposure") +
  theme_minimal()

# STEP 4G: POPULATION DENSITY

# Divide policies into 10 density groups, from lowest to highest density
motor <- motor %>% mutate(DensityGroup = ntile(Density, 10))

# Calculate exposure, claims and frequency for each density group
density_summary <- motor %>% group_by(DensityGroup) %>%
  summarise(Policies = n(),
            AvgDensity = mean(Density),
            Exposure = sum(Exposure_clean),
            Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure,
            .groups = "drop")
density_summary

# Plot observed claim frequency by density group
ggplot(density_summary, aes(DensityGroup, Frequency)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Observed Claim Frequency by Population Density", x = "Density Group (1 = Lowest, 10 = Highest)", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by density group
ggplot(density_summary, aes(DensityGroup, Exposure)) + geom_col() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Portfolio Exposure by Population Density", x = "Density Group (1 = Lowest, 10 = Highest)", y = "Exposure") +
  theme_minimal()

# STEP 4H: VEHICLE BRAND

# Calculate policies, exposure, claims and frequency for each vehicle brand
brand_summary <- motor %>% group_by(VehBrand) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
brand_summary

# Plot observed claim frequency by vehicle brand
ggplot(brand_summary, aes(VehBrand, Frequency)) + geom_col() +
  labs(title = "Observed Claim Frequency by Vehicle Brand", x = "Vehicle Brand", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by vehicle brand
ggplot(brand_summary, aes(VehBrand, Exposure)) + geom_col() +
  labs(title = "Portfolio Exposure by Vehicle Brand", x = "Vehicle Brand", y = "Exposure") +
  theme_minimal()

# STEP 4I: REGION

# Calculate policies, exposure, claims and frequency for each region
region_summary <- motor %>% group_by(Region) %>%
  summarise(Policies = n(), Exposure = sum(Exposure_clean), Claims = sum(ClaimNb_clean),
            Frequency = Claims / Exposure, .groups = "drop")
region_summary

# Plot observed claim frequency by region
ggplot(region_summary, aes(reorder(Region, Frequency), Frequency)) + geom_col() +
  coord_flip() +
  labs(title = "Observed Claim Frequency by Region", x = "Region", y = "Claim Frequency") +
  theme_minimal()

# Plot total exposure by region
ggplot(region_summary, aes(reorder(Region, Exposure), Exposure)) + geom_col() +
  coord_flip() +
  labs(title = "Portfolio Exposure by Region", x = "Region", y = "Exposure") +
  theme_minimal()

# STEP 5: TRAIN / TEST SPLIT

# Set seed so the random split can be reproduced
set.seed(123456)

# Randomly select 90% of rows for the training data
train_index <- sample(seq_len(nrow(motor)), size = 0.9 * nrow(motor))

# Create training and test datasets
train <- motor[train_index, ]
test <- motor[-train_index, ]

# Check the number of rows
nrow(train)
nrow(test)

# STEP 6: PREPARE VARIABLES FOR POISSON GLM

# Group vehicle power: values 9 and above are combined
train$VehPower_group <- pmin(train$VehPower, 9)
test$VehPower_group <- pmin(test$VehPower, 9)

# Convert to a categorical variable
train$VehPower_group <- factor(train$VehPower_group)
test$VehPower_group <- factor(test$VehPower_group)

# Check the new groups
table(train$VehPower_group)

# Group vehicle age into 3 categories
train$VehAge_group <- cut(train$VehAge,
                          breaks = c(-Inf, 1, 10, Inf),
                          labels = c("0-1", "2-10", "11+"))

test$VehAge_group <- cut(test$VehAge,
                         breaks = c(-Inf, 1, 10, Inf),
                         labels = c("0-1", "2-10", "11+"))

# Check the new groups
table(train$VehAge_group)

# Group driver age into 7 categories
train$DrivAge_group <- cut(train$DrivAge,
                           breaks = c(-Inf, 20, 25, 30, 40, 50, 70, Inf),
                           labels = c("18-20", "21-25", "26-30", "31-40",
                                      "41-50", "51-70", "71+"))

test$DrivAge_group <- cut(test$DrivAge,
                          breaks = c(-Inf, 20, 25, 30, 40, 50, 70, Inf),
                          labels = c("18-20", "21-25", "26-30", "31-40",
                                     "41-50", "51-70", "71+"))

# Check the groups
table(train$DrivAge_group)

# Cap BonusMalus at 150 for modelling
train$BonusMalus_model <- pmin(train$BonusMalus, 150)
test$BonusMalus_model <- pmin(test$BonusMalus, 150)

# Check the new variable
summary(train$BonusMalus_model)

# Log-transform population density for modelling
train$logDensity <- log(train$Density)
test$logDensity <- log(test$Density)

# Check the transformed variable
summary(train$logDensity)

# Convert Area from A-F to numerical values 1-6
train$Area_model <- as.numeric(train$Area)
test$Area_model <- as.numeric(test$Area)

# Check the conversion
table(train$Area, train$Area_model)

# Convert categorical variables to factors
train$VehBrand <- factor(train$VehBrand)
test$VehBrand <- factor(test$VehBrand, levels = levels(train$VehBrand))

train$VehGas <- factor(train$VehGas)
test$VehGas <- factor(test$VehGas, levels = levels(train$VehGas))

train$Region <- factor(train$Region)
test$Region <- factor(test$Region, levels = levels(train$Region))

# STEP 7: POISSON GLM

# Fit Poisson claim frequency model
glm1 <- glm(ClaimNb_clean ~ Area_model + VehPower_group + VehAge_group +
              DrivAge_group + BonusMalus_model + VehBrand + VehGas +
              logDensity + Region,
            offset = log(Exposure_clean),
            family = poisson(link = "log"),
            data = train)
# View model results
summary(glm1)

# STEP 8: MODEL VALIDATION

# Predict expected claim counts for the test data
test$PredictedClaims <- predict(glm1, newdata = test, type = "response")

# Convert predicted claim counts to annual claim frequency
test$PredictedFrequency <- test$PredictedClaims / test$Exposure_clean

# Check the predictions
summary(test$PredictedClaims)
summary(test$PredictedFrequency)

# Compare actual and predicted claims in the test data
actual_claims <- sum(test$ClaimNb_clean)
predicted_claims <- sum(test$PredictedClaims)

actual_claims
predicted_claims

# Calculate actual-to-expected ratio
AE_ratio <- actual_claims / predicted_claims
AE_ratio

# Compare actual and predicted portfolio frequencies
test_exposure <- sum(test$Exposure_clean)

actual_frequency <- actual_claims / test_exposure
predicted_frequency <- predicted_claims / test_exposure

actual_frequency
predicted_frequency

# Calculate Poisson deviance on the test data
y <- test$ClaimNb_clean
mu <- test$PredictedClaims

poisson_deviance <- 2 * sum(ifelse(y == 0, mu, y * log(y / mu) - (y - mu)))
poisson_deviance

# Average deviance per observation
mean_poisson_deviance <- poisson_deviance / nrow(test)
mean_poisson_deviance

# Calculate overall claim frequency from the training data
baseline_frequency <- sum(train$ClaimNb_clean) / sum(train$Exposure_clean)

# Predict test claims assuming every policy has the same frequency
test$BaselineClaims <- baseline_frequency * test$Exposure_clean

# Calculate baseline Poisson deviance
mu_baseline <- test$BaselineClaims

baseline_deviance <- 2 * sum(ifelse(y == 0, mu_baseline,
                                    y * log(y / mu_baseline) - (y - mu_baseline)))

# Average baseline deviance per observation
mean_baseline_deviance <- baseline_deviance / nrow(test)

baseline_frequency
baseline_deviance
mean_baseline_deviance

# Divide test policies into 10 groups from lowest to highest predicted risk
test <- test %>% mutate(RiskGroup = ntile(PredictedFrequency, 10))

# Calculate actual and predicted frequency within each group
calibration <- test %>% group_by(RiskGroup) %>%
  summarise(Exposure = sum(Exposure_clean),
            ActualClaims = sum(ClaimNb_clean),
            PredictedClaims = sum(PredictedClaims),
            ActualFrequency = ActualClaims / Exposure,
            PredictedFrequency = PredictedClaims / Exposure,
            .groups = "drop")
calibration

# Compare actual and predicted frequency
ggplot(calibration, aes(RiskGroup)) +
  geom_line(aes(y = ActualFrequency, linetype = "Actual")) +
  geom_point(aes(y = ActualFrequency)) +
  geom_line(aes(y = PredictedFrequency, linetype = "Predicted")) +
  geom_point(aes(y = PredictedFrequency)) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Actual vs Predicted Claim Frequency by Risk Group",
       x = "Predicted Risk Group (1 = Lowest, 10 = Highest)",
       y = "Claim Frequency", linetype = "") +
  theme_minimal()
