################ FINAL MODELS ##################

model1 <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4, data=df)
summary(model1)               


model2 <-lm(no2 ~ ws + air_temp + bg.no2 + season, data=df)
summary(model2)

# Unfortunately model2 has insignificant air_temp variable (p value 0.289)
# it is because of corelation with season as seen below
# For the exercise sake, lets use model1 and then see

model2_inter <- lm(no2 ~ ws + bg.no2 + season * air_temp, data = df)
summary(model2_inter)
anova(model2, model2_inter, test = "Chisq")


########### COMPLETING TASKS FROM THE EXERCISE :  ##################
### 1.) a.) is done in Descriptive_and_Visualization.r

### 1.) b.)
# here is only code for this exercise, suggested metrics and graphs
#A well founded discussion has to be made later in the report.

summary(model1)

# Diagnostic graphs
par(mfrow = c(2, 2))
plot(model1)  ## seems OK 


# extracting residuals and fitted values 
residuals <- residuals(model1)
fitted_values <- fitted(model1)

# Histogram of residuals
hist(residuals, breaks = 30, main = "Histogram of residuals", 
     xlab = "Residuals", col = "skyblue")


# MSE, RMSE a R-squared AIC a BIC
MSE <- mean(residuals^2)  # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
R_squared <- summary(model1)$r.squared  # R squareed
Adjusted_R_squared <- summary(model1)$adj.r.squared  #Adjusted R squared
AIC_value <- AIC(model1)
BIC_value <- BIC(model1)

cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared:", R_squared, "\n")
cat("Adjusted R-squared:", Adjusted_R_squared, "\n")
cat("AIC:", AIC_value, "\n")
cat("BIC:", BIC_value, "\n")


 
# # 5. Multicolinearity of predictors
# # Computing of  VIF (Variance Inflation Factor)
# library(car)
# vif(model1)  # Values of VIF > 5 can indicate colinearity.


### 1.) c.)
### i. Interpret the raw and adjusted effects of X1 (ws) and X2 (wd_category4)

# Extract the summary of the model
summary(model1)

# For ws (X1):
# The coefficient of `ws` represents the raw effect, i.e., how much the dependent variable (no2) 
# is expected to change for a one-unit increase in ws, holding all other variables constant.

# For wd_category4 (X2):
# wd_category4 is a categorical variable with more than two levels. Its effect is represented in the 
# model using dummy variables. The coefficients for the categories (excluding the reference category) 
# represent the expected difference in no2 compared to the reference category, holding other variables constant.

# To interpret:
# - Look at the coefficients and p-values for `ws` and each level of `wd_category4`.
# - Significant p-values indicate meaningful relationships.
# - Coefficients are interpreted as the average difference in no2 (or its change) per unit increase in ws 
#   or as the difference between a category of wd_category4 and the reference level.


#-------------------------------------------------

# ii. Interpret the effect of changing wd_category4 from the third to the second category

# Extract the coefficients and covariance matrix from the model
coef_model <- coef(model1)          # Coefficients of the model
vcov_model <- vcov(model1)          # Variance-covariance matrix of the coefficients
coef_model
vcov_model

# The effect of going from SOUTH to EAST is:
effect <- coef_model["wd_category4East"] - coef_model["wd_category4South"]
effect

# Compute the standard error of this difference as sd = sqrt(Var)
sd_effect <- sqrt(vcov_model["wd_category4East", "wd_category4East"] + 
                    vcov_model["wd_category4South", "wd_category4South"] - 
                    2 * vcov_model["wd_category4East", "wd_category4South"])

# Compute the 95% confidence interval
# Assuming normal distribution of residuals
k_95 = qnorm(0.975)
k_90 = qnorm(0.95)

ci_95 <- c(effect - k_95 * sd_effect, effect + k_95 * sd_effect)
ci_90 <- c(effect - k_90 * sd_effect, effect + k_90 * sd_effect)

# Print the results
cat("Effect of changing from the third to the second category of wd_category4:", effect, "\n")
cat("Standard Error:", sd_effect, "\n")
cat("95% Confidence Interval:", ci_95, "\n")
cat("90% Confidence Interval:", ci_90, "\n")

# Interpretation:
# The effect of changing from the third category (SOUTH)
# to the second category (EAST) of wd_category4 on
# the response variable (no2) represents the difference
# in predicted NO2 concentrations between these two wind directions
# ,keeping all other predictors (ws, air_temp, bg.no2) constant. 
# And with other predictors kept in constant, the effect is simply the difference
# between raw effect of SOUTH and EAST
# The calculated effect is 3.290624, which can be interpreted as :
# Changing the wind direction from SOUTH to EAST leads to increase of NO2 values
# by 3.290624 (on average!), when all other predictors are constant.

# - Confidence intervals provide a range of plausible values for this effect.
# - If the confidence interval does not include 0, the effect is statistically significant.
# - A 95% confidence interval indicates that there is a 95% probability
# - that the true difference in NO2 lies within this interval, assuming the model is correct.
# A 90% confidence interval provides a slightly narrower range but with a lower level of certainty.


#-------------------------------------------------

# iii. Investigate the existence of a significant interaction between X1 (ws) and X2 (wd_category4)

# Add an interaction term to the model
model_interaction <- lm(no2 ~ ws * wd_category4 + air_temp + bg.no2, data = df)
summary(model_interaction)

# Check the significance of the interaction term
anova(model1, model_interaction)

# Interpretation:
# Anova is showing that including a interaction term significantly improves our model
# so we can say that there is colinearity between wd_category and wind speed,
# which seems expected

# Lets add some graphs to show this interaction

# Visualization of the interaction effect
library(interactions)
interact_plot(model_interaction, pred = ws, modx = wd_category4)

library(ggplot2)
ggplot(df, aes(x = ws, y = no2, color = wd_category4)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction between Wind Speed and Direction",
       x = "Wind Speed",
       y = "NO2 Levels")

# From these graphs we can definitely say that with with higher wind speed there 
# is lower NO2 values, which seems correct. But the main colinearity is at this:
# with different wind directions there is different NO2 values !!!!!!!!!



#### 2.) 

# Transform the response variable
median_no2 <- median(df$no2, na.rm = TRUE)
df$no2_binary <- ifelse(df$no2 >= median_no2, 1, 0)

# Create the logisic model
logistic_model <- glm(no2_binary ~ ws + wd_category4 + air_temp + bg.no2, 
                      data = df, 
                      family = binomial)

summary(logistic_model)

### i.)

# In logistic regression, the coefficients (Estimate)
# describe the log-odds change in the response variable (no2_binary)
# for a one-unit change in the predictor, holding other variables constant.

### For given X1 = windspeed:
ws_estimate = -0.47457648
# For a one-unit increase in wind speed (e.g., 1 m/s),
# the log-odds of high NO2 levels (no2_binary = 1)
# decrease by 0.4746, holding all other variables constant.
ws_odds_ratio = exp(ws_estimate)
ws_odds_ratio         # 0.6221485
#The odds of high NO2 levels decrease by ~37.8%  (1-0.6221485)
# for each 1 m/s increase in wind speed.


### For given X2 = wd_category4
wd_east_estimate = 1.67291584
# Compared to the baseline category (North), the log-odds of high NO2 levels
# are 1.6730 higher when the wind direction is East.
wd_odds_ratio_east = exp(1.67291584)
wd_odds_ratio_east      #5.32768
# his means the odds of high NO2 levels are ~5.33 times greater
# when the wind direction is East compared to North.

wd_south_estimate = 1.29607534
# Compared to North, the log-odds increase by 1.2961 
# when the wind direction is South
wd_odds_ratio_south = exp(1.29607534)
wd_odds_ratio_south
# The odds of high NO2 levels are ~3.65 times greater
# for South compared to North.

wd_west_estimate = 1.11120341
# Compared to North, the log-odds increase by 1.1112
# when the wind direction is
wd_odds_ratio_west = exp(1.11120341)
wd_odds_ratio_west
# The odds of high NO2 levels are ~3.04 times greater
# for West compared to North.



### ii.)

# Calculate the log-odds change from South to East
diff_coef <- wd_east_estimate - wd_south_estimate
diff_coef

# Standard error of the difference
vcov_logit <- vcov(logistic_model)
sd_effect <- sqrt(
  vcov_logit["wd_category4East", "wd_category4East"] +
    vcov_logit["wd_category4South", "wd_category4South"] -
    2 * vcov_logit["wd_category4East", "wd_category4South"]
)

vcov_logit["wd_category4East", "wd_category4South"]
# Confidence intervals
z_95 <- qnorm(0.975)
z_90 <- qnorm(0.95)
ci_95 <- c(diff_coef - z_95 * sd_effect, diff_coef + z_95 * sd_effect)
ci_90 <- c(diff_coef - z_90 * sd_effect, diff_coef + z_90 * sd_effect)

# Print results
list(
  log_odds_difference = diff_coef,
  confidence_interval_95 = ci_95,
  confidence_interval_90 = ci_90,
  odds_ratio_difference = exp(diff_coef)  #1.457672
)
### This means that switching from South to East increases the odds of high NO2 levels by ~45.7% compared to SOUTH

### iii.)
# Fit logistic regression with interaction
logistic_model_interaction <- glm(no2_binary ~ ws * wd_category4 + air_temp + bg.no2, 
                                  data = df, 
                                  family = binomial)

# Likelihood ratio test
anova(logistic_model, logistic_model_interaction, test = "Chisq")

## Chisq test has p-value 2.2e-16, which inidicates significant improvement of the model
## which means there is colinearity between wind speed and wd_category