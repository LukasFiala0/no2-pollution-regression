######## NOW WE EXPLORE COLINEARITY ######## 
## A.) CONTINUOUS EXPLANATORY VARIABLES

explanatory_vars <- df[, c("bg.no2", "ws", "wd", "air_temp")]  # explanatory variables
cor_matrix <- cor(explanatory_vars, use = "complete.obs", method = "pearson")
cor_matrix

# USING PEARSON TEST, WITH SPEARMAN TEST THE RESULTS WERE THE SAME

cor.test(df$bg.no2,df$ws)
cor.test(df$bg.no2,df$wd)
cor.test(df$bg.no2,df$air_temp)
cor.test(df$ws,df$wd)
cor.test(df$ws,df$air_temp)
cor.test(df$wd,df$air_temp)



# COLINEARITY SEEN BETWEEN:   (significant)
# bg.no2 and ws  cor = -0.279  -- EXPECTED   --> will create interaction
# bg.no2 and wd  cor = -0.393  -- RANDOM     --> will NOT create interaction, wd is not used
# bg.no2 and air_temp  cor = -0.263  -- EXPECTED   --> will create interaction
# ws and wd cor = -0.017 (p value = 0.006) --> will NOT create interaction, wd is not used
# ws and air_temp cor = 0.153  --> will create interaction
# wd and air_temp cor = 0.071 --> will NOT create interaction, wd is not used

# 1.) bg.no2 and ws interaction
df$bg.no2_ws <- (scale(df$bg.no2) * scale(df$ws))

# 2.) bg.no2 and ait_temp interaction
df$bg.no2_air_temp <- (scale(df$bg.no2) * scale(df$air_temp))

# 3.) air_temp and ws interaction
df$air_temp_ws <- (scale(df$air_temp) * scale(df$ws))


### B.) CATEGORICAL EXPLANATORZ VARIABLES INTERACTIONS

# Contingency tables for testing colinearity of categorical variables
tab_wd4_wd8 <- table(df$wd_category4, df$wd_category8)
tab_wd4_month <- table(df$wd_category4, df$month)
tab_wd4_season <- table(df$wd_category4, df$season)
tab_wd8_month <- table(df$wd_category8, df$month)
tab_wd8_season <- table(df$wd_category8, df$season)
tab_month_season <- table(df$month, df$season)

# Applying Chi-Square test
chisq.test(tab_wd4_wd8)    # Test wd_category4 vs wd_category8
chisq.test(tab_wd4_month)  # Test wd_category4 vs month
chisq.test(tab_wd4_season) # Test wd_category4 vs season
chisq.test(tab_wd8_month)  # Test wd_category8 vs month
chisq.test(tab_wd8_season) # Test wd_category8 vs season
chisq.test(tab_month_season) # Test month vs season

## All of them are significantly NOT INDEPENDENT!! --> COLINEARITY:
## We need to combine them (reasonably) -- CREATE INTERACTIONS

### 1.) 4 wind directions and month ineraction
df$wd4_month <-interaction(df$wd_category4, df$month)

### 2.) 4 wind directions and season interaction
df$wd4_season <-interaction(df$wd_category4, df$season)


## this is enough of categorical interactions, it is already too complicated
## THESE CATEGORICAL INTERACTIONS ARE TOO COMPLICATED, NONE OF THEM WILL BE USED...
# also as you can from ModelsR.R file, there is no significant improvement

