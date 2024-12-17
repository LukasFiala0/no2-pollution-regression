                             ################# MODELS ##################
                  
##### 1.) MODEL no2 ~ ws + air_temp + wd_category4
model_wd_category4 <- lm(no2 ~ ws + air_temp + wd_category4, data = df)
summary(model_wd_category4)


##### 2.) MODEL no2 ~ ws + air_temp + wd_category8
model_wd_category8 <- lm(no2 ~ ws + air_temp + wd_category8, data = df)
summary(model_wd_category8)


##### 3.) Model no2 ~ ws + air_temp + month
model_month <- lm(no2 ~ ws + air_temp + month, data = df)
summary(model_month)

##### 4.) Model no2 ~ ws + air_temp + season
model_season <- lm(no2 ~ ws + air_temp + season, data = df)
summary(model_season)

#-------------------------------------------------

# NOW WE ADD BG.NO2 TO ALL PREVIOUS ONEs

##### 5.) MODEL no2 ~ ws + air_temp + bg.no2 + wd_category4
model_wd_category4_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category4, data = df)
summary(model_wd_category4_bg.no2)

##### 6.) MODEL no2 ~ ws +air_temp + bg.no2 + wd_category8
model_wd_category8_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category8, data = df)
summary(model_wd_category8_bg.no2)

##### 7.) MODEL no2 ~ ws + air_temp + bg.no2 + month
model_month_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + month, data = df)
summary(model_month_bg.no2)


##### 8.) MODEL no2 ~ ws + air_temp + bg.no2 + season
model_season_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + season, data = df)
summary(model_season_bg.no2)

#-------------------------------------------------

# NOW WE COMBINE 1.) 2.) with month and season

##### 9.) MODEL no2 ~ ws + air_temp + wd_category4 + month
model_wd_category4_month <- lm(no2 ~ ws + air_temp + wd_category4 + month, data = df)
summary(model_wd_category4_month)

##### 10.) MODEL no2 ~ ws + air_temp + wd_category8 + month
model_wd_category8_month <- lm(no2 ~ ws + air_temp + wd_category8 + month, data = df)
summary(model_wd_category8_month)

##### 11.) MODEL no2 ~ ws + air_temp + wd_category4 + season
model_wd_category4_season <- lm(no2 ~ ws + air_temp + wd_category4 + season, data = df)
summary(model_wd_category4_season)

##### 12.) MODEL no2 ~ ws + air_temp + wd_category8 + season
model_wd_category8_season <- lm(no2 ~ ws + air_temp + wd_category8 + season, data = df)
summary(model_wd_category8_season)


#-------------------------------------------------

# NOW WE ADD BG.NO2 TO 9.) 10.) 11.) 12.)



##### 13.) MODEL no2 ~ ws + air_temp + bg.no2 + wd_category4 + month
model_wd_category4_month_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + month, data = df)
summary(model_wd_category4_month_bg.no2)

##### 14.) MODEL no2 ~ ws + air_temp + bg.no2 + wd_category8 + month
model_wd_category8_month_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category8 + month, data = df)
summary(model_wd_category8_month_bg.no2)

##### 15.) MODEL no2 ~ ws + air_temp + bg.no2 + wd_category4 + season
model_wd_category4_season_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + season, data = df)
summary(model_wd_category4_season_bg.no2)

##### 16.) MODEL no2 ~ ws + air_temp + bg.no2 + wd_category8 + season
model_wd_category8_season_bg.no2 <- lm(no2 ~ ws + air_temp + bg.no2 + wd_category8 + season, data = df)
summary(model_wd_category8_season_bg.no2)


#-------------------------------------------------
#-------------------------------------------------
##### MODELS WITH INTERACTIONS #####

#(Interactions created in Colinearity.R)


#### 1.) MODEL no2 ~ ws + air_temp + wd_category4 + air_temp_ws
model_wd_category4_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + wd_category4 + air_temp_ws, data=df)
summary(model_wd_category4_inter_AIRTEMP_WS)

#### 2.) MODEL no2 ~ ws + air_temp + wd_category8 + air_temp_ws
model_wd_category8_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + wd_category8 + air_temp_ws, data=df)
summary(model_wd_category8_inter_AIRTEMP_WS)


#### 3.) MODEL no2 ~ ws + air_temp + month + air_temp_ws
model_month_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + month + air_temp_ws, data=df)
summary(model_month_inter_AIRTEMP_WS)

#### 4.) MODEL no2 ~ ws + air_temp + season + air_temp_ws
model_season_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + season + air_temp_ws, data=df)
summary(model_season_inter_AIRTEMP_WS)

#-------------------------------------------------

#### 5.) MODEL no2 ~ ws + air_temp + wd_category4 + air_temp_ws + bg.no2
model_wd_category4_bg.no2_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + wd_category4 + air_temp_ws + bg.no2, data=df)
summary(model_wd_category4_bg.no2_inter_AIRTEMP_WS)

#### 6.) MODEL no2 ~ ws + air_temp + wd_category4 + bg.no2_ws + bg.no2
model_wd_category4_bg.no2_inter_BG.NO2_WS <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + bg.no2_ws, data=df)
summary(model_wd_category4_bg.no2_inter_BG.NO2_WS)

#### 7.) MODEL no2 ~ ws + air_temp + wd_category4 + bg.no2_air_temp + bg.no2
model_wd_category4_bg.no2_inter_BG.NO2_AIRTEMP <-lm(no2 ~ ws + air_temp + wd_category4 + bg.no2_air_temp + bg.no2 , data=df)
summary(model_wd_category4_bg.no2_inter_BG.NO2_AIRTEMP)

#-------------------------------------------------

#### 8.) MODEL no2 ~ ws + air_temp + month + air_temp_ws + bg.no2
model_month_bg.no2_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + month + air_temp_ws + bg.no2, data=df)
summary(model_month_bg.no2_inter_AIRTEMP_WS)

#### 9.) MODEL no2 ~ ws + air_temp + month + bg.no2_ws + bg.no2
model_month_bg.no2_inter_BG.NO2_WS <-lm(no2 ~ ws + air_temp + bg.no2 + month + bg.no2_ws, data=df)
summary(model_month_bg.no2_inter_BG.NO2_WS)

#### 10.) MODEL no2 ~ ws + air_temp + month + bg.no2_air_temp + bg.no2
model_month_bg.no2_inter_BG.NO2_AIRTEMP <-lm(no2 ~ ws + air_temp + month + bg.no2_air_temp + bg.no2 , data=df)
summary(model_month_bg.no2_inter_BG.NO2_AIRTEMP)

#-------------------------------------------------

#### 8.) MODEL no2 ~ ws + air_temp + season + air_temp_ws + bg.no2
model_season_bg.no2_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + season + air_temp_ws + bg.no2, data=df)
summary(model_season_bg.no2_inter_AIRTEMP_WS)

#### 9.) MODEL no2 ~ ws + air_temp + season + bg.no2_ws + bg.no2
model_season_bg.no2_inter_BG.NO2_WS <-lm(no2 ~ ws + air_temp + bg.no2 + season + bg.no2_ws, data=df)
summary(model_season_bg.no2_inter_BG.NO2_WS)

#### 10.) MODEL no2 ~ ws + air_temp + season + bg.no2_air_temp + bg.no2
model_season_bg.no2_inter_BG.NO2_AIRTEMP <-lm(no2 ~ ws + air_temp + season + bg.no2_air_temp + bg.no2 , data=df)
summary(model_season_bg.no2_inter_BG.NO2_AIRTEMP)


#-------------------------------------------------

#### 11.) MODEL no2 ~ ws + air_temp + wd_category4 + month +  air_temp_ws
model_wd_category4_month_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + wd_category4 + air_temp_ws + month, data=df)
summary(model_wd_category4_month_inter_AIRTEMP_WS)


#### 12.) MODEL no2 ~ ws + air_temp + wd_category4 + season +  air_temp_ws
model_wd_category4_season_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + wd_category4 + air_temp_ws + season, data=df)
summary(model_wd_category4_season_inter_AIRTEMP_WS)


#-------------------------------------------------

#### 13.) MODEL no2 ~ ws + air_temp + wd_category4 + month + bg.no2 + air_temp_ws
model_wd_category4_month_bg.no2_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + air_temp_ws + month, data=df)
summary(model_wd_category4_month_bg.no2_inter_AIRTEMP_WS)

#### 14.) MODEL no2 ~ ws + air_temp + wd_category4 + month + bg.no2 + bg.no2_ws
model_wd_category4_month_bg.no2_inter_BG.NO2_WS <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + bg.no2_ws + month, data=df)
summary(model_wd_category4_month_bg.no2_inter_BG.NO2_WS)

#### 15.) MODEL no2 ~ ws + air_temp + wd_category4 + month + bg.no2 + bg.no2_air_temp
model_wd_category4_month_bg.no2_inter_BG.NO2_AIRTEMP <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + bg.no2_air_temp + month, data=df)
summary(model_wd_category4_month_bg.no2_inter_BG.NO2_AIRTEMP)



#-------------------------------------------------

#### 16.) MODEL no2 ~ ws + air_temp + wd_category4 + season + bg.no2 + air_temp_ws
model_wd_category4_season_bg.no2_inter_AIRTEMP_WS <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + air_temp_ws + season, data=df)
summary(model_wd_category4_season_bg.no2_inter_AIRTEMP_WS)

#### 17.) MODEL no2 ~ ws + air_temp + wd_category4 + season + bg.no2 + bg.no2_ws
model_wd_category4_season_bg.no2_inter_BG.NO2_WS <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + bg.no2_ws + season, data=df)
summary(model_wd_category4_season_bg.no2_inter_BG.NO2_WS)

#### 18.) MODEL no2 ~ ws + air_temp + wd_category4 + season + bg.no2 + bg.no2_air_temp
model_wd_category4_season_bg.no2_inter_BG.NO2_AIRTEMP <-lm(no2 ~ ws + air_temp + bg.no2 + wd_category4 + bg.no2_air_temp + season, data=df)
summary(model_wd_category4_season_bg.no2_inter_BG.NO2_AIRTEMP)




