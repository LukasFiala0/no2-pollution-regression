# Function foe extraction of METRICS
extract_model_metrics <- function(model, model_name) {
  summary_model <- summary(model)
  data.frame(
    Model = model_name,
    AIC = AIC(model),
    BIC = BIC(model),
    Rsquared = summary_model$r.squared,
    Adjusted_Rsquared = summary_model$adj.r.squared,
    Residual_Std_Error = summary_model$sigma,
    F_Statistic = summary_model$fstatistic[1],
    P_Value_F = pf(summary_model$fstatistic[1], 
                   summary_model$fstatistic[2], 
                   summary_model$fstatistic[3], 
                   lower.tail = FALSE)
  )
}

# List of all the cerated models
models <- list(
  model_wd_category4, model_wd_category8, model_month, model_season,
  model_wd_category4_bg.no2, model_wd_category8_bg.no2, model_month_bg.no2, model_season_bg.no2,
  model_wd_category4_month, model_wd_category8_month, model_wd_category4_season, model_wd_category8_season,
  model_wd_category4_month_bg.no2, model_wd_category8_month_bg.no2, model_wd_category4_season_bg.no2, model_wd_category8_season_bg.no2,
  model_wd_category4_inter_AIRTEMP_WS, model_wd_category8_inter_AIRTEMP_WS, model_month_inter_AIRTEMP_WS, model_season_inter_AIRTEMP_WS,
  model_wd_category4_bg.no2_inter_AIRTEMP_WS, model_wd_category4_bg.no2_inter_BG.NO2_WS, model_wd_category4_bg.no2_inter_BG.NO2_AIRTEMP,
  model_month_bg.no2_inter_AIRTEMP_WS, model_month_bg.no2_inter_BG.NO2_WS, model_month_bg.no2_inter_BG.NO2_AIRTEMP,
  model_season_bg.no2_inter_AIRTEMP_WS, model_season_bg.no2_inter_BG.NO2_WS, model_season_bg.no2_inter_AIRTEMP_WS,
  model_wd_category4_month_inter_AIRTEMP_WS, model_wd_category4_season_inter_AIRTEMP_WS,
  model_wd_category4_month_bg.no2_inter_AIRTEMP_WS, model_wd_category4_month_bg.no2_inter_BG.NO2_WS, model_wd_category4_month_bg.no2_inter_BG.NO2_AIRTEMP,
  model_wd_category4_season_bg.no2_inter_AIRTEMP_WS, model_wd_category4_season_bg.no2_inter_BG.NO2_WS, model_wd_category4_season_bg.no2_inter_BG.NO2_AIRTEMP
)

model_names <- c(
  "Model wd_category4", "Model wd_category8", "Model month", "Model season",
  "Model wd_category4 + bg.no2", "Model wd_category8 + bg.no2", "Model month + bg.no2", "Model season + bg.no2",
  "Model wd_category4 + month", "Model wd_category8 + month", "Model wd_category4 + season", "Model wd_category8 + season",
  "Model wd_category4 + month + bg.no2", "Model wd_category8 + month + bg.no2", "Model wd_category4 + season + bg.no2", "Model wd_category8 + season + bg.no2",
  "Model wd_category4 + air_temp_ws", "Model wd_category8 + air_temp_ws", "Model month + air_temp_ws", "Model season + air_temp_ws",
  "Model wd_category4 + bg.no2 + air_temp_ws", "Model wd_category4 + bg.no2_ws", "Model wd_category4 + bg.no2_air_temp",
  "Model month + bg.no2 + air_temp_ws", "Model month + bg.no2_ws", "Model month + bg.no2_air_temp",
  "Model season + bg.no2 + air_temp_ws", "Model season + bg.no2_ws", "Model season + bg.no2_air_temp",
  "Model wd_category4 + month + air_temp_ws", "Model wd_category4 + season + air_temp_ws",
  "Model wd_category4 + month + bg.no2 + air_temp_ws", "Model wd_category4 + month + bg.no2_ws", "Model wd_category4 + month + bg.no2_air_temp",
  "Model wd_category4 + season + bg.no2 + air_temp_ws", "Model wd_category4 + season + bg.no2_ws", "Model wd_category4 + season + bg.no2_air_temp"
)

# Applz the function on all the models
model_metrics <- do.call(rbind, lapply(seq_along(models), function(i) {
  extract_model_metrics(models[[i]], model_names[i])
}))



print(model_metrics)

# EXPORT TO CSV
write.csv(model_metrics, "model_comparison.csv", row.names = FALSE)
