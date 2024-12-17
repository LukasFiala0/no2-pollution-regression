# Load necessary libraries
library(AQEval)  # For the dataset
library(ggplot2) # For advanced plotting
library(dplyr)   # For data manipulation
library(GGally)  # For scatterplot matrix

# Load the dataset
data("aq.data")
df <- aq.data

# View the structure of the dataset
str(df)

# Get rid of the NA values
df <- na.omit(df)


# Comprehensive descriptive statistics for all numeric variables
print(summary(df))

# Generate histograms for each variable
variables <- c("no2", "bg.no2", "ws", "wd", "air_temp")
for (var in variables) {
  p <- ggplot(df, aes_string(x = var)) +
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
    theme_minimal() +
    ggtitle(paste("Histogram of", var)) +
    xlab(var) + ylab("Frequency")
  print(p)
}

# Generate boxplots for each variable
for (var in variables) {
  p <- ggplot(df, aes_string(y = var)) +
    geom_boxplot(fill = "tomato", color = "black") +
    theme_minimal() +
    ggtitle(paste("Boxplot of", var)) +
    ylab(var)
  print(p)
}


# TEST THE NORMALLITY

variables <- c("no2", "bg.no2", "ws", "wd", "air_temp")
for (var in variables) {
  p <- ggplot(df, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot for", var)) +
    theme_minimal()
  
  print(p)
}


set.seed(123) #reproducibility
sample_size <- 5000

#  Shapiro-Wilk test
apply(df[, variables], 2, function(x) {
  sample_data <- sample(x, sample_size, replace = FALSE)
  shapiro.test(sample_data)$p.value
})

# Unfortunately none of the variables has the normal distribution
# also the dataset is full of outliers, but we dont know from where they come from
# since nno further description is in the DATASET CATALOG

# Scatterplot matrix (pairplot)
ggpairs(df[, variables]) +
  theme_minimal() +
  ggtitle("Scatterplot Matrix of Variables")

# Additional exploration for time (e.g., monthly trends)
# Extract month from the date column
df <- df %>%
  mutate(month = format(as.Date(date), "%m"))

# Aggregate data by month and plot trends
monthly_avg <- df %>%
  group_by(month) %>%
  summarise(
    no2_avg = mean(no2, na.rm = TRUE),
    bg_no2_avg = mean(bg.no2, na.rm = TRUE)
  )

# Plot monthly trends
ggplot(monthly_avg, aes(x = as.numeric(month))) +
  geom_line(aes(y = no2_avg, color = "NO2"), size = 1) +
  geom_line(aes(y = bg_no2_avg, color = "Background NO2"), size = 1) +
  theme_minimal() +
  ggtitle("Monthly Averages of NO2 and Background NO2") +
  xlab("Month") +
  ylab("Concentration") +
  scale_color_manual(values = c("NO2" = "blue", "Background NO2" = "green"))


# Dependency of NO2 and bg.NO2 on wind speed (ws)
ggplot(df, aes(x = ws)) +
  geom_point(aes(y = no2, color = "NO2"), alpha = 0.6) +
  geom_point(aes(y = bg.no2, color = "Background NO2"), alpha = 0.6) +
  geom_smooth(aes(y = no2, color = "NO2smooth"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = bg.no2, color = "Background NO2smooth"), method = "loess", se = FALSE) +
  theme_minimal() +
  ggtitle("Dependency of NO2 and Background NO2 on Wind Speed") +
  xlab("Wind Speed (ws)") +
  ylab("Concentration") +
  scale_color_manual(values = c("NO2" = "blue", "Background NO2" = "green","NO2smooth" = "red", "Background NO2smooth"="black"))



