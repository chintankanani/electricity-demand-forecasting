setwd("~/R Class")

library(readr)
library(dplyr)
library(lubridate)


# Load CSV
df <- read_csv("powerdemand_5min_2021_to_2024_with weather.csv")

# rename column
df <- df %>%
  rename(
    power_demand = `Power demand`,
    temperature = temp,
    dew_point = dwpt,
    humidity = rhum,
    wind_dir = wdir,
    wind_speed = wspd,
    pressure = pres,
    moving_avg = moving_avg_3
  )


# Convert date column to Date format
df$datetime <- as.Date(df$datetime)

# Check missing values
colSums(is.na(df))

# Drop rows with any NA
df <- na.omit(df)

# 5-minute data: 6 lags = 30 min, 12 lags = 1 hour
# Create lagged variables using dplyr::lag()
df <- df %>%
  mutate(
    temperature_lag30min = lag(temperature, 6),
    temperature_lag1hr = lag(temperature, 12),
    humidity_lag30min = lag(humidity, 6),
    humidity_lag1hr = lag(humidity, 12),
    windspeed_lag30min = lag(wind_speed, 6),
    windspeed_lag1hr = lag(wind_speed, 12)
  )

# Check missing values
colSums(is.na(df))

# Drop rows with new NA values (from lagging)
df <- df %>%
  filter(
    !is.na(temperature_lag30min),
    !is.na(temperature_lag1hr),
    !is.na(humidity_lag30min),
    !is.na(humidity_lag1hr),
    !is.na(windspeed_lag30min),
    !is.na(windspeed_lag1hr)
  )

### Descriptive Statistics
summary(df)

#Checking skewness of the data
install.packages("e1071")  # Run this only once
library(e1071)

# Calculate skewness for all numeric variables

numeric_vars <- sapply(df, is.numeric)
skew_values <- sapply(df[, numeric_vars], skewness, na.rm = TRUE)

# View skewness

print(skew_values)


#Box plot for each numeric variable
library(ggplot2)
library(tidyr)

# List of numeric columns (modify as needed)
numeric_vars <- c("power_demand", "temperature", "humidity", "wind_speed", 
                  "temperature_lag30min", "temperature_lag1hr", 
                  "humidity_lag30min", "humidity_lag1hr", 
                  "windspeed_lag30min", "windspeed_lag1hr",
                  "pressure", "moving_avg")

# Plot individually
for (var in numeric_vars) {
  print(
    ggplot(df, aes_string(y = var)) +
      geom_boxplot(fill = "skyblue", outlier.color = "red") +
      ggtitle(paste("Boxplot of", var)) +
      theme_minimal()
  )
}

# Linearity & Homoscedasticity Assumption
# Fit basic MLR model
model <- lm(power_demand ~ temperature + humidity + wind_speed +
              temperature_lag30min + temperature_lag1hr +
              humidity_lag30min + humidity_lag1hr +
              windspeed_lag30min + windspeed_lag1hr +
              pressure + moving_avg, data = df)

# Residual plot
plot(model$fitted.values, model$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")

data <- data.frame(
  Predicted = model$fitted.values,
  Residuals = model$residuals
)

ggplot(data, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "#228B22", alpha = 0.7, size = 1.2) +  # Forest green
  geom_hline(yintercept = 0, color = "#8B4513", size = 1) +  # Saddle brown
  labs(title = "Residual Plot", x = "Predicted power_demand", y = "Residuals") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#f5f5dc", color = NA),  # Beige
    panel.background = element_rect(fill = "#f5f5dc", color = NA),
    panel.grid.major = element_line(color = "#d2b48c"),  # Tan grid
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )


#Autocorrelation / Independence of Residuals (Durbin-Watson Test)
library(lmtest)
dwtest(model)  # p > 0.05 means no autocorrelation (good)

#fix autocorrelation
# Include Lagged Dependent Variables (Autoregressive Terms)
df <- df %>%
  mutate(
    powerdemand_lag1 = lag(power_demand, 1),
    powerdemand_lag2 = lag(power_demand, 2)
  ) %>%
filter(!is.na(powerdemand_lag1), !is.na(powerdemand_lag2))

model_ar <- lm(power_demand ~ temperature + humidity + wind_speed +
                 temperature_lag30min + temperature_lag1hr +
                 humidity_lag30min + humidity_lag1hr +
                 windspeed_lag30min + windspeed_lag1hr +
                 powerdemand_lag1 + powerdemand_lag2,
               data = df)

library(lmtest)
dwtest(model_ar)

##Autocorrelation has been resolved

#Residual Plots for Linearity & Homoscedasticity
# Residual vs Fitted plot
plot(model_ar, which = 1)
abline(h = 0, col = "red")

# Normal Q-Q plot
plot(model_ar, which = 2)

#Histogram of Residuals
hist(residuals(model_ar), breaks = 50, col = "lightblue", main = "Histogram of Residuals")

library(ggplot2)
ggplot(data.frame(resid = residuals(model_ar)), aes(x = resid)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals")

#Check for Multicollinearity (VIF)
library(car)
vif(model_ar)

#Fixed the multicolinearity
#Calculate correlations between lagged variables and power demand
# Load required libraries
library(corrplot)
install.packages("ggcorrplot")
library(ggcorrplot)

# Select only numeric columns relevant for correlation check
cor_data <- df[, c("power_demand", 
                   "temperature", "temperature_lag30min", "temperature_lag1hr",
                   "humidity", "humidity_lag30min", "humidity_lag1hr",
                   "wind_speed", "windspeed_lag30min", "windspeed_lag1hr", "powerdemand_lag1", "powerdemand_lag2")]

# Compute correlation matrix
corr_matrix <- cor(df[, c("power_demand", "temperature", "temperature_lag30min",
                          "temperature_lag1hr", "humidity", "humidity_lag30min",
                          "humidity_lag1hr", "wind_speed", "windspeed_lag30min",
                          "windspeed_lag1hr", "powerdemand_lag1", "powerdemand_lag2")], use = "complete.obs")

# Visualize the correlation heatmap
corrplot(corr_matrix,
         method = "color",            # Color tiles
         type = "lower",              # Show only lower triangle
         tl.col = "black",            # Label color
         tl.cex = 0.7,
         tl.srt = 90,                 # Rotate labels
         addCoef.col = "black",       # Add correlation coefficients
         number.cex = 0.7,            # Coefficient font size
         number.digits = 2,
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Color gradient
         title = "Correlation Matrix", # Add title
         mar = c(0,0,1,0)) 

#Correlation between powerdemand lag
cor(df[, c("power_demand", "powerdemand_lag1", "powerdemand_lag2")], use = "complete.obs")

#Create MLR Model with weather lagged variables
# Split data (e.g., 80-20 train-test)
set.seed(123) 
n <- nrow(df)
train_index <- 1:floor(0.8 * n)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

#Only Weather Lagged Variables
model_lagged <- lm(power_demand ~ temperature_lag30min + humidity_lag30min + windspeed_lag30min +
                powerdemand_lag1, data = train_data)
#summary
summary(model_lagged)

#Predict on Test Set
# Predictions
pred_lagged <- predict(model_lagged, newdata = test_data)

# Actual values
actual <- test_data$power_demand

#Calculate RMSE and R²
# Load Metrics Library
install.packages("Metrics")
library(Metrics)

#Model Perdormance
rmse_lagged <- rmse(actual, pred_lagged)
r2_lagged <- 1 - sum((actual - pred_lagged)^2) / sum((actual - mean(actual))^2)

# Display
cat("Model with only Lagged Weather):\nRMSE =", rmse_lagged, ", R² =", r2_lagged, "\n")

#Compare model with different temperature lag
#Model A: current temperature
model_A <- lm(power_demand ~ temperature + humidity_lag30min + windspeed_lag30min +
                powerdemand_lag1, data = train_data)

#Model B: temperature_lag30min
model_B <- lm(power_demand ~ temperature_lag30min + humidity_lag30min + windspeed_lag30min +
                powerdemand_lag1, data = train_data)

#Model C: temperature_lag1hr
model_C <- lm(power_demand ~ temperature_lag1hr + humidity_lag30min + windspeed_lag30min +
                powerdemand_lag1, data = train_data)

#summary of the model
summary(model_A)
summary(model_B)
summary(model_C)

#Predict on Test Set
# Predictions
pred_A <- predict(model_A, newdata = test_data)
pred_B <- predict(model_B, newdata = test_data)
pred_C <- predict(model_C, newdata = test_data)

# Actual values
actual <- test_data$power_demand

#Calculate RMSE and R²
# Load Metrics Library
install.packages("Metrics")
library(Metrics)

# Model A Performance
rmse_A <- rmse(actual, pred_A)
r2_A <- 1 - sum((actual - pred_A)^2) / sum((actual - mean(actual))^2)

# Model B Performance
rmse_B <- rmse(actual, pred_B)
r2_B <- 1 - sum((actual - pred_B)^2) / sum((actual - mean(actual))^2)

#Model C Perdormance
rmse_C <- rmse(actual, pred_C)
r2_C <- 1 - sum((actual - pred_C)^2) / sum((actual - mean(actual))^2)

# Display
cat("Model A (Current Temperature Only):\nRMSE =", rmse_A, ", R² =", r2_A, "\n")
cat("Model B (Temperature Lagged 30min):\nRMSE =", rmse_B, ", R² =", r2_B, "\n")
cat("Model C (Temperaure Lagged 1hr):\nRMSE =", rmse_C, ", R² =", r2_B, "\n")

#AIC comparison
AIC(model_A, model_B, model_C)

# Plot Actual vs Predicted values
par(bg = "#f5f5dc")
# Create the plot
plot(test_data$power_demand, pred_B,
     xlab = "Actual Power Demand",
     ylab = "Predicted Power Demand",
     main = "Predicted vs Actual Power Demand (MLR)",
     col = "#228B22", pch = 16,
     panel.first = rect(par("usr")[1], par("usr")[3],
                        par("usr")[2], par("usr")[4],
                        col = "#f5f5dc", border = NA))  # Panel background

# Add 45-degree reference line
abline(a = 0, b = 1, col = "#8B4513", lwd = 2)

#Random Forest
install.packages("caret")
library(randomForest)
library(caret)

#Prepare the dataset
# Drop rows with NA if any (e.g., due to lagging)
df_clean <- na.omit(df)

# Define features (same as MLR for fair comparison)
features <- c("temperature_lag30min", "humidity_lag30min", "windspeed_lag30min",
              "powerdemand_lag1")

# Train-test split (80-20)
set.seed(123)
n <- nrow(df_clean)
train_index <- 1:floor(0.8 * n)
train_data <- df_clean[train_index, ]
test_data <- df_clean[-train_index, ]

#Train the Random Forest model
# Build model
rf_model <- randomForest(power_demand ~ ., data = train_data[, c("power_demand", features)], ntree = 500)

# Predict on test data

pred_rf <- predict(rf_model, newdata = test_data[, features])


#Evaluate the performance
# Evaluation metrics
MAE_rf <- mean(abs(pred_rf - test_data$power_demand))
RMSE_rf <- sqrt(mean((pred_rf - test_data$power_demand)^2))
R2_rf <- 1 - sum((pred_rf - test_data$power_demand)^2) / sum((mean(train_data$power_demand) - test_data$power_demand)^2)

# Print results

cat("Random Forest Results:\n")
cat("MAE:", MAE_rf, "\nRMSE:", RMSE_rf, "\nR²:", R2_rf, "\n")

# Actual vs Predicted Plot - RF

plot(test_data$power_demand, pred_rf,
     xlab = "Actual Power Demand",
     ylab = "Predicted Power Demand",
     main = "Predicted vs Actual Power Demand (Random Forest)",
     col = "darkgreen", pch = 16)
abline(a = 0, b = 1, col = "red", lwd = 2)  # 45-degree reference line

#Variable Importance Plot

importance(rf_model)
varImpPlot(rf_model)

#Step-by-Step Code for Bar Plot of Variable Importance

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Extract variable importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Sort by importance
importance_df <- importance_df[order(importance_df$IncMSE, decreasing = TRUE), ]

# Create the bar chart
ggplot(importance_df, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (%IncMSE)",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()

#Weather-Only Variable Importance Bar Plot

# Extract importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Filter to include only weather-related variables
weather_vars <- c("temperature", "humidity_lag30min", "windspeed_lag30min")
weather_importance <- importance_df[importance_df$Variable %in% weather_vars, ]

# Sort by importance
weather_importance <- weather_importance[order(weather_importance$IncMSE, decreasing = TRUE), ]

# Create the bar plot
ggplot(weather_importance, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (Weather Variables)",
    x = "Weather Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()



......................*************************************

# Subset data with selected features
selected_features <- df[, c("power_demand", 
                            "temperature", 
                            "humidity", 
                            "windspeed", 
                            "powerdemand_lag1")]

# Fit the MLR model with only selected features
# Split data (e.g., 80-20 train-test)
set.seed(123)
n <- nrow(selected_features)
train_index <- 1:floor(0.8 * n)
train_data <- selected_features[train_index, ]
test_data <- selected_features[-train_index, ]

# Fit model
model_mlr_reduced <- lm(power_demand ~ ., data = train_data)

# Summary
summary(model_mlr_reduced)

#Variable Importance Plot

importance(rf_model)
#varImpPlot(rf_model)

#Step-by-Step Code for Bar Plot of Variable Importance

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Extract variable importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Sort by importance
importance_df <- importance_df[order(importance_df$IncMSE, decreasing = TRUE), ]

# Create the bar chart
ggplot(importance_df, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (%IncMSE)",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()

#Weather-Only Variable Importance Bar Plot

# Extract importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Filter to include only weather-related variables
weather_vars <- c("temperature", "humidity_lag30min", "windspeed_lag30min")
weather_importance <- importance_df[importance_df$Variable %in% weather_vars, ]

# Sort by importance
weather_importance <- weather_importance[order(weather_importance$IncMSE, decreasing = TRUE), ]

# Create the bar plot
ggplot(weather_importance, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (Weather Variables)",
    x = "Weather Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()



................................
#Remove or Combine Redundant Variables
model_clean <- lm(power_demand ~ temperature_lag30min + humidity_lag30min +
                    windspeed_lag30min + powerdemand_lag1,
                  data = df)
vif(model_clean)

#Model Summary
summary(model_clean)

#Fit Multiple Models
model_temp_current <- lm(power_demand ~ temperature + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)
model_temp_15min <- lm(power_demand ~ temperature_lag15min + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)
model_temp_30min <- lm(power_demand ~ temperature_lag30min + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)
model_temp_45min <- lm(power_demand ~ temperature_lag45min + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)
model_temp_1hr <- lm(power_demand ~ temperature_lag1hr + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)

#Compare Models
#Summary outputs
summary(model_temp_current)
summary(model_temp_15min)
summary(model_temp_30min)
summary(model_temp_45min)
summary(model_temp_1hr)

#AIC comparison
AIC(model_temp_current, model_temp_15min, model_temp_30min, model_temp_45min, model_temp_1hr)

#validate model assumptions

#Residual Plot
plot(model_temp_current$residuals, main="Residual Plot", ylab="Residuals", xlab="Index")
abline(h=0, col="red")

#QQ Plot (Normality Check)
qqnorm(model_temp_current$residuals)
qqline(model_temp_current$residuals, col="red")

#Histogram of Residuals
hist(model_temp_current$residuals, breaks=50, main="Histogram of Residuals", xlab="Residuals")

#Plot Fitted vs Actual Values
plot(df$power_demand, fitted(model_temp_current), 
     xlab = "Actual", ylab = "Fitted", 
     main = "Actual vs Fitted Values")
abline(0, 1, col = "blue")

#Forecasting Accuracy

#MLR with Lag & Weather Features
# Features with best predictive performance
final_model <- lm(power_demand ~ temperature + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = df)

# Split Data into Train & Test Sets
set.seed(123)  # Reproducibility
n <- nrow(df)
train_index <- 1:floor(0.8 * n)
train <- df[train_index, ]
test <- df[-train_index, ]

#Train Model on Training Set
mlr_model <- lm(power_demand ~ temperature + humidity_lag30min + windspeed_lag30min + powerdemand_lag1, data = train)

#Predict on Test Set
predictions <- predict(mlr_model, newdata = test)

#Calculate Forecasting Accuracy
# Actual values
actuals <- test$power_demand

# Error metrics
MAE <- mean(abs(actuals - predictions))
RMSE <- sqrt(mean((actuals - predictions)^2))
MAPE <- mean(abs((actuals - predictions) / actuals)) * 100

cat("MAE:", MAE, "\nRMSE:", RMSE, "\nMAPE (%):", MAPE)

#Visualize Predictions vs Actual
plot(actual, pred_mlr, main = "MLR: Actual vs Predicted Power Demand",
     xlab = "Actual Power Demand", ylab = "Predicted Power Demand", col = "maroon", pch = 20)
abline(0, 1, col = "red")

#OR
plot(actuals, type = "l", col = "blue", lwd = 2, ylab = "Power Demand", main = "Actual vs Predicted")
lines(predictions, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)


# Random Forest in R
# Load the Required Library

install.packages("randomForest")
library(randomForest)

# Prepare the Dataset
set.seed(123)

# Select relevant features
df_rf <- df[, c("power_demand", "temperature", "humidity_lag30min", 
                "windspeed_lag30min", "powerdemand_lag1")]

# Split into train and test (80-20)
n <- nrow(df_rf)
train_index <- 1:round(0.8 * n)
train_rf <- df_rf[train_index, ]
test_rf <- df_rf[-train_index, ]

#Train the Random Forest Model
rf_model <- randomForest(power_demand ~ ., data = train_rf, ntree = 100, mtry = 2, importance = TRUE)
print(rf_model)

#Predict on Test Data

rf_pred <- predict(rf_model, newdata = test_rf)

#Evaluate Performance
# Actual values

actual <- test_rf$power_demand

# Error metrics

MAE <- mean(abs(actual - rf_pred))
RMSE <- sqrt(mean((actual - rf_pred)^2))
MAPE <- mean(abs((actual - rf_pred) / actual)) * 100

cat("Random Forest Results:\n")
print(MAE)
print(RMSE)
print(MAPE)

cat("MAE:", MAE, "\nRMSE:", RMSE, "\nMAPE (%):", MAPE, "\n")

#visualize prediction errors (residuals)

# Actual vs Predicted Plot - RF
plot(actual, rf_pred, main = "RF: Actual vs Predicted",
     xlab = "Actual", ylab = "Predicted", col = "forestgreen", pch = 20)
abline(0, 1, col = "red")

# Residuals
residuals_mlr <- actual - pred_mlr
residuals_rf <- actual - pred_rf
par(mfrow = c(1, 2))  # Side-by-side again

#Residul plot
plot(pred_mlr, residuals_mlr, main = "MLR Residuals",
     xlab = "Predicted", ylab = "Residuals", col = "blue", pch = 20)
abline(h = 0, col = "red")

plot(pred_rf, residuals_rf, main = "RF Residuals",
     xlab = "Predicted", ylab = "Residuals", col = "forestgreen", pch = 20)
abline(h = 0, col = "red")


#Variable Importance Plot

importance(rf_model)
#varImpPlot(rf_model)

#Step-by-Step Code for Bar Plot of Variable Importance

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Extract variable importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Sort by importance
importance_df <- importance_df[order(importance_df$IncMSE, decreasing = TRUE), ]

# Create the bar chart
ggplot(importance_df, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (%IncMSE)",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()

#Weather-Only Variable Importance Bar Plot

# Extract importance
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  IncMSE = importance(rf_model)[, "%IncMSE"]
)

# Filter to include only weather-related variables
weather_vars <- c("temperature", "humidity_lag30min", "windspeed_lag30min")
weather_importance <- importance_df[importance_df$Variable %in% weather_vars, ]

# Sort by importance
weather_importance <- weather_importance[order(weather_importance$IncMSE, decreasing = TRUE), ]

# Create the bar plot
ggplot(weather_importance, aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (Weather Variables)",
    x = "Weather Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()

