library(readxl)
library(neuralnet)
library(dplyr)
library(grid)
library(MASS)
library(Metrics)
library(MLmetrics) #built in function – different package
library(forecast)
library(caret)
library(knitr)


#################################
#This is for level T4
# SettingMASS::# Settingneuralnet()# Setting the directory
setwd("C:/Users/ehima/OneDrive/Desktop/Year2/Semester2/Machine Learning/CourseWork")
getwd()

# Reading the Excel file
dataset <- read_excel("ExchangeUSD.xlsx")

# Writing to CSV (running only once)
# write.csv(dataset, "Exchange.csv", row.names = FALSE)

# Reading the CSV file
Thedata <- read.csv("Exchange.csv")
head(Thedata)

# Extracting the 'USD.EUR' column
inputsource <- data.frame(USD_EUR=c(Thedata$USD.EUR))
inputsource_rand<-inputsource[sample(1:nrow(inputsource)), ]##randomizing all the values
inputsource_rand<-inputsource





##################################
#For t-level 4
#4 columns in this I/O matrix. 
time_lagged_data1 <- bind_cols(G_previous2 = lag(inputsource_rand,3),
                              G_previous = lag(inputsource_rand,2),
                              G_current = lag(inputsource_rand,1),
                              G_pred = inputsource_rand)    
time_lagged_data1 <- time_lagged_data1[complete.cases(time_lagged_data1),]
head(time_lagged_data1)
colnames(time_lagged_data1) <- c("G_previous2", "G_previous", "G_current", "G_pred")

####################
# Normalizing function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#Normalize the data
data_norm1=as.data.frame(lapply(time_lagged_data1,normalize))

summary(data_norm1)
#summary of my output
summary(data_norm1$G_pred)
########### 3 vector time Lagged
#summary of my output
summary(data_norm1$G_pred)

####################
#Training the Model Using the neural-net function
##Training data is first 400
##Testing data is last 100 a s the data is 500

# Extracting first 400 data points for training
#randomising the data to avoid over fitting
set.seed(123)  # Setting seed for reproducibility
train_data1 <- data_norm1[1:398,]
test_data1 <- time_lagged_data1[399:497,]

# Define the formula for the neural network
formula1 <- as.formula(G_pred ~ G_previous2 + G_previous + G_current)
formula2 <- as.formula(G_pred ~  G_previous + G_current)

##### Hidden layers
hidden_layers<-list(c(10,4),c(8,4),c(2,4),c(6),
                    c(3,5),c(4),c(3),c(1,5),c(8,5),c(10,5),c(2,3),c(3,5),c(8,3),c(9,4),c(2,7)
                     )
activation_functions <- c('logistic', 'tanh')



# Training the neural network model
#--Storing values like comparasion
comparix1 <- data.frame(
  mape_value = numeric(),
  rmse_value = numeric(),
  mae_value = numeric(),
  smape_value = numeric(),
  hidden_layer = character(),
  activation_function = character(),
  stringsAsFactors = FALSE
)
num_test_samples <- nrow(test_data1)
USD_EUR_pred_nn1 <- matrix(NA, nrow = num_test_samples, ncol = length(hidden_layers) * length(activation_functions))

# Assigning words to the first row
comparix1[1, ] <- c("MAPE", "RMSE", "MAE", "SMAPE", "HIDDENLAYER", "ACTIVATION FUNCTION")

set.seed(50)
# Loop through different model configurations
for (i in seq_along(hidden_layers)) {
  cat("Training neural network model with hidden layers configuration", i, "\n")
  for (z in seq_along(activation_functions)) {
    cat("Training neural network model with activation function:", activation_functions[z], "\n")
    
    # Train neural network model
    concrete_model1 <- neuralnet(
      formula1,
      data = train_data1,
      hidden = hidden_layers[[i]],
      linear.output = FALSE,
      act.fct = activation_functions[z],
      err.fct = "sse"
    )
    
    
    # Predictions on test data
    predictions <- predict(concrete_model1, test_data1[, c("G_previous2","G_previous", "G_current")])
    plot(predictions)
    # Denormalization
    unnormalize <- function(x, min, max) {return( (max - min)*x + min )}
    min_val <- min(inputsource_rand)
    max_val <- max(inputsource_rand)
    USD_EUR_pred <- unnormalize(predictions,min_val,max_val)
    col_index <- (i - 1) * length(activation_functions) + z
    USD_EUR_pred_nn1[, col_index] <- USD_EUR_pred
    # Compute performance metrics
    mape_value <- mape(test_data1$G_pred, USD_EUR_pred)
    rmse_value <- rmse(test_data1$G_pred, USD_EUR_pred)
    mae_value <- mae(test_data1$G_pred, USD_EUR_pred)
    smape_value <- smape(test_data1$G_pred, USD_EUR_pred)
    
    # Store performance metrics and model configuration
    comparix1 <- rbind(comparix1, list(mape_value, rmse_value, mae_value, smape_value, paste(hidden_layers[i],sep = ""), activation_functions[z]))
  }
}

# Print the data frame
print(comparix1)# Print the comparison table with kable for better formatting
kable(comparix1, caption = "Comparison of MLP Performances", align = "c")
# Extracting the relevant columns from the data frames
desired1 <- test_data1$G_pred
predicted1 <- USD_EUR_pred_nn1[6]



# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(desired1 - predicted1))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((desired1 - predicted1)^2))

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((desired1 - predicted1) / desired1)) * 100
# Print the performance indices
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")











##################plotting
# Scatterplot of actual vs. predicted values
par(mfrow=c(1,1))  # Set the plotting layout outside the loop

plot(test_data1$G_pred, USD_EUR_pred_nn1[,6], col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(a=0, b=1, h=90, v=90)


x <- 1:length(test_data1$G_pred)
plot(x, test_data1$G_pred, col = "red", type = "l", lwd = 2,
     main = "USD/EUR Prediction")
lines(x, USD_EUR_pred_nn1[7], col = "blue", lwd = 2)
legend("topright", legend = c("original-strength", "predicted-strength"),
       fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()


#########################
#T-level-3
#4 columns in this I/O matrix. 
time_lagged_data2 <- bind_cols(
                               G_previous = lag(inputsource_rand,2),
                               G_current = lag(inputsource_rand,1),
                               G_pred = inputsource_rand)    
time_lagged_data2 <- time_lagged_data2[complete.cases(time_lagged_data2),]
head(time_lagged_data2)
colnames(time_lagged_data2) <- c( "G_previous", "G_current", "G_pred")

####################
#Normalize the data
data_norm2=as.data.frame(lapply(time_lagged_data2,normalize))

summary(data_norm2)
#summary of my output
summary(data_norm2$G_pred)
########### 3 vector time Lagged
#summary of my output
summary(data_norm2$G_pred)

####################
#Training the Model Using the neural-net function
##Training data is first 400
##Testing data is last 100 a s the data is 500

# Extracting first 400 data points for training
#randomising the data to avoid over fitting
set.seed(123)  # Setting seed for reproducibility
train_data2 <- data_norm2[1:398,]
test_data2 <- time_lagged_data2[399:498,]

# Define the formula for the neural network
formula2 <- as.formula(G_pred ~ G_previous + G_current)

# Training the neural network model
#--Storing values like comparasion
comparix <- data.frame(
  mape_value = numeric(),
  rmse_value = numeric(),
  mae_value = numeric(),
  smape_value = numeric(),
  hidden_layer = character(),
  activation_function = character(),
  stringsAsFactors = FALSE
)

# Assigning words to the first row
comparix[1, ] <- c("MAPE", "RMSE","MAE", "SMAPE", "HIDDENLAYER","ACTIVATION FUNCTION")
num_test_samples2 <- nrow(test_data2)
USD_EUR_pred_nn2 <- matrix(NA, nrow = num_test_samples2, ncol = length(hidden_layers) * length(activation_functions))


# Loop through different model configurations
for (i in seq_along(hidden_layers)) {
  cat("Training neural network model with hidden layers configuration", i, "\n")
  for (z in seq_along(activation_functions)) {
    cat("Training neural network model with activation function:", activation_functions[z], "\n")
    
    # Train neural network model
    concrete_model2 <- neuralnet(
      formula2,
      data = train_data2,
      hidden = hidden_layers[[i]],
      linear.output = TRUE,
      act.fct = activation_functions[z],
      err.fct = "sse"
    )
    
    # Predictions on test data
    predictions <- predict(concrete_model2, test_data2[, c("G_previous", "G_current")])
    
    # Denormalization
    unnormalize <- function(x, min, max) {return( (max - min)*x + min )}
    min_val <- min(inputsource_rand)
    max_val <- max(inputsource_rand)
    USD_EUR_pred2 <- unnormalize(predictions,min_val,max_val)
    col_index <- (i - 1) * length(activation_functions) + z
    USD_EUR_pred_nn2[, col_index] <- USD_EUR_pred2
    
    # Compute performance metrics
    mape_value2 <- mape(test_data2$G_pred, USD_EUR_pred2)
    rmse_value2 <- rmse(test_data2$G_pred, USD_EUR_pred2)
    mae_value2 <- mae(test_data2$G_pred, USD_EUR_pred2)
    smape_value2 <- smape(test_data2$G_pred, USD_EUR_pred2)
    
    # Store performance metrics and model configuration
    comparix <- rbind(comparix, list(mape_value2, rmse_value2, mae_value2, smape_value2, paste(hidden_layers[i],sep = ""), activation_functions[z]))
  }
}

# Print the data frame
print(comparix)
kable(comparix, caption = "Comparison of MLP Performances", align = "c")
# Extracting the relevant columns from the data frames
desired2 <- test_data1$G_pred
predicted2 <- USD_EUR_pred_nn2[4]


# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(desired2 - predicted2))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((desired2 - predicted2)^2))

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((desired2 - predicted2) / desired2)) * 100

# Print the performance indices
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")


par(mfrow=c(1,1))
plot(test_data2$G_pred, predicted2, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(a=0, b=1, h=90, v=90)

x <- 1:length(test_data2$G_pred)
plot(x, test_data2$G_pred_nn2[6], col = "red", type = "l", lwd = 2,
     main = "USD/EUR Prediction")
lines(x, predicted2, col = "blue", lwd = 2)
legend("topright", legend = c("original-strength", "predicted-strength"),
       fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()



