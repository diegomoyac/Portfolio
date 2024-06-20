##########################################################################################################
###                                      Assignment 5                                   ###
##########################################################################################################

# ------------------------------
# Step 1: Load Required Packages
# ------------------------------
# If not already installed, install necessary packages
install.packages("dplyr")
install.packages("lubridate")

# Load packages for data manipulation, and working with dates
library(dplyr) 
library(lubridate) 

# ------------------------------------------
# Import and Process Data
# ------------------------------------------
# Load ship case data from a CSV file
df = read.csv("df_store.csv", header = TRUE, sep = "," )

# ------------------------------------------
# Linear regression
# ------------------------------------------
linear_df = lm(Profit ~ 
                MTenure + CTenure + Pop + Comp + 
                Visibility + PedCount + Res + Hours24, data = df)
print(summary(linear_df))

# Create the residuals vs fitted values plot
plot(linear_df$fitted.values, linear_df$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

# Create the Normal Q-Q plot
qqnorm(linear_df$residuals, main = "Normal Q-Q Plot")
qqline(linear_df$residuals, col = 2)  # Add a reference line

# Create a scatter plot of the data points no line
plot(df$Profit, linear_df$fitted.values, 
     xlab = "Observed Profit", ylab = "Fitted Profit",
     main = "Linear Regression Line")

# Add the regression line
abline(linear_df, col = "red")

# Assuming 'ols_df' is your linear regression model
predicted_values <- predict(linear_df)
plot(df$Profit, predicted_values, xlab = "Actual Profit", ylab = "Predicted Profit", main = "Actual vs. Predicted Profit")
abline(lm(df$Profit ~ predicted_values), col = "red")


## c)
bonus_impact = coef(linear_df)["CTenure"] * 1.38
low = (bonus_impact/4) - ((21000/4) * .03)
medium = (bonus_impact/4) - ((32000/4) * .03)
high =  (bonus_impact/4) - ((45000/4) *.03)

## e)
alpha = .05
z_value = qnorm(1 - alpha/2)
lower_bound = coef(linear_df)["Pop"] - z_value * summary(linear_df)$coefficients["Pop", "Std. Error"]
upper_bound = coef(linear_df)["Pop"] + z_value * summary(linear_df)$coefficients["Pop", "Std. Error"]
ci_pop = c(lower_bound, upper_bound)

## f)
linear2_df = lm(Profit ~ MTenure + CTenure, data = df)
print(summary(linear2_df)) 

## g)
quadratic_df = lm(formula = Profit ~ MTenure + I(MTenure^2) + CTenure + I(CTenure^2), data = df)
print(summary(quadratic_df)) 

quadratic2_df = lm(formula = Profit ~ MTenure + I(MTenure^2) + CTenure + I(CTenure^2) + 
                 Pop + Comp + Visibility + PedCount + Res + Hours24, data = df)
print(summary(quadratic2_df)) 

with(df,plot(MTenure, Profit, pch=19)) # scatter plot
with(df,plot(CTenure, Profit, pch=19)) # scatter plot


## h)
beta1 = coef(quadratic2_df)["MTenure"]
beta2 = coef(quadratic2_df)["I(MTenure^2)"]

critical_point = -beta1 / (2 * beta2)




