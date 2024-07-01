#Step 1: Set Up the Environment and Load Libraries
#NSSO
library(dplyr)
library(car)

#Step 2: Load the Dataset and Inspect It
setwd('D:\\#YPR\\VCU\\Summer Courses\\SCMA\\Data')
getwd()

# Load the dataset
data <- read.csv("NSSO68.csv")
head(data)
unique(data$state_1)

#Step 3: Subset the Data for the Assigned State ('KA') and Perform Missing Value Imputation
# Subset data to state assigned
subset_data <- data %>%
  filter(state_1 == 'AP') %>%
  select(foodtotal_q, MPCE_MRP, MPCE_URP,Age,Meals_At_Home,Possess_ration_card,Education, No_of_Meals_per_day)
print(subset_data)

sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$Age))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(data$Education))

impute_with_mean <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Columns to impute
columns_to_impute <- c("Education")

# Impute missing values with mean
data <- impute_with_mean(data, columns_to_impute)

sum(is.na(data$Education))


#Step 4: Fit the Multiple Regression Model
# Fit the regression model
model <- lm(foodtotal_q~ MPCE_MRP+MPCE_URP+Age+Meals_At_Home+Possess_ration_card+Education, data = subset_data)

# Print the regression results
print(summary(model))


install.packages("car")
library(car)

#Step 5: Perform Regression Diagnostics
# Check for multicollinearity using Variance Inflation Factor (VIF)
vif_values <- vif(model)
print(vif_values)

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}

# Print the equation
print(equation)


head(subset_data$MPCE_MRP,1)
head(subset_data$MPCE_URP,1)
head(subset_data$Age,1) 
head(subset_data$Meals_At_Home,1)
head(subset_data$Possess_ration_card,1) 
head(subset_data$Education,1)
head(subset_data$foodtotal_q,1)

#Step 6: Visualize and Analyze Diagnostics
# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)





