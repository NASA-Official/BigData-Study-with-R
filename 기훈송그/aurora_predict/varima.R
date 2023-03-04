install.packages("vars", repos = "http://cran.us.r-project.org")
library(vars)


# Create a multivariate time series data set
data <- ts(matrix(rnorm(1000), ncol = 2), start = c(2022, 1), frequency = 12)

View(data)

# Create a lagged matrix of the multivariate time series using the embed() function
lags <- embed(data, 3)

View(lags)

# Fit a VAR model to the lagged matrix using the VAR() function
model <- VAR(lags, p = 2, type = "const")

# Print the summary of the VAR model
summary(model)
