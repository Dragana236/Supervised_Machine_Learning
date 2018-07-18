library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(rms)


# Load first clv dataset
clv <- read_csv('clvData1.csv')

# Inspect clv dataset
head(clv)

# Inspect clv dataset with dplyr's glimpse function
glimpse(clv)


# First select only numeric values except customerID variable
clv1 <- clv %>% 
  select_if(is.numeric) %>%
  select(-customerID)

# Visualization of correlations
clv1 %>% 
  cor() %>% 
  corrplot()

# Fit simple linear regression model
simpleLM <- lm(futureMargin ~ margin, data = clv1)

# Call summary on the model
summary(simpleLM)

# Visualize regression model
ggplot(clv1, aes(margin, futureMargin)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Margin year 1") +
  ylab("Margin year 2")

# Fit mulitple regression model
multipleLM <- lm(futureMargin ~ margin + nOrders + nItems + daysSinceLastOrder +
                   returnRatio + shareOwnBrand + shareVoucher + shareSale +
                   gender + age + marginPerOrder + marginPerItem +
                   itemsPerOrder, data = clv)

# Looking at model summary
summary(multipleLM)

# Variance Inflation Factors
vif(multipleLM)

# New model
multipleLM2 <- lm(futureMargin ~ margin + nOrders +
                    daysSinceLastOrder + returnRatio + shareOwnBrand +
                    shareVoucher + shareSale + gender + age +
                    marginPerItem + itemsPerOrder,
                  data = clv)

# Look at Variance Inflation Factors again
vif(multipleLM2)

# Interprete coefficients
summary(multipleLM2)

# Avoid overfitting
AIC(multipleLM2)

# Load second dataset
clv2 <- read_csv('clvData2.csv')

# Take a look at it
glimpse(clv2)

# Predict futureMargin for year two using Margins from year two
predMargin <- predict(multipleLM2,
                      newdata = clv2)
# Calculate the mean of predicted futureMargin values
mean(predMargin, na.rm = TRUE)

