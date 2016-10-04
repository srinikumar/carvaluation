library(ggplot2)
library(stringr)
library(party)
# Setting the directory - set it to  yours
#setwd("<Location of where your files are. Make sure that the script and data files are in the same directory>")

# Note: be careful if you use Excel to read and save as a tab delimited text file. Ensure that 
# MS Excel does not add empty fields to each record.
pol <- read.csv("cardata.txt", header = TRUE, sep = "\t") # Now, read the data
pol$state <- str_sub(pol$location, -2, -1) # Extract the state, in case it comes in useful
pol$state <- as.factor(pol$state)          # Make the state a factor, to regress on it
pol$location <- NULL
# The exploratory plot
ggplot(data=pol, aes(mileage, price)) + geom_point(aes(color=state)) +
  geom_smooth()

# Another exploratory plot, to see if there is a correlation between the mileage and the state
ggplot(data=pol, aes(mileage, state)) + geom_point(aes(size=price))
# Evidently not.

# Now, a linear regression model
mileageStateLinearModel <- glm(data=pol, formula = price ~ mileage + state)

# Prediction from the linear model
myCar <- data.frame(45000, 0, "CA") # 45K miles was my closest guess
names(myCar) <- names(pol)
levels(myCar) <- levels(pol)
predict(mileageStateLinearModel, newdata = myCar)
# Predicted value = 23122.47

# Now, a decision tree model since we want to see if the states determined the price
mileageStateDecisionTreeModel <- ctree(data=pol, formula = price ~ mileage + state)
plot(mileageStateDecisionTreeModel)
# Apparently the state is not as important as the mileage

# Alternate Linear model, since the decision tree illustrated dependence only on mileage, 
# though it may not have caught it the way the linear regression seemingly did.
mileageStateLinearModel <- glm(data=pol, formula = price ~ mileage)

# Prediction from the alternate linear model
myCar <- data.frame(45000, 0, "CA")
names(myCar) <- names(pol)
levels(myCar) <- levels(pol)
predict(mileageStateLinearModel, newdata = myCar)
 # Predicted value = 23969.09
