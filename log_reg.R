library(data.table)
library(ggplot2)
library(dplyr)
data <- read_csv("restaurant_customer_satisfaction.csv")
data  <- data.table(data)
data <- data[,-1]
str(data)
sum(is.na(data))
summary(data)

#categorical columns 
data$Gender = factor(data$Gender,
                         levels = c('Male', 'Female'),
                         labels = c('Male', 'Female'))

unique(data$VisitFrequency)

data$VisitFrequency = factor(data$VisitFrequency,
                           levels = c("Daily","Weekly",  "Monthly", "Rarely" ),
                           labels = c("Daily","Weekly",  "Monthly", "Rarely" ))

unique(data$PreferredCuisine)

data$PreferredCuisine = factor(data$PreferredCuisine,
                             levels = c("Chinese","American", "Indian","Mexican","Italian"  ),
                             labels = c("Chinese","American", "Indian","Mexican","Italian"  ))


unique(data$TimeOfVisit)

data$TimeOfVisit = factor(data$TimeOfVisit,
                               levels = c("Breakfast",  "Lunch"  , "Dinner"),
                               labels = c("Breakfast",  "Lunch"  , "Dinner"))

unique(data$DiningOccasion)

data$DiningOccasion = factor(data$DiningOccasion,
                          levels = c("Business" , "Casual" ,"Celebration"),
                          labels = c("Business" , "Casual" ,"Celebration"))

unique(data$MealType)

data$MealType = factor(data$MealType,
                             levels = c("Takeaway" , "Dine-in" ),
                             labels = c("Takeaway" , "Dine-in" ))
data$OnlineReservation = factor(data$OnlineReservation)

data$DeliveryOrder = factor(data$DeliveryOrder)

data$LoyaltyProgramMember = factor(data$LoyaltyProgramMember)

split = sample.split(data$HighSatisfaction, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Feature Scaling of numerical variales

tmp = scale(training_set[, c(1,3,5,8,14)])
training_set <- cbind(tmp,training_set[, -c(1,3,5,8,14)])
tmp = scale(test_set[, c(1,3,5,8,14)])
test_set <- cbind(tmp,test_set[, -c(1,3,5,8,14)])

#Fitting Logistic regression to the training set
classifier = glm(formula = HighSatisfaction ~ ., 
                 family = binomial,
                 data = training_set)

#Predicting the test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[,-18])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#making the confusion matrix (to evaluate the predictions)
cm = table(test_set$HighSatisfaction, y_pred)
cm

# Extract summary of the model
summary_classifier <- summary(classifier)
coefficients <- summary_classifier$coefficients

# Plotting
plot_data <- data.frame(
  term = rownames(coefficients),
  estimate = coefficients[, "Estimate"],
  std.error = coefficients[, "Std. Error"],
  p.value = coefficients[, "Pr(>|z|)"]
)

plot_data <- plot_data %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significance = ifelse(p.value < 0.05, "s", "ns")
  )

# Plot the coefficients with error bars and colored points
ggplot(plot_data, aes(x = reorder(term, estimate), y = estimate, color = significance)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_color_manual(values = c("s" = "red", "ns" = "black")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Results",
       x = "Terms",
       y = "Estimate",
       color = "Significance") +theme_bw()+
  theme(axis.text.x = element_text( hjust = 1))









