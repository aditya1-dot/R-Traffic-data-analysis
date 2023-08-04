# Install and load required libraries
suppressMessages(install.packages("ggplot2"))
suppressMessages(install.packages("tidyverse"))
suppressMessages(install.packages("caret"))
suppressMessages(install.packages("randomForest"))

suppressMessages(library(ggplot2))
#supressMessages(library(conflicted))
suppressMessages(library(tidyverse))
suppressMessages(library(caret))
suppressMessages(library(randomForest))

# conflict_prefer("filter", "dplyr") 
# # Choose dplyr's filter() over stats' filter()
# conflict_prefer("lag", "dplyr")
# # Create a data frame with the given data

data <- data.frame(
  Years = c(1970, 1980, 1990, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
            2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
            2015, 2016, 2017),
  Total_Accidents = c(114100, 153200, 282600, 325864, 351999, 371204, 373671, 385018,
                      386456, 391449, 405637, 407497, 406726, 429910, 439255, 460920,
                      479216, 484704, 486384, 499628, 497686, 490383, 486476, 489400,
                      501423, 480652, 464910),
  Total_Killed = c(14500, 24000, 54100, 64463, 70781, 74665, 76977, 79919, 81966,
                   78911, 80888, 84674, 85998, 92618, 94968, 105749, 114444, 119860,
                   125660, 134513, 142485, 138258, 137572, 139671, 146133, 150785,
                   147913),
  Total_Injured = c(70100, 109100, 244100, 311500, 323200, 369502, 378361, 390674,
                    375051, 399265, 405216, 408711, 435122, 464521, 465282, 496481,
                    513340, 523193, 515458, 527512, 511394, 509667, 494893, 493474,
                    500279, 494624, 470975),
  Population_Thousands = c(539000, 673000, 835000, 904000, 924359, 941579, 959792,
                           978081, 996130, 1014825, 1028610, 1045547, 1062388, 1079117,
                           1095722, 1112186, 1128521, 1144734, 1160813, 1176742, 1210193.4,
                           1208116, 1223581, 1238887, 1254019, 1268961, 1283601),
  Total_Vehicles_Thousands = c(1401, 4521, 19152, 27660, 30295, 33786, 37332, 41368,
                               44875, 48857, 54991, 58924, 67007, 72718, 81502, 89618,
                               96707, 105353, 114951, 127746, 141865.6, 159490.6, 181508,
                               190704, 210023, 230031, NA),
  Road_Length_Kms = c(1188728, 1491873, 1983867, 2890950, 2975035, 3202515, 3298788,
                      3228356, 3296650, 3316078, 3373520, 3426603, 3528654, 3621507,
                      3809156, 3880651, 4016401, 4109592, 4471510, 4582439, 4676838,
                      4865394, 5231922, 5402486, 5472144, 5603293, NA)
)
# Cleaning Noise
# Remove rows with missing values
data <- na.omit(data)

# Data visualization
p1 <- ggplot(data, aes(x = Years)) +
  geom_line(aes(y = Total_Accidents, color = "Total Accidents")) +
  geom_line(aes(y = Total_Killed, color = "Total Killed")) +
  geom_line(aes(y = Total_Injured, color = "Total Injured")) +
  labs(x = "Years", y = "Count", title = "Trends in Road Accidents, Deaths, and Injuries") +
  theme_minimal() +
  scale_color_manual(name = "Variables",
                     values = c("Total Accidents" = "blue", "Total Killed" = "red", "Total Injured" = "green"))

p2 <- ggplot(data, aes(x = Years)) +
  geom_line(aes(y = Total_Vehicles_Thousands, color = "Total Vehicles (in Thousands)")) +
  geom_line(aes(y = Population_Thousands, color = "Population (in Thousands)")) +
  labs(x = "Years", y = "Count", title = "Trends in Population and Registered Vehicles") +
  theme_minimal() +
  scale_color_manual(name = "Variables",
                     values = c("Total Vehicles (in Thousands)" = "blue", "Population (in Thousands)" = "red"))

# For the third plot, we'll calculate the rates instead of raw numbers to avoid Inf and NaN values
data <- data %>%
  mutate(Accidents_per_Lakh_Population = Total_Accidents / (Population_Thousands / 100000),
         Accidents_per_10000_Vehicles = Total_Accidents / (Total_Vehicles_Thousands / 10000),
         Accidents_per_10000_Kms_of_Roads = Total_Accidents / (Road_Length_Kms / 10000))

p3 <- ggplot(data, aes(x = Years)) +
  geom_line(aes(y = Accidents_per_Lakh_Population, color = "Accidents per Lakh Population")) +
  geom_line(aes(y = Accidents_per_10000_Vehicles, color = "Accidents per 10,000 Vehicles")) +
  geom_line(aes(y = Accidents_per_10000_Kms_of_Roads, color = "Accidents per 10,000 Kms of Roads")) +
  labs(x = "Years", y = "Rate", title = "Trends in Accidents per Population, Vehicles, and Roads") +
  theme_minimal() +
  scale_color_manual(name = "Variables",
                     values = c("Accidents per Lakh Population" = "blue", "Accidents per 10,000 Vehicles" = "red", "Accidents per 10,000 Kms of Roads" = "green"))

# Print the plots
plot(data$Total_Accidents,data$Total_Killed)
print(p1)
print(p2)
print(p3)

#Spliting the data in x and y

median_killed <- median(data$Total_Killed)
data$Severity <- factor(ifelse(data$Total_Killed >= median_killed, 1, 0), levels = c(1, 0))

#x stands for the features and y is target column
x<-select(data,-Total_Killed,-Severity)
y<-select(data,Severity)
# now spiliting the data
set.seed(9)
train_index <- sample(nrow(data), 0.1 * nrow(data))
#print(train_index)
x_train_data <- x[train_index, ]
x_test_data <- x[-train_index, ]
y_train_data<-y[train_index, ]
y_test_data<-y[-train_index, ]

# print(x)
# print(y)
model<-randomForest(x=x_train_data,y=y_train_data)
# Make predictions on the test set
predictions <- predict(model, newdata = x_test_data)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions,y_test_data)
# Manually compute precision and recall for the "Severe" class
TP <- conf_matrix$table[1, 1]
FP <- sum(conf_matrix$table[, 1]) - TP
FN <- sum(conf_matrix$table[1, ]) - TP

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

# Print the evaluation metrics
accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Precision:", round(precision * 100, 2), "%\n")
cat("Recall:", round(recall * 100, 2), "%\n")


