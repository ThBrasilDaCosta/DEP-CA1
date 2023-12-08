library(readr)
library(dplyr)
library(ggplot2)
library(MASS)
install.packages("reshape2")
library(reshape2)
install.packages("GGally")
library(GGally)

#reading csv file from big data set, Size 319073, 17.
CrimesinBoston <- read_csv("CrimesinBoston.csv")
#Dropping unnecessary columns.
CrimesinBoston <- subset(CrimesinBoston, select = -c(INCIDENT_NUMBER, OFFENSE_CODE, REPORTING_AREA, UCR_PART ))
View(CrimesinBoston)#Size 319073, 13

#creating subset of crimes 2015, 2016, 2017.
crimes_2015 <- subset(CrimesinBoston, YEAR == 2015)
crimes_2016 <- subset(CrimesinBoston, YEAR == 2016)
crimes_2017 <- subset(CrimesinBoston, YEAR == 2017)
crimes_2018 <- subset(CrimesinBoston, YEAR == 2018)
#using the number of rows in each subset to determine the total number of crimes in each year.
total_crimes15 <- nrow(crimes_2015)
total_crimes16 <- nrow(crimes_2016)
total_crimes17 <- nrow(crimes_2017)
total_crimes18 <- nrow(crimes_2018)
total_crimesboston <- nrow(CrimesinBoston)
#calling the variables created to check and compare the numbers with plots to come.
show(total_crimes15)
show(total_crimes16)
show(total_crimes17)
show(total_crimes18)
show(total_crimes15 + total_crimes16 + total_crimes17 + total_crimes18)
show(total_crimesboston)

#TASK a.
#creates subsets for each year and grouping them back.
crimes_by_year <- CrimesinBoston %>%
  group_by(YEAR) %>%
  summarize(Total_Crimes = n())
show(crimes_by_year)
#creates a normal bar plot to visualize the crimes committed by year.
ggplot(crimes_by_year, aes(x = factor(YEAR), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Total Crimes in Boston (2015-2018)",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

#creates subsets for each weekday and grouping them back.
crimes_by_weekday <- CrimesinBoston %>%
  group_by(DAY_OF_WEEK) %>%
  summarize(Total_Crimes = n())
#defines an order for the weekdays to be displayed.
weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
#creates a normal bar plot to visualize the crimes committed by each day of the week along 2015-2018.
ggplot(crimes_by_weekday, aes(x = factor(DAY_OF_WEEK, levels = weekday_order), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "Total Crimes in Boston on Weekdays (2015-2018)",
       x = "Weekdays",
       y = "Total Crimes") +
  theme_minimal()

#creates subsets for each district and grouping them back.
crimes_by_district <- CrimesinBoston %>%
  group_by(DISTRICT) %>%
  summarize(Total_Crimes = n())
#creates a normal bar plot to visualize the crimes committed by district along 2015-2018.
ggplot(crimes_by_district, aes(x = factor(DISTRICT), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Total Crimes in Boston by District (2015-2018)",
       x = "Districts",
       y = "Total Crimes") +
  theme_minimal()

#TASK b.

#creates the mean for Crimes in Boston from 2015 to 2018.

crimes <- c(total_crimes15,total_crimes16,total_crimes17,total_crimes18)
crime_mean <- mean(crimes)
print(crime_mean)

ggplot(crimes_by_year, aes(x = YEAR, y = Total_Crimes)) +
  geom_line(lwd = 1.5, color= "brown") +
  geom_hline(yintercept = crime_mean, linetype = "dashed", lwd = 2, color = "blue") +
  labs(title = "Total Crimes in Boston by Year",
       x = "Year",
       y = "Total Crimes",
       caption = paste("Mean Crimes per Year: ", round(crime_mean, 2))) +
  theme_minimal()

#creates the median for Crimes in Boston from 2015 to 2018.

crime_median <- median(crimes)
print(crime_median)

ggplot(crimes_by_year, aes(x = YEAR, y = Total_Crimes)) +
  geom_line(lwd = 1.5, color = "green") +
  geom_hline(yintercept = crime_median, linetype = 4,lwd = 2, color = "purple") +
  labs(title = "Total Crimes in Boston by Year",
       x = "Year",
       y = "Total Crimes",
       caption = paste("Median Crimes per Year: ", round(crime_median, 2))) +
  theme_minimal()

#creates the Standard Deviation for Crimes in Boston from 2015 to 2018.
crime_std <- sd(crimes)
print(crime_std)



#TASK c.

#Min-Max Normalization function
min_max_normalization <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max normalization to the dataset crimes by each year.

totalCrime_norm <- min_max_normalization(crimes_by_year$Total_Crimes)

ggplot(crimes_by_year, aes(x = factor(YEAR), y = totalCrime_norm)) +
  geom_bar(stat = "identity", fill = "brown", color = "black") +
  labs(title = "Total Crimes in Boston (2015-2018) - Min-Max Normalized",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

print("Normalized Data:")
print(totalCrime_norm )

# Apply Min-Max normalization to the dataset crimes by each district.

districtCrime_norm <- min_max_normalization(crimes_by_district$Total_Crimes)

ggplot(crimes_by_district, aes(x = factor(DISTRICT), y = districtCrime_norm)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Total Crimes in Boston by District - Min-MAx Normalized",
       x = "District",
       y = "Total Crimes") +
  theme_minimal()

print("Normalized Data:")
print(districtCrime_norm )

# Apply Min-Max normalization to the dataset crimes by week.

weekdayCrime_norm <- min_max_normalization(crimes_by_weekday$Total_Crimes)

ggplot(crimes_by_weekday, aes(x = factor(DAY_OF_WEEK, levels = weekday_order), y = weekdayCrime_norm)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "Total Crimes in Boston on Weekdays (2015-2018) - Min-Max Normalized",
       x = "Weekdays",
       y = "Total Crimes") +
  theme_minimal()

print("Normalized Data:")
print(weekdayCrime_norm )

# Z-score Standardization function.
normalizeStandardized <- function(x) {
  return((x - mean(x)) / sd(x))
}
# Apply Z-Score standardization to the dataset crimes by each year.
totalCrime_std <- normalizeStandardized(crimes_by_year$Total_Crimes)

ggplot(crimes_by_year, aes(x = factor(YEAR), y = totalCrime_std)) +
  geom_bar(stat = "identity", fill = "brown", color = "black") +
  labs(title = "Total Crimes in Boston by District - Z-Score Std",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

print("Standardized Data:")
print(totalCrime_std)

# Apply Z-Score standardization to the dataset crimes by district.

districtCrime_std <- normalizeStandardized(crimes_by_district$Total_Crimes)

ggplot(crimes_by_district, aes(x = factor(DISTRICT), y = districtCrime_std)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Total Crimes in Boston (2015-2018) - Z-Score Std",
       x = "District",
       y = "Total Crimes") +
  theme_minimal()

print("Standardized Data:")
print(districtCrime_std)

# Apply Z-Score standardization to the dataset crimes by week.

weekdayCrime_std <- normalizeStandardized(crimes_by_weekday$Total_Crimes)

ggplot(crimes_by_weekday, aes(x = factor(DAY_OF_WEEK, levels = weekday_order), y = weekdayCrime_norm)) +
  geom_bar(stat = "identity", fill = "lightyellow", color = "black") +
  labs(title = "Total Crimes in Boston on Weekdays (2015-2018) - Z-Score Std",
       x = "Weekdays",
       y = "Total Crimes") +
  theme_minimal()

print("Normalized Data:")
print(weekdayCrime_std )

print("Standardized Data:")
print(districtCrime_std)


# Robust Scaling function
robust_scaling <- function(x) {
  (x - median(x)) / IQR(x)
}
# Apply Robust Scale to the dataset crimes by each year.
robust_scaled_year <- robust_scaling(crimes_by_year$Total_Crimes)

ggplot(crimes_by_year, aes(x = factor(YEAR), y = robust_scaled_year)) +
  geom_bar(stat = "identity", fill = "gold", color = "black") +
  labs(title = "Total Crimes in Boston (2015-2018) - Robust Scale",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

print("Robust Scaled Data:")
print(robust_scaled_year)

# Apply Robust Scale to the dataset crimes by district.

robust_scaled_district <- robust_scaling(crimes_by_district$Total_Crimes)

ggplot(crimes_by_district, aes(x = factor(DISTRICT), y = robust_scaled_district)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "black") +
  labs(title = "Total Crimes in Boston by District - Robust Scale",
       x = "District",
       y = "Total Crimes") +
  theme_minimal()

print("Robust Scaled Data:")
print(robust_scaled_district)


# Apply Robust Scale to the dataset crimes by weekday.

robust_scaled_weekday <- robust_scaling(crimes_by_weekday$Total_Crimes)

ggplot(crimes_by_weekday, aes(x = factor(DAY_OF_WEEK, levels = weekday_order), y = robust_scaled_weekday)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Total Crimes in Boston by Weekdays - Robust Scale",
       x = "Weekdays",
       y = "Total Crimes") +
  theme_minimal()

print("Robust Scaled Data:")
print(robust_scaled_weekday)

# Task d.
columns <- c("HOUR", "Lat", "Long", "MONTH")

#correlation
cor_matrix <- cor(CrimesinBoston[, columns], use = "complete.obs")
print(cor_matrix)

# correlation matrix
melted_cor_matrix <- as.data.frame(as.table(cor_matrix))
names(melted_cor_matrix) <- c("OFFENSE_CODE_GROUP", "YEAR", "Correlation")
str(melted_cor_matrix)

# correlation heatmap
heatmap_cor_plot <- ggplot(melted_cor_matrix, aes(OFFENSE_CODE_GROUP, YEAR, fill = Correlation)) +
  geom_tile(color = "gold") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")
print(heatmap_cor_plot)

#scattered subset
#subset_data <- na.omit(CrimesinBoston[, columns])
#ggpairs(subset_data, title = "Scatterplot Matrix")

# Task f.

#Dummy encode
install.packages("caret")
install.packages("lattice")
library(caret)
#dataset seleccted.
crimesBoston_dummy <- CrimesinBoston
#picked columns to apply the dummy encoding
columns_to_dummy <- c("DISTRICT", "DAY_OF_WEEK")
#dummy variables generator.
dummy_data <- dummyVars("~.", data = crimesBoston_dummy[columns_to_dummy])
#apply changes to the original dataset.
crime_data_dummies <- as.data.frame(predict(dummy_data, newdata = crimesBoston_dummy[columns_to_dummy]))
#bind the original dataset with generated dummy variables.
crimesBoston_dummy <- cbind(crimesBoston_dummy, crime_data_dummies)
#if necessary remove original categorical columns.
crimesBoston_dummy <- crimesBoston_dummy[, !(names(crimesBoston_dummy) %in% columns_to_dummy)]

show(crimesBoston_dummy)
#View(crimesBoston_dummy)

# git
#https://github.com/ThBrasilDaCosta/DEP-CA1