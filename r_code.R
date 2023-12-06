library(readr)
library(dplyr)
library(ggplot2)
library(MASS)

#reading csv file from big data set, Size 319073, 17.
CrimesinBoston <- read_csv("CrimesinBoston.csv")
#Dropping unnecessary columns.
CrimesinBoston <- subset(CrimesinBoston, select = -c(INCIDENT_NUMBER, OFFENSE_CODE, REPORTING_AREA, SHOOTING, UCR_PART ))
View(CrimesinBoston)#Size 319073, 12

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
  labs(title = "Total Crimes in Boston (2015-2018) - Normalized",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

# Print the original and normalized datasets
print("Original Data:")
print(crimes_by_year)

print("Normalized Data:")
print(totalCrime_norm )

# Apply Min-Max normalization to the dataset crimes by each district.

districtCrime_norm <- min_max_normalization(crimes_by_district$Total_Crimes)

ggplot(crimes_by_district, aes(x = factor(DISTRICT), y = districtCrime_norm)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Total Crimes in Boston by District - Normalized",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()

# Apply Min-Max normalization to the dataset crimes by each district.

weekdayCrime_norm <- min_max_normalization(crimes_by_weekday$Total_Crimes)

ggplot(crimes_by_weekday, aes(x = factor(DAY_OF_WEEK, levels = weekday_order), y = weekdayCrime_norm)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "Total Crimes in Boston on Weekdays (2015-2018) - Normalized",
       x = "Weekdays",
       y = "Total Crimes") +
  theme_minimal()

# Z-score Standardization function.
normalizeStandardized <- function(x) {
  return((x - mean(x)) / sd(x))
}
# Apply Z-Score standardization to the dataset crimes by each year.
totalCrime_std <- normalizeStandardized(crimes_by_year$Total_Crimes)

print("Original Data:")
print(crimes_by_year)

print("Standardized Data:")
print(totalCrime_std)

# Robust Scaling function
robust_scaling <- function(x) {
  (x - median(x)) / IQR(x)
}
# Apply Robust Scale to the dataset crimes by each year.
robust_scaled_data <- as.data.frame(lapply(crimes_by_year, robust_scaling))
print("Robust Scaled Data:")
print(robust_scaled_data)
