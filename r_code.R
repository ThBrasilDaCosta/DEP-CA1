library(readr)
library(dplyr)
library(ggplot2)

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

#creates subsets for each year and grouping them back.
crimes_by_year <- CrimesinBoston %>%
  group_by(YEAR) %>%
  summarize(Total_Crimes = n())


#creates a normal bar plot to visualize the crimes committed by year.
ggplot(crimes_by_year, aes(x = factor(YEAR), y = Total_Crimes)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Total Crimes in Boston (2015-2018",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal()