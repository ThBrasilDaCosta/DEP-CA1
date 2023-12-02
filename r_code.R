library(readr)
#reading csv file from big data set
CrimesinBoston <- read_csv("CrimesinBoston.csv")
View(CrimesinBoston)

#creating subset of crimes in 2017
crimes_2017 <- subset(CrimesinBoston, YEAR == 2017)
View(crimes_2017)

#dropping unnecessary columns  from the subset.
crimes_2017 <- subset(crimes_2017, select = -c(INCIDENT_NUMBER, OFFENSE_CODE, REPORTING_AREA, SHOOTING, UCR_PART ))
View(crimes_2017) 
#changed subset from 2018 to 2017