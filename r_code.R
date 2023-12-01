library(readr)
#reading csv file from big data set
CrimesinBoston <- read_csv("CrimesinBoston.csv")
View(CrimesinBoston)

#creating subset of crimes in 2018
crimes_2018 <- subset(CrimesinBoston, YEAR == 2018)
View(crimes_2018)