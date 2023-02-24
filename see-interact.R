# Sort and plot survey data.  

library(readr)
library(dplyr)
library(tidyr)

x <- read_csv("frc.csv", skip = 1)

y <- x %>%
     select(`How often do you see free-roaming cats in your neighborhood?`, `How often do you intentionally interact with free-roaming cats?`) %>%
     rename(see=`How often do you see free-roaming cats in your neighborhood?`, interact=`How often do you intentionally interact with free-roaming cats?`)

see <- array(NA, dim = nrow(y))
for (i in 1:nrow(y)) {
     if (is.na(y$see[i])) {
          see[i] <- NA
     } else {
          if (y$see[i] == "At least once a day") {
               see[i] <- 30 # times per month
          } else if (y$see[i] == "At least once a week") {
               see[i] <- 4 # times per month
          } else if (y$see[i] == "At least once a month") {
               see[i] <- 1 # times per month
          } else if (y$see[i] == "Rarely (once every 2-3 months)") {
               see[i] <- 0.333 # times per month
          } else if (y$see[i] == "I never see free-roaming cats in my neighborhood.") {
               see[i] <- 0 # times per month
          } else {
               see[i] <- NA
          }
     }
}
interact <- array(NA, dim = nrow(y))
for (i in 1:nrow(y)) {
     if (is.na(y$interact[i])) {
          interact[i] <- NA
     } else {
          if (y$interact[i] == "At least once a day") {
               interact[i] <- 30 # times per month
          } else if (y$interact[i] == "At least once a week") {
               interact[i] <- 4 # times per month
          } else if ( (y$interact[i] == "At least once a month") | (y$interact[i] == "At lease once a month") ) {
               interact[i] <- 1 # times per month
          } else if (y$interact[i] == "Rarely (once every 2-3 months)") {
               interact[i] <- 0.333 # times per month
          } else if (y$interact[i] == "I never interact with free-roaming cats in my neighborhood.") {
               interact[i] <- 0 # times per month
          } else {
               interact[i] <- NA
          }
     }
}

