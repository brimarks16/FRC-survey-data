# Sort and plot survey data.  

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

x <- read_csv("frc.csv", skip = 1)

y <- x %>%
     select(`How often do you see free-roaming cats in your neighborhood?`, `How often do you intentionally interact with free-roaming cats?`, `Do you rent or own your home?`) %>%
     rename(see=`How often do you see free-roaming cats in your neighborhood?`, interact=`How often do you intentionally interact with free-roaming cats?`, own=`Do you rent or own your home?`)

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

own <- array(NA, dim = nrow(y))
for (i in 1:nrow(y)) {
     if (is.na(y$own[i])) {
          own[i] <- NA
     } else {
          if (y$own[i] == "Rent") {
               own[i] <- -1 # times per month
          } else if (y$own[i] == "Own/mortgage") {
               own[i] <- 1
          } else {
               own[i] <- NA
          }
     }
}

rm(interact,see,own)
z <- data.frame(see,interact,own)

ggplot(z) +
     geom_violin(aes(factor(see),interact)) +
     xlab(TeX('How often do you see free-roaming cats in your neighborhood? $(month^{-1})$')) +
     ylab(TeX('How often do you intentionally interact with free-roaming cats? $(month^{-1})$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

# It appears that the population that actually plays with FRC is small, but there may be some other factor when we look at the rest of the data.

own <- filter(z, own==1)
rent <- filter(z, own ==-1)

m <- lm(interact ~ see, data = z)
m1 <- lm(interact ~ see, data = own)
m2 <- lm(interact ~ see, data = rent)







