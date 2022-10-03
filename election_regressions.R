# Load Packages -----------------------------------------------------------

library(tidyverse)
library(sf)

# Load Data ---------------------------------------------------------------

prez <- 
  read_csv("data/processed/va_prez_2016.csv")

sen <- 
  read_csv("data/processed/va_sen_2018.csv")

gov <- 
  read_csv("data/processed/va_gov_2017.csv")

cd2_22 <- 
  read_csv("data/processed/va_cd2_2022.csv")


# Regress Statewide Data --------------------------------------------------

prez_gov <-
  inner_join(prez, gov, on = "pcode")
gov.lm <- lm(winner_percent_2016 ~ winner_percent_2017, data = prez_gov)
summary(gov.lm)
plot(gov.lm$residuals, pch = 16, col = "red")
ggplot(prez_gov, aes(x=winner_percent_2016, y=winner_percent_2017)) +
  geom_point()

prez_sen <-
  inner_join(prez, sen, on = "pcode")
sen.lm <- lm(winner_percent_2016 ~ winner_percent_2018, data = prez_sen)
summary(sen.lm)
plot(sen.lm$residuals, pch = 16, col = "red")
ggplot(prez_sen, aes(x=winner_percent_2016, y=winner_percent_2018)) +
  geom_point()
