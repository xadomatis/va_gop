# Load Packages -----------------------------------------------------------

library(tidyverse)
library(sf)

# Load Data ---------------------------------------------------------------

prez <- 
  read_csv("data/processed/va_prez_2016.csv") %>% mutate(pcode = as.character(pcode))

sen <- 
  read_csv("data/processed/va_sen_2018.csv") %>% mutate(pcode = as.character(pcode))

gov <- 
  read_csv("data/processed/va_gov_2017.csv") %>% mutate(pcode = as.character(pcode))

cd2_22 <- 
  read_csv("data/processed/va_cd2_2022.csv") %>% mutate(pcode = as.character(pcode))

cd7_22 <- 
  read_csv("data/processed/va_cd7_2022.csv") %>% mutate(pcode = as.character(pcode))

cd10_18 <- 
  read_csv("data/processed/va_cd10_2018.csv") %>% mutate(pcode = as.character(pcode))

# Regress Statewide Data --------------------------------------------------

merge <- inner_join(prez, df, on = "pcode")
ggplot(merge, aes_string(x=names(df)[5], y=names(df)[9])) +
  geom_point()

plot_corr <- 
  function(df,string) {
    merge <- inner_join(prez, df, on = "pcode")
    lm <- lm(as.formula(paste(names(merge)[5], "~", paste(names(merge)[9], collapse = "+"), sep = "")), data = merge)
    subtitle <- paste("R-squared:", round(summary(lm)$r.squared, 2), sep = " ")
    title <- paste("Correlation Plot, 2016 Presidential",string,"Election")
    plot <- ggplot(merge, aes_string(x=names(merge)[5], y=names(merge)[9])) + geom_point() + labs(title = title, subtitle = subtitle, x = "2016 Presidential Voteshare", y = paste(string, "Voteshare")) + geom_smooth(method='lm', formula= y~x) + theme_classic()
    return(plot)
  }

plot_corr(sen,"2018 Senate")
