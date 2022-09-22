# setup -------------------------------------------------------------------

# Load in relevant libraries

library(tidyverse)

# Create a function to make column names easier to use

fix_cols <- 
  function(df) {
    names(df) <- tolower(names(df))
    names(df) <- gsub('[[:punct:] ]+',' ',names(df))
    names(df) <- gsub(" ", "_", names(df))
    return(df)
  }


# Load Data ---------------------------------------------------------------

# Read in 2022 Congressional District 2 Data

cd <- 
  read_csv("data/raw/va_cd2_2022.csv") %>%
  
  # Fix columns
  
  fix_cols

# Read in 2016 Presidential Election Data

prez <- 
  read_csv("data/raw/va_prez_2016.csv") %>% 
  
  # Fix columns
  
  fix_cols

# Read in fips codes

fips <-
  read_csv("data/raw/US_FIPS_Codes.csv", skip=1)  %>% 
  
  # Limit dataset to Virginia for simplicity
  
  filter(State == "Virginia") %>% 
  
  # Make single fips id columns
  
  unite("fips", `FIPS State`:`FIPS County`, sep="") %>% 
  
  fix_cols


# Create Tidy Data --------------------------------------------------------

# Strip " County" suffix string for merge

prez$county_city <- gsub(' County','', prez$county_city)


# Filler ------------------------------------------------------------------

sort(unique(prez$Pct))

sort(unique(cd$id_string))

