# setup -------------------------------------------------------------------

# Load in relevant libraries

library(tidyverse)

# Create a function to make column names easier to use

fix_cols <- 
  function(df) {
    names(df) <- tolower(names(df))
    names(df) <- gsub('[[:punct:] ]+',' ',names(df))
    names(df) <- gsub(" ", "_", names(df))
    names(df) <- gsub("_republican", "", names(df))
    return(df)
  }



# Tidy up Presidential Data -----------------------------------------------

# Read in fips codes:

fips <-
  read_csv("data/raw/US_FIPS_Codes.csv", skip=1)  %>% 
  
  # Limit dataset to Virginia for simplicity
  
  filter(State == "Virginia") %>% 
  
  # Make single fips id columns
  
  unite("fips", `FIPS State`:`FIPS County`, sep="") %>% 
  
  # Fix column names
  
  fix_cols %>% 
  
  # 
  
  mutate(county_name = str_to_lower(county_name))

# Read in 2016 Presidential Election Data

prez <- 
  read_csv("data/raw/va_prez_2016.csv") %>% 
  
  # Fix columns
  
  fix_cols %>% 
  
  # Add county_name to match FIPS data
  
  mutate(county_name = str_to_lower(gsub(' County','', county_city)))

# Merge prez with FIPS
prez_tidy <- 
  inner_join(
    prez,
    fips,
    by = "county_name") %>% 
  
  # Drop untidy columns
  
  select(-c(county_city,ward,total_votes_cast,state))

  # leaving county name is technically not tidy but it helps later


# Tidy up CD2 data --------------------------------------------------------


# Read in 2022 Congressional District 2 Data:

cd2 <- 
  read_csv("data/raw/va_cd2_2022.csv") %>%
  
  # Fix columns
  
  fix_cols %>% 
  
  # Drop the _
  
  mutate(county_name = gsub('_',' ', county)) %>% 
  
  mutate(county_name = str_to_lower(gsub(' COUNTY','', county_name)))

cd2_tidy <-
  inner_join(
    cd2,
    fips,
    by = "county_name") %>% 
  
  select(-c(county,`_1`,county_name,state,precinct_))

# Output to CSV -----------------------------------------------------------

write.csv(prez_tidy,"data/processed/2016_prez.csv")
write.csv(cd2_tidy,"data/processed/2022_cd2.csv")
write.csv(fips,"data/processed/fips.csv")

# Filler ------------------------------------------------------------------

sort(unique(prez$ward))

sort(unique(cd2$id_string))

