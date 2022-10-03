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
  
  # Lowercase the county names for later joins
  
  mutate(county_name = str_to_lower(county_name))

# Create a function specirically to strip candidates to just their last name

last_name <- 
  function(string) {
    last_name = gsub(".*_","",string)
    return(last_name)
  }

# Read in Presidential Data:

prez_tidy <- 
  read_csv("poc/data/raw/va_2016_prez_county.csv") %>% 
  
  # Fix columns
  
  fix_cols %>% 
  
  # Add county_name to match FIPS data
  
  mutate(county_name = str_to_lower(gsub(' COUNTY','', county))) %>% 
  
  # Join with fips

  inner_join(
    .,
    fips,
    by = "county_name") %>% 
  
  # Sum all withdrawn candidates
  
  mutate(others = rowSums(select(., contains("withdrawn")))) %>% 
  
  # Remove superfluous columns
  
  select(!c(contains("withdrawn_"),contains("county"),"state","_1")) %>%
  
  # Simplify candidate columns names
  
  rename_with(last_name) %>% 
  
  # Reorder column names with FIPS leading
  select(order(colnames(.))) %>% 
  select(fips, everything(), others) 


# Tidy up Gubernatorial data ----------------------------------------------

# Read in Gubernatorial data

gov_tidy <-
  read_csv("poc/data/raw/va_2017_gov_county.csv") %>% 
  
  # Fix columns
  
  fix_cols %>% 
  
  # Add county_name to match FIPS data
  
  mutate(county_name = str_to_lower(gsub(' County','', county_city))) %>% 
  
  # Join with fips
  
  inner_join(
    .,
    fips,
    by = "county_name") %>%
  
  # Remove superfluous columns
  
  select(!c("state","county_name","county_city","total_votes_cast","_2","_3")) %>% 
  
  # Simplify candidate columns names
  
  rename_with(last_name) %>% 
  
  # Reorder columns with FIPS leading
  
  select(order(colnames(.))) %>% 
  select(fips, everything(), others) 


# Output ------------------------------------------------------------------

write.csv(prez_tidy,"poc/data/processed/2016_prez.csv", row.names = FALSE)
write.csv(gov_tidy,"poc/data/processed/2017_gov.csv", row.names = FALSE)


# Tableau prep (untidy) ---------------------------------------------------

prez_tab <- prez_tidy %>% 
  mutate(total = rowSums((prez_tidy[,2:11])), 
         lead_pct = trump/total*100,
         lead_scale = scale(lead_pct)) %>% 
  select(fips,lead_pct,lead_scale)

gov_tab <- gov_tidy %>% 
  mutate(total = rowSums((gov_tidy[,2:5])), 
         lead_pct = gillespie/total*100,
         lead_scale = scale(lead_pct)) %>% 
  select(fips,lead_pct,lead_scale)

prez_gov_combo <- 
  inner_join(
    prez_tab,
    gov_tab,
    by = "fips",
    suffix = c("_prez","_gov")) %>% 
  left_join(
    .,
    fips,
    on = "fips"
  ) %>% 
  mutate(county_name =  str_to_title(county_name)) %>% 
  select(-state)

prez_gov_stack <- 
  bind_rows(prez_tab,gov_tab,.id = "year") %>% 
  mutate(year = ifelse(year == 1, 2016, 2017)) %>% 
  left_join(
    .,
    fips,
    on = "fips"
  ) %>% 
  mutate(county_name =  str_to_title(county_name)) %>% 
  select(-state) %>% 
  filter(fips != 51097) %>% 
  mutate(scale_adjust = ifelse(year == 2017, -lead_scale, lead_scale))


# Regress -----------------------------------------------------------------

# Create and view regression results

scale.lm <- lm(lead_scale_gov ~ lead_scale_prez, data = prez_gov_combo)
summary(scale.lm)
pct.lm <- lm(lead_pct_gov ~ lead_pct_prez, data = prez_gov_combo)
summary(pct.lm)

# Plot residuals

plot(pct.lm$residuals, pch = 16, col = "red")

prez_gov_combo$pct_residuals = pct.lm$residuals
prez_gov_combo$scale_residuals = scale.lm$residuals

prez_gov_stack$scale_residuals = c(scale.lm$residuals, scale.lm$residuals)
prez_gov_stack$pct_residuals = c(pct.lm$residuals, pct.lm$residuals)

library(ggplot2)

ggplot(prez_gov_combo, aes(x=lead_pct_prez, y=lead_pct_gov)) +
  geom_point()



# Write for Tableau -------------------------------------------------------

write.csv(prez_gov_combo,"poc/data/tableau/prez_gov_combo.csv", row.names = FALSE)
write.csv(prez_gov_stack,"poc/data/tableau/prez_gov_stack.csv", row.names = FALSE)
write.csv(prez_tab,"poc/data/tableau/2016_prez.csv", row.names = FALSE)
write.csv(gov_tab,"poc/data/tableau/2017_gov.csv", row.names = FALSE)


# Scrap -------------------------------------------------------------------
