# setup -------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(ggplot2)

# Read in countries data:

countries <-
  read_csv(
    'data/processed/world_bank_countries.csv'
  )

# Read in population data:

world_pop <-
  read_csv(
    'data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_2763937.csv', 
    skip = 3) %>% 
  
  # Reshape year columnns into rows:
  
  pivot_longer(
    names_to = 'year',
    values_to = 'population',
    `1960`:`2020`) %>% 
  
  # Rename columns with a space:
  
  rename(country_code = `Country Code`) %>% 
  
  # Select columns of interest:
  
  select(country_code, year, population)


# Read in co2 data:

co2 <-
  read_csv(
    'data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv', 
    skip = 3) %>% 
  
  pivot_longer(
    names_to = 'year',
    values_to = 'co2',
    `1960`:`2020`) %>% 
  
  # Rename columns with a space:
  
  rename(country_code = `Country Code`) %>% 
  
  # Select requested columns:
  
  select(country_code, year, co2)


# normalize and clean data ------------------------------------------------

population_co2 <- 
  
  # Join the co2 and population data
  
  inner_join(
    world_pop,
    co2,
    by = c("country_code","year")) %>% 
  
  # Turn year into a numeric
  
  mutate(year = as.numeric(year))

# plot population density and co2 emissions in Africa ---------------------

africa_population_co2 <- 
  
  # Join the population CO2 and countries dataset to align land area variables
  inner_join(
    population_co2,
    countries,
    by = "country_code") %>% 
  
  # Reduce dataset to only African regions
  
  filter(country_georegion %in% 
           c("Southern Africa",
             "Western Africa", 
             "Eastern Africa", 
             "Central Africa", 
             "Northern Africa")) %>% 
  
  # Select to desired variables

  select(country_name, land_area, population, co2, year) %>% 
  
  # Create population density and eliminate redundant variables
  
  mutate(population_density = population / land_area, .keep = "unused")

# Plot Africa Population Density

ggplot(africa_population_co2, aes(
  
  # Set X, Y, and color variables
  
  x = year, 
  y = population_density, 
  color = country_name)) +
  
  # Setup lines and points
  
  geom_line() +
  geom_point() +
  
  # Label axes
  
  labs(title="Population Density of Africa (1960-2020)",
       x="Year", y = "Population Density") +
  
  # Make plot backgroung blank
  
  theme(panel.background = element_rect(fill = "white", color = "grey"))

# Create faceted plot

ggplot((
  
  # Reshape data to long form
  
  africa_population_co2 %>% 
  pivot_longer(c(population_density, co2),
                 names_to = "variable",
                 values_to = "value")),
  
  # Set up plot
  
  aes(
    
    # Set X, Y, and color variables
    
    x = year, 
    y = value, 
    color = country_name)) +
  
  # Setup lines
  
  geom_line() +
  
  # Label axes
  
  labs(title="Population Density and CO2 levels of Africa (1960-2020)",
       x="Year", y = "") +
  
  # Make plot backgroung blank
  
  theme(panel.background = element_rect(fill = "white", color = "grey")) +
  
  # Create facet wrap
  
  facet_wrap(vars(variable), scales = "free")






  



# Notes -------------------------------------------------------------------



### Corrections
# Q7 use the year column
# add division and as.numeric, filter, ggplot labs

