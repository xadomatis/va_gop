
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(sf)

fix_cols <- 
  function(df) {
    names(df) <- tolower(names(df))
    names(df) <- gsub('[[:punct:] ]+',' ',names(df))
    names(df) <- gsub(" ", "_", names(df))
    names(df) <- gsub("_republican", "", names(df))
    return(df)
  }

# Load Data ---------------------------------------------------------------

fips <- 
  read_csv("data/processed/fips.csv")

pct <- 
  st_read("data/raw/shapefiles/va_2016/va_2016_president.shp") %>% 
  fix_cols %>% 
  mutate(fips = paste("51",countyfp, sep = ""),
         fips = as.integer(fips),
         pcode = paste(fips, str_sub(vtdst, start= -3), sep = "")) %>% 
  select(pcode,geometry)

prez <- 
  read_csv("data/processed/va_prez_2016.csv") %>% mutate(pcode = as.character(pcode))

sen <- 
  read_csv("data/processed/va_sen_2018.csv") %>% mutate(pcode = as.character(pcode))

gov <- 
  read_csv("data/processed/va_gov_2017.csv") %>% mutate(pcode = as.character(pcode))

cd2_22 <- 
  read_csv("data/processed/va_cd2_2022.csv") %>% mutate(pcode = as.character(pcode))

# Merge Data --------------------------------------------------------------

united_prez <- 
  left_join(pct,prez, on = 'pcode') %>% 
  st_transform(crs = 2284)

united_cd <- 
  inner_join(pct,cd2_22, on = 'pcode') %>% 
  st_transform(crs = 2284)

# Plot --------------------------------------------------------------------

cd_plot <- united_cd %>% 
  ggplot() +
  geom_sf(aes(fill = winner_percent_2022), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Kiggans Vote Share"))

cd_plot

ggsave('images/va_plot_pct_cd2_2022.png', va_plot, bg='transparent')

va_plot <- united_prez %>% 
  ggplot() +
  geom_sf(aes(fill = winner_percent_2016), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Trump Vote Share"))

ggsave('images/va_plot_pct_prez_2016.png', va_plot, bg='transparent')

va_plot_winner <- united %>%
  mutate(Winner = mapvalues(winner_2016, 
                            from=c("donald_j_trump", "marco_rubio", 
                                   "rafael_edward_cruz","benjamin_s_carson_sr"), 
                            to=c("Donald Trump", "Marco Rubio", 
                                 "Other","Other"))) %>% 
  ggplot() +
  geom_sf(aes(fill = Winner), size = 0.001) +
  scale_fill_viridis_d(option = "magma", direction = -1) +
  theme_void()

ggsave('images/va_plot_winner_prez_2016.png', va_plot_winner, bg='transparent')