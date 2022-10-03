library(tidyverse)
library(sf)
library(plyr)

fix_cols <- 
  function(df) {
    names(df) <- tolower(names(df))
    names(df) <- gsub('[[:punct:] ]+',' ',names(df))
    names(df) <- gsub(" ", "_", names(df))
    names(df) <- gsub("_republican", "", names(df))
    return(df)
  }
remove_pct_prefix <- 
  function(string) {
    string <- sub("^(\\S*\\s+\\S+) ", "", string)
    return(string)
  }
remove_pct_suffix <-
  function(string) {
    x <- str_sub(string, end = 4) %>% 
      gsub("[^0-9]", "", .) %>% 
      paste("0000", ., sep = "0") %>% 
      str_sub(., start= -6)
    return(x)
  }

fips <- 
  read_csv("data/processed/fips.csv")

pct <- 
  st_read("data/raw/shapefiles/va_2016/va_2016_president.shp") %>% 
  fix_cols %>% 
  mutate(fips = paste("51",countyfp, sep = ""),
         fips = as.integer(fips),
         pcode = paste(fips, vtdst, sep = "")) %>% 
  select(pcode,geometry)

prez <- 
  read_csv("data/processed/2016_prez.csv") %>% 
  filter(!grepl("Absentee",pct),
         !grepl("Provisional",pct)) %>% 
  mutate(precinct = remove_pct_prefix(pct),
         pct_num = remove_pct_suffix(pct),
         fips = as.integer(fips),
         pcode = paste(fips, pct_num, sep = "")) %>% 
  select(-c(1,fips,county_name,precinct,pct_num)) %>% 
  select(pcode, pct, everything()) %>% 
  mutate(winner = colnames(.[(3:16)])[apply(.[(3:16)],1,which.max)],
         total = rowSums(.[(3:16)]),
         trump_pct = donald_j_trump / total) 

united <- 
  inner_join(pct,prez, on = 'pcode') %>% 
  st_transform(crs = 2284)

# Evaluate precincts

# Create an Arlington-only dataset for easy use

arlington <- united %>% 
  filter(grepl("51013",pcode))


# Plot --------------------------------------------------------------------



ar_plot <- arlington %>% 
  ggplot() +
  geom_sf(aes(fill = trump_pct), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Trump Vote Share"))

ar_plot

va_plot <- united %>% 
  ggplot() +
  geom_sf(aes(fill = trump_pct), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Trump Vote Share"))

ggsave('va_plot_pct.png', va_plot, bg='transparent')

va_plot_winner <- united %>%
  mutate(Winner = mapvalues(winner, 
                            from=c("donald_j_trump", "marco_rubio", 
                                   "rafael_edward_cruz","benjamin_s_carson_sr"), 
                            to=c("Donald Trump", "Marco Rubio", 
                                 "Other","Other"))) %>% 
  ggplot() +
  geom_sf(aes(fill = Winner), size = 0.001) +
  scale_fill_viridis_d(option = "magma", direction = -1) +
  theme_void()
  
va_plot_winner

ggsave('va_plot_winner.png', va_plot_winner, bg='transparent')


# Scrap -------------------------------------------------------------------

# Display a list of redundant pcodes and investigate

dup_pcodes <- table(united$pcode) %>% data.frame %>% filter(Freq != 1)

examine_prez <- prez %>% filter(pcode %in% dup_pcodes$Var1) %>% select(pct)

pct %>% filter(pcode %in% dup_pcodes$Var1) %>% select(pcode)

