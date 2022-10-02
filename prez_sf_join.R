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
remove_pct_prefix <- 
  function(string) {
    string <- sub("^(\\S*\\s+\\S+) ", "", string)
    return(string)
  }
remove_pct_suffix <-
  function(string) {
    x <- str_sub(string, end = 4) %>% 
      gsub("[^0-9]", "", .) %>% 
      paste("0", ., sep = "0") %>% 
      str_sub(., start= -3)
    return(x)
  }

fips <- 
  read_csv("data/processed/fips.csv")

pct <- 
  st_read("data/raw/shapefiles/va_2016/va_2016_president.shp") %>% 
  fix_cols %>% 
  mutate(fips = paste("51",countyfp, sep = ""),
         pct_num = str_sub(vtdst, start= -3),
         fips = as.integer(fips),
         pcode = paste(fips, pct_num, sep = "")) %>% 
  select(pcode,geometry)

prez <- 
  read_csv("data/processed/2016_prez.csv") %>% 
  filter(!grepl("Absentee",pct),
         !grepl("Provisional",pct)) %>% 
  mutate(precinct = remove_pct_prefix(pct),
         pct_num = remove_pct_suffix(pct),
         fips = as.integer(fips),
         pcode = paste(fips, pct_num, sep = ""),
         .keep = "unused") %>% 
  select(-c(1,fips,county_name,precinct,pct_num)) %>% 
  select(pcode, everything()) %>% 
  mutate(total = rowSums(.[(2:15)]),
         trump_pct = donald_j_trump / total,
         winner = colnames(.[(2:15)])[apply(.[(2:15)],1,which.max)])

united <- 
  inner_join(pct,prez, on = 'pcode') %>% 
  st_transform(crs = 2284)

arlington <- united %>% 
  filter(grepl("51013",pcode))

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

ggsave('myplot.png', va_plot, bg='transparent')



