library(tidyverse)
library(sf)
library(plyr)

# Load in relevant functions
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

find_winner <-
  function(df) {
    string_cols <- c("pcode", "winner","total")
    string <- names(colSums(df[, !names(df) %in% string_cols] ))[colSums(df[, !names(df) %in% string_cols] ) == max(colSums(df[, !names(df) %in% string_cols] ))]
    return(string)
  }

prep <- 
  function(path,year_string) {
    df <- 
      read_csv(path) %>% 
      filter(!grepl("Absentee",pct),
             !grepl("Provisional",pct),
             !grepl("Member House of Representatives",pct)) %>% 
      mutate(precinct = remove_pct_prefix(pct),
             pct_num = remove_pct_suffix(pct),
             fips = as.integer(fips),
             pcode = paste(fips, pct_num, sep = "")) %>% 
      select(-c(1,fips,precinct,pct_num,pct)) %>% 
      select(pcode, everything()) %>% 
      group_by(pcode) %>% 
      summarise_each(list(sum)) %>% 
      mutate(winner = colnames(.[, names(.) != "pcode"])[apply(.[, names(.) != "pcode"],1,which.max)],
             total = rowSums(.[, names(.) != "pcode"]))
    win_col <- find_winner(df)
    df["winner_votes"] = df[win_col]
    df <- df %>% 
      mutate(winner_percent = winner_votes / total) %>% 
      rename_with(~paste0(., year_string), -1)
    return(df)
  }

fips <- 
  read_csv("data/processed/fips.csv")

pct <- 
  st_read("data/raw/shapefiles/va_2016/va_2016_president.shp") %>% 
  fix_cols %>% 
  mutate(fips = paste("51",countyfp, sep = ""),
         fips = as.integer(fips),
         pcode = paste(fips, str_sub(vtdst, start= -3), sep = "")) %>% 
  select(pcode,geometry)

prez <- prep("data/processed/2016_prez.csv","_2016")
cd2 <- prep("data/processed/2022_cd2.csv","_2022")

united <- 
  left_join(pct,prez, on = 'pcode') %>% 
  st_transform(crs = 2284)

united_cd <- 
  inner_join(pct,cd2, on = 'pcode') %>% 
  st_transform(crs = 2284)

# Evaluate precincts

# Create an Arlington-only dataset for easy use

arlington <- united %>% 
  filter(grepl("51013",pcode))


# Plot --------------------------------------------------------------------


ar_plot <- arlington %>% 
  ggplot() +
  geom_sf(aes(fill = winner_percent_2016), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Trump Vote Share"))

ar_plot

cd_plot <- united_cd %>% 
  ggplot() +
  geom_sf(aes(fill = winner_percent_2022), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Kiggans Vote Share"))

cd_plot

va_plot <- united %>% 
  ggplot() +
  geom_sf(aes(fill = winner_percent_2016), size = 0.001) +
  scale_fill_continuous(type = "viridis", option = "magma") +
  theme_void() +
  guides(fill=guide_legend(title="Trump Vote Share"))

ggsave('images/va_plot_pct.png', va_plot, bg='transparent')

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

ggsave('images/va_plot_winner.png', va_plot_winner, bg='transparent')




