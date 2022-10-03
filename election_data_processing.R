# Load Packages -----------------------------------------------------------
library(tidyverse)
library(plyr)



# Create Cleaning Functions -----------------------------------------------

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

state_pre_prep <-
  function(name) {
    df <- 
      read_csv(paste("data/raw/vde_",name,".csv", sep = "")) %>% 
      fix_cols %>% 
      mutate(county_name = str_to_lower(gsub(' County','', county_city))) %>% 
      inner_join(
        .,
        fips,
        by = "county_name") %>% 
      select(-c(county_city,ward,total_votes_cast,state,county_name))
    return(df)
  }

find_winner <-
  function(df) {
    string_cols <- c("pcode", "winner","total")
    string <- names(colSums(df[, !names(df) %in% string_cols] ))[colSums(df[, !names(df) %in% string_cols] ) == max(colSums(df[, !names(df) %in% string_cols] ))]
    return(string)
  }

prep <- 
  function(frame,year_string) {
    df <- 
      frame %>% 
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
      select(pcode, winner, total, winner_votes, winner_percent) %>% 
      rename_with(~paste0(., year_string), -1)
    return(df)
  }

# Read In Data ------------------------------------------------------------

# Import FIPS and Store

fips <-
  read_csv("data/raw/US_FIPS_Codes.csv", skip=1)  %>% 
  filter(State == "Virginia") %>% 
  unite("fips", `FIPS State`:`FIPS County`, sep="") %>% 
  fix_cols %>% 
  mutate(county_name = str_to_lower(county_name))
write.csv(fips,"data/processed/fips.csv", row.names = FALSE)

# Import the 2016 Presidential election, clean, and store:

prez <- prep(state_pre_prep("prez_2016"),"_2016")
write.csv(prez,"data/processed/va_prez_2016.csv", row.names = FALSE)

# Import the 2017 Gubernatorial election, clean, and store:

gov <- prep(state_pre_prep("gov_2017"),"_2017")
write.csv(gov,"data/processed/va_gov_2017.csv", row.names = FALSE)

# Import the 2018 Senate election, clean, and store:

sen <- prep(state_pre_prep("sen_2018"),"_2018")
write.csv(sen,"data/processed/va_sen_2018.csv", row.names = FALSE)

# Import the 2022 CD-2 election and clean:

cd2_22_pre <- 
  read_csv("data/raw/vde_cd2_2022.csv") %>%
  fix_cols %>% 
  mutate(county_name = gsub('_',' ', county)) %>% 
  mutate(county_name = str_to_lower(gsub(' COUNTY','', county_name))) %>% 
  inner_join(
    .,
    fips,
    by = "county_name") %>% 
  select(-c(county,`_1`,state,precinct_,county_name))

cd2_22 <- prep(cd2_22_pre,"_2022")
write.csv(cd2_22,"data/processed/va_cd2_2022.csv", row.names = FALSE)
rm(cd2_22_pre)