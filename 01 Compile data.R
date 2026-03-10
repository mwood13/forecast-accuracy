# testing github connection

# combine water scanner data with hurricane data
library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest, lubridate)

# load in hurricane data and create treatment variables ------------------------

# pull in scanner to get fips of interest

scanner <- fread("Data/water_scanner.csv")

scanner <- scanner %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  week_end = as.Date((gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", week_end))),
  fips = str_pad(fips, 5,'left', '0')
)

scanner <- scanner[order(fips, week_end),]

scanner <- subset(scanner, fips_state_code != 51)


length(unique(scanner$fips))
# 894 counties


# load in hurricanes 
hur_df <- fread("Data/Daily_Hurricanes.csv")


hur_df <- hur_df %>% mutate(
  fips = str_pad(fips, 5,'left', '0')
)


# create weekend variable to match with scanner
hur_df <- hur_df %>% group_by(fips) %>% mutate(
  week = ceiling_date(Date, "week")-1,
)

hur_df <- hur_df[order(hur_df$fips, hur_df$week),]


# condense to weekly to merge with scanner. 
weekly_hur_df <- hur_df %>% group_by(fips, week) %>% summarize(
  County = head(County,n=1),
  # Treatment
  Landfall = ifelse(sum(Landfall) > 0, 1, 0),
  Land_MaxWind = max(Land_MaxWind), 
  
  Threat24 = max(Threat24),
  Threat48 = max(Threat48),
  Threat72 = max(Threat72),
  Threat96 = max(Threat96),
  Threat120 = max(Threat120),
  
)

# create dummies for accuracy of forecast.
weekly_hur_df <- weekly_hur_df %>% mutate(
  
  # assignment to a treatment group for the week based on observed forecast
  anyTG = ifelse(Threat24 == 1 | Threat48 == 1 | Threat72 == 1 | Threat96 == 1 | Threat120 == 1, 1,0),
  earlyTG = ifelse(Threat96 == 1 | Threat120 == 1, 1,0),
  midTG = ifelse(Threat72 == 1 , 1, 0),
  lateTG = ifelse(Threat24 == 1 | Threat48 == 1, 1, 0),
  
  # in a treatment group and got hit
  anyTG_Hit = ifelse((Landfall ==1 | dplyr::lead(Landfall, n=1) ==1)& anyTG == 1, 1, 0),
  earlyTG_Hit = ifelse((Landfall ==1 | dplyr::lead(Landfall, n=1) ==1) & earlyTG == 1, 1, 0),
  midTG_Hit = ifelse((Landfall ==1 | dplyr::lead(Landfall, n=1) ==1)& midTG == 1 , 1, 0),
  lateTG_Hit = ifelse((Landfall ==1 | dplyr::lead(Landfall, n=1) ==1) & lateTG == 1 , 1, 0),
  
  
  # in a treatment group and no hit
  anyTG_NoHit = ifelse((Landfall ==0 & dplyr::lead(Landfall, n=1) ==0)& anyTG == 1, 1, 0),
  earlyTG_NoHit = ifelse((Landfall ==0 & dplyr::lead(Landfall, n=1) ==0) & earlyTG == 1, 1, 0),
  midTG_NoHit = ifelse((Landfall ==0 & dplyr::lead(Landfall, n=1) ==0)& midTG == 1 , 1, 0),
  lateTG_NoHit = ifelse((Landfall ==0 & dplyr::lead(Landfall, n=1) ==0) & lateTG == 1 , 1, 0),
  
  # got hit without being in treatment group
  Hit_noTG = ifelse(Landfall == 1 & (anyTG == 0 & dplyr::lag(anyTG, n=1) ==0), 1, 0),
  Hit_noEarlyTG = ifelse(Landfall == 1 & (earlyTG == 0 & dplyr::lag(earlyTG, n=1) ==0), 1, 0),
  Hit_nomidTG = ifelse(Landfall == 1 & (midTG == 0 & dplyr::lag(midTG, n=1) ==0), 1, 0),
  Hit_nolateTG = ifelse(Landfall == 1 & (lateTG == 0 & dplyr::lag(lateTG, n=1) ==0), 1, 0),
)


# OBSERVATIONS OF EACH TREATMENT TYPE
sum(weekly_hur_df$anyTG_Hit) # 2191
sum(weekly_hur_df$earlyTG_Hit) # 1529
sum(weekly_hur_df$midTG_Hit) # 1702
sum(weekly_hur_df$lateTG_Hit) # 1899

sum(weekly_hur_df$anyTG_NoHit) # 29828
sum(weekly_hur_df$earlyTG_NoHit) # 24890
sum(weekly_hur_df$midTG_NoHit) # 14351
sum(weekly_hur_df$lateTG_NoHit) # 10858

sum(weekly_hur_df$Hit_noTG) # 55
sum(weekly_hur_df$Hit_noEarlyTG) # 508
sum(weekly_hur_df$Hit_nomidTG) # 368
sum(weekly_hur_df$Hit_nolateTG) # 71

rm(hur_df)
gc()


scanner <- scanner %>% mutate(
  week_end = as.Date(week_end)
)

weekly_hur_df <- weekly_hur_df %>% mutate(
  fips =  str_pad(fips, 5,'left', '0')
)

scanner <- left_join(scanner, weekly_hur_df, by = c("fips", "week_end" = "week"))

# pull in population and heat ---------------------------------------------------


#population
pop_df <- read.csv("Data/County_pop.csv")

pop_df <- pop_df %>% mutate(
  state = str_pad(state, 2,'left', '0'),
  county = str_pad(county, 3,'left', '0'),
  fips = str_c(state, county, sep = ""),
  fips = ifelse(fips == 12025, 12086, fips),
  fips = str_pad(fips, 5,'left', '0')
)

pop_df <- pop_df[,c(8,37)]

scanner <- scanner %>% 
mutate(
  fips = str_pad(fips, 5,'left', '0'),
  week_end = as.Date(week_end)
)
scanner <- left_join(scanner , pop_df, by = "fips")

scanner <- scanner %>% mutate(
  total_rev_per_cap = total_rev/CENSUS2010POP * 100000
)



# weather

weather <- read.csv('Data\\weekly_weather.csv')

weather <- weather %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  week_end = as.Date(week_end),
  fips = str_pad(fips, 5,'left', '0')
)

weather<- weather[,c(1,2,6)]


scanner <- left_join(scanner, weather, by =c("fips", "week_end"))





fwrite(scanner, "Data\\scanner_hur.csv", row.names = F)

