# build out last event info. 

library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest)

df <- fread("Data//scanner_hur.csv")

df <- df[order(fips, week_end),]

# get dates that there were any threats or landfalls
hur_events <- df %>% subset(
  Landfall == 1 | anyTG==1 #need to get if expected and no one hit
)

length(unique(hur_events$week_end))
#  96 landfalls, expected landfalls or threat dates


# remove excess columns
hur_events <- hur_events[,c(1,2,20:31)]

# for each date in the list when a county is hit the county gets a category. ----

hist_df <- df[,c(1,2)]%>%as.data.frame()
 
# set up dataframe to store info
hist_df <- hist_df %>% mutate(
  last_date = as.Date(NA),
  last_anyTG_Hit = as.integer(NA),
  last_earlyTG_Hit =as.integer(NA), 
  last_midTG_Hit = as.integer(NA),
  last_lateTG_Hit = as.integer(NA),
  last_anyTG_NoHit = as.integer(NA),
  last_earlyTG_NoHit = as.integer(NA),
  last_midTG_NoHit = as.integer(NA),
  last_lateTG_NoHit = as.integer(NA),
  last_Hit_noTG = as.integer(NA),
  last_Hit_noEarlyTG = as.integer(NA),
  last_Hit_noMidTG = as.integer(NA),
  last_Hit_noLateTG = as.integer(NA),
  miss_streak = as.integer(NA),
  hit_streak = as.integer(NA),
  acc_streak_cont = as.integer(NA)
)

N = length(df$fips)
old_county = 1


for(n in 1:N){
  new_county = hist_df$fips[n]
  new_date = hist_df$week_end[n]
  
  # if new county then start over
  if(new_county != old_county){
    old_county = new_county
    info_vec = c(hist_df[1,c(3:15)])
    miss_streak = NA
    hit_streak = NA
    acc_streak = NA
    print(old_county)
    print(n)
    next
  }
  
  
  # fill in current row last weeks information 
  hist_df[n,c(3:15)] = info_vec
  hist_df$miss_streak[n] = miss_streak
  hist_df$hit_streak[n] = hit_streak
  hist_df$acc_streak_cont[n] = acc_streak
  
  
  # if the county and date are in the hurricane list, see if the info needs to be updated
  if(new_county %in% hur_events$fips && new_date %in% hur_events$week_end){
    for_df <- subset(hur_events, fips == new_county & week_end == new_date)
    T = length(for_df$fips)
    
    if(T>0){
      info_vec = c(for_df[1,c(2:14)])
      
      if(for_df$anyTG_NoHit == 1){
        miss_streak = ifelse(is.na(miss_streak), 1, miss_streak + 1)
        acc_streak = ifelse(is.na(acc_streak), -1, acc_streak - 1)
        hit_streak = 0
      }
      
      if(for_df$anyTG_Hit){
        hit_streak = ifelse(is.na(hit_streak), 1, hit_streak + 1)
        acc_streak = ifelse(is.na(acc_streak), 1, acc_streak + 1)
        miss_streak = 0
      }
      
    }
  }
  
  
}


# merge historical data with current data. 

df <- left_join(df, hist_df, by = c("fips", "week_end" ))%>%as.data.frame()


fwrite(df, "Data/scanner_with_past.csv", row.names = F)

