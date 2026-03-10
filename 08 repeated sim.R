# simulating average log sales under improved forecasts. 

library(pacman)
p_load(tidyverse, data.table, fastDummies,survival, stats, fixest, broom,
       stringr, viridis, reshape2, jtools, sf, tmap,paletteer,
       lubridate,  'tigris')

# pull in data and get regreession results ------------------------------------
main_df <- fread("Data//scanner_with_past.csv")%>%as.data.frame()

main_df <- main_df %>% mutate(
  week_end = as.Date(week_end),
  month =  month(ymd(main_df$week_end)),
  year = year(ymd(main_df$week_end)),
  fips = str_pad(fips, 5,'left', '0')
)


# build out regression variables
main_df <- main_df %>% mutate(
  
  acc_streak = ifelse(hit_streak > 0, hit_streak, -1*miss_streak),
  
  anyTG_acc = anyTG*acc_streak,
  land_acc = Landfall*acc_streak,
  anyTG_acc_sq = anyTG*acc_streak*acc_streak,
  land_acc_sq = Landfall*acc_streak*acc_streak,
)


reg <- feols(data = main_df, 
            total_rev ~ anyTG + Landfall+
               anyTG_acc + land_acc + 
               anyTG_acc_sq + land_acc_sq + 
               temp_mean |
               fips + month + year, 
             cluster = c("fips"),
            fixef.keep_names = TRUE
)



# get dates of inaccurate forecasts
events <-unique(subset(main_df, anyTG == 1 | Landfall == 1)$week_end)

events <- sort(events)

miss_df <- as.data.frame(events)

set.seed(10171996)



# testing regression 

predict_df <-  main_df[,c(1,3,9,16,34,51:56)]


main_df <- main_df %>% mutate(
  fit_val = predict(reg, predict_df)
) 


# make functions to use for simulating better forecasts -------------------------

# Step 1. Generate a df of improved forecasting. 
make_sim_df <- function(date_df, full_df){
  accuracy_df <- date_df %>% mutate(
    better25 = rbinom(length(date_df[,1]), 1, 0.25),
    better50= rbinom(length(date_df[,1]), 1, 0.50),
    better05 = rbinom(length(date_df[,1]), 1, 0.05),
    better10 = rbinom(length(date_df[,1]), 1, 0.1),
  )
  
  df <- left_join(full_df, accuracy_df, by = c("week_end" = "events"))
  
  # get dates that there were any threats or landfalls
  sim_df <- df %>% subset(
    Landfall == 1 | anyTG==1 #need to get if expected and no one hit
  )
  
  
  # remove excess columns
  sim_df <- sim_df[,c(1,2,9,20,24,58:61)]
  
  
  sim_df <- sim_df %>% mutate(
    
    new_TG_25 = ifelse(better25 == 1, Landfall, anyTG_Hit + anyTG_NoHit),
    new_TG_50 = ifelse(better50 == 1, Landfall, anyTG_Hit + anyTG_NoHit),
    new_TG_05 = ifelse(better05 == 1, Landfall, anyTG_Hit + anyTG_NoHit),
    new_TG_10 = ifelse(better10 == 1, Landfall, anyTG_Hit + anyTG_NoHit),
    
    acc_streak_25 = 0,
    acc_streak_50 = 0,
    acc_streak_05 = 0,
    acc_streak_10 = 0
  )
  
  return(sim_df)
}

# Build new streaks based on improved accuracy

make_new_streaks <- function(df){
  N = length(df[,1])
  old_county =df[10000,1]
  
  for(n in 1:N){
    new_county = df[n,1]
    new_date = df$week_end[n]
    
    # if new county then start over
    if(new_county != old_county){
      old_county = new_county
      accuracy25 = 0
      accuracy50 = 0
      accuracy05 = 0
      accuracy10 = 0
      #print(old_county)
      #print(n)
    }
    
    
    # fill in current streaks
    df$acc_streak_25[n] = accuracy25
    df$acc_streak_50[n] = accuracy50
    df$acc_streak_05[n] = accuracy05
    df$acc_streak_10[n] = accuracy10
    
    
    
    
    # if the county and date are in the hurricane list, see if the info needs to be updated
    if(new_date %in% events){
      for_df <- subset(df, fips == new_county & week_end == new_date)
      T = length(for_df$fips)
      
      if(T>0){
        
        # accurate forecast  and landfall add 1
        if(for_df$new_TG_25[1] == 1 & for_df$Landfall[1] ==1){
          accuracy25 = ifelse(
            accuracy25 < 0 , 1, accuracy25 + 1
          )
        }
        
        # if a forecast with a miss minus 1
        if(for_df$new_TG_25[1] == 1 & for_df$Landfall == 0){
          accuracy25 = ifelse(
            accuracy25 > 0 , -1, accuracy25 - 1
          )
        }
        
        
        # if both are zero do nothing because there is no forecast or landfall
        
        if(for_df$new_TG_50[1] == 1 & for_df$Landfall[1] ==1){
          accuracy50 = ifelse(
            accuracy50 < 0 , 1, accuracy50 + 1
          )
        }
        
        # if a forecast with a miss minus 1
        if(for_df$new_TG_50[1] == 1 & for_df$Landfall == 0){
          accuracy50 = ifelse(
            accuracy50 > 0 , -1, accuracy50 - 1
          )
        }
        
        if(for_df$new_TG_05[1] == 1 & for_df$Landfall[1] ==1){
          accuracy05 = ifelse(
            accuracy05 < 0 , 1, accuracy05 + 1
          )
        }
        
        # if a forecast with a miss minus 1
        if(for_df$new_TG_05[1] == 1 & for_df$Landfall == 0){
          accuracy05 = ifelse(
            accuracy05 > 0 , -1, accuracy05 - 1
          )
        }
        
        
        # i10%
        
        if(for_df$new_TG_10[1] == 1 & for_df$Landfall[1] ==1){
          accuracy10 = ifelse(
            accuracy10 < 0 , 1, accuracy10 + 1
          )
        }
        
        # if a forecast with a miss minus 1
        if(for_df$new_TG_10[1] == 1 & for_df$Landfall == 0){
          accuracy10 = ifelse(
            accuracy10 > 0 , -1, accuracy10 - 1
          )
        }
        
        
      }
      
    }
    
    
  }
  
  return(df)
}


# Calculate new log sales based on simulated streaks. 

calculate_sales <- function(df, sim_df, reg_results){
 
   sim_df <- sim_df %>% mutate(
    fips = str_pad(fips, 5,'left', '0'),
    week_end = as.Date(week_end)
    
  )
  
  sim_df <- sim_df[,c(1,2,10:17)]
  
  # join data to calculate
  comb_df <- merge(df, sim_df , by = c("fips", "week_end"))
  
  # get predict data frames
  imp_25_df <- comb_df[,c(1,2,3,9,34,51, 58,62)]
  imp_50_df <- comb_df[,c(1,2,3,9,34,51, 59,63)]
  imp_05_df <- comb_df[,c(1,2,3,9,34,51, 60,64)]
  imp_10_df <- comb_df[,c(1,2,3,9,34,51, 61,65)]
  
  # 10% better forecasts
  imp_25_df <- rename(imp_25_df, c("anyTG" = "new_TG_25", "acc_streak" = "acc_streak_25"))
  
  imp_25_df <- imp_25_df %>% mutate(
    anyTG_acc = anyTG*acc_streak,
    land_acc = Landfall*acc_streak,
    anyTG_acc_sq = anyTG*acc_streak*acc_streak,
    land_acc_sq = Landfall*acc_streak*acc_streak,
  )
  
  
  comb_df$fit_val_25 <- predict(reg, imp_25_df)
  
  # 25% better forecasting
  
  imp_50_df <- rename(imp_50_df, c("anyTG" = "new_TG_50", "acc_streak" = "acc_streak_50"))
  
  imp_50_df <- imp_50_df %>% mutate(
    anyTG_acc = anyTG*acc_streak,
    land_acc = Landfall*acc_streak,
    anyTG_acc_sq = anyTG*acc_streak*acc_streak,
    land_acc_sq = Landfall*acc_streak*acc_streak,
  )
  
  comb_df$fit_val_50 <- predict(reg, imp_50_df)
  
  
  # 50% better forecasting
  
  imp_05_df <- rename(imp_05_df, c("anyTG" = "new_TG_05", "acc_streak" = "acc_streak_05"))
  
  imp_05_df <- imp_05_df %>% mutate(
    anyTG_acc = anyTG*acc_streak,
    land_acc = Landfall*acc_streak,
    anyTG_acc_sq = anyTG*acc_streak*acc_streak,
    land_acc_sq = Landfall*acc_streak*acc_streak,
  )
  
  comb_df$fit_val_05 <- predict(reg, imp_05_df)
  
  # 75% accurate forecasting
  imp_10_df <- rename(imp_10_df, c("anyTG" = "new_TG_10", "acc_streak" = "acc_streak_10"))
  
  imp_10_df <- imp_10_df %>% mutate(
    anyTG_acc = anyTG*acc_streak,
    land_acc = Landfall*acc_streak,
    anyTG_acc_sq = anyTG*acc_streak*acc_streak,
    land_acc_sq = Landfall*acc_streak*acc_streak,
  )
  
  
  comb_df$fit_val_10 <- predict(reg, imp_10_df)
  
  
  return(comb_df)
}



# filter data and save out average by county
find_hur_sales <- function(df){
  
  #threat_sales <- subset(df, any_TG == 1)
  #landfall_ales <- subset(df, Landfall == 1)
  combined_sales <- subset(df, Landfall == 1 | anyTG == 1)
  
  # only save mean for last half
  combined_sales <- subset(combined_sales, year >=2013)
  
  county_threat_df <- combined_sales %>% group_by(fips) %>% summarize(
    mean_real = mean(total_rev),
    mean_fit = mean(fit_val, na.rm=T),
    mean25 = mean(fit_val_25),
    mean50 = mean(fit_val_50),
    mean05 = mean(fit_val_05),
    mean10 = mean(fit_val_10)
  )
  
  return(county_threat_df)
}


simulate_data <- function(scanner, misses, reg){
  test_sim_df <- make_sim_df(misses, scanner)
  test_sim_df <- make_new_streaks(test_sim_df)
  
  new_sales_df <- calculate_sales(scanner, test_sim_df, reg)
  
  county_sim <- find_hur_sales(new_sales_df)
  
  return(county_sim)
}



# test df.
#test_sim_df <- make_sim_df(miss_df, main_df)
#test_sim_df <- make_new_streaks(test_sim_df)

#new_sales_df <- calculate_sales(main_df, test_sim_df, reg)
#county_sim <- find_hur_sales(new_sales_df)



# Run simulation function 10,000 times -----------------------------------------

library(doParallel)
cl<- makeCluster(8)
registerDoParallel(cl)

results <- tibble()

results <- foreach (i = 1:1000 ,
                    .combine = rbind,
                    .packages="tidyverse") %dopar%{
   sim_df <- simulate_data(main_df, miss_df, reg)
   sim_df <- sim_df%>%mutate(iter = i)

                    }

stopCluster(cl)

#sav simulation results
write.csv(results, "Data/repeated_sim.csv", row.names = F)


# condense to average result over all simulations

mean_res_df <- results %>% 
  group_by(fips) %>% 
  summarize( 
    mean_real = mean(mean_real),
    mean_fit = mean(mean_fit),
    mean25 = mean(mean25),
    mean50 = mean(mean50),
    mean05 = mean(mean05),
    mean10 = mean(mean10),
    
  )


# map out results of true and simulated sales ----------------------------------

rm(cl, main_df, miss_df, reg, results)
gc()


us_counties <- tigris::counties(state = c('01', '12', '13',
                                          '22', '28','37', '45',
                                          '48'))

us_counties <- us_counties %>% mutate(
  fips = paste(STATEFP, COUNTYFP, sep= ""),
  fips = str_pad(fips, 5,'left', '0')
)

landfall_counties <- subset(main_df, Landfall == 1)
image_df <- us_counties %>% filter(fips %in% landfall_counties$fips)


image_df <- left_join(image_df, mean_res_df , by = "fips")

pop <- unique(main_df[,c("fips", "CENSUS2010POP")])

image_df <- left_join(image_df, pop, by = "fips")

image_df <- image_df %>% mutate(
  fit_per_cap = mean_fit/CENSUS2010POP*100000
)

# map of actual average sales during landfall/threat
tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "fit_per_cap", 
              fill.scale = tm_scale_intervals(
                #style = c("quantile"),
                values = "mako",
                label.style = "continuous",
                breaks = c(0,500, 1000, 2000, 3000, 10000)
                ),
              fill.legend = tm_legend(
                title = "Sales per 100K ",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
               position = tm_pos_out("right")
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("Predicted Sales During a Hurricane", size = 4)


# change in sales from a 1% increase in forecasting. 

image_df <- image_df %>% mutate(
  percent25 = ((mean25 - mean_fit)/mean_fit)*100,
  percent50 = ((mean50 - mean_fit)/mean_fit)*100,
  percent05 = ((mean05 - mean_fit)/mean_fit)*100,
  percent10 = ((mean10 - mean_fit)/mean_fit)*100,
)



# 5 % improvement
tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "percent05", 
              fill.scale = tm_scale_intervals(
                #style = c("quantile"),
                values = "brewer.rd_bu",
                midpoint = 0,
                label.style = "continuous",
                breaks = c(-13, -6, -3, 0, 3, 6, 13)
              ),
              fill.legend = tm_legend(
                title = "% Change in Sales",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("5% Better Forecasting", size = 4)



# 10% better
tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "percent10", 
              fill.scale = tm_scale_intervals(
               # style = c("quantile"),
                values = "brewer.rd_bu",
                midpoint = 0,
                label.style = "continuous",
                breaks = c(-13, -6, -3, 0, 3, 6, 13)
              ),
              fill.legend = tm_legend(
                title = "% Change in Sales",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("10% Better Forecasting", size = 4)


# 25% better 

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "percent25", 
              fill.scale = tm_scale_intervals(
                #style = c("quantile"),
                values = "brewer.rd_bu",
                midpoint = 0,
                label.style = "continuous",
                breaks = c( -20,  -15, -10, -5, 0, 5, 10)
              ),
              fill.legend = tm_legend(
                title = "% Change in Sales",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("25% Better Forecasting", size = 4)


# 2.5 % improvement

tm_shape(us_counties)+
  tm_polygons(fill = "white")+
  tm_shape(image_df)+
  tm_polygons(fill = "percent50", 
              fill.scale = tm_scale_intervals(
                #style = c("quantile"),
                values = "brewer.rd_bu",
                midpoint = 0,
                label.style = "continuous",
                breaks = c(-30, -25, -20,  -15, -10, -5, 0, 5)
              ),
              fill.legend = tm_legend(
                title = "% Change in Sales",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              ))+
  tm_layout(frame=FALSE,
            component.autoscale = FALSE)+
  tm_title("50% Better Forecasting", size = 4)

